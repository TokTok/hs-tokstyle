{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.Memzero (descr) where

import           Control.Monad                 (forM, forM_)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.C.Analysis.SemRep    (FunDef (..), GlobalDecls (..),
                                                IdentDecl (..))
import           Language.C.Analysis.TravMonad (Trav, getUserState)
import           Language.C.Data.Ident         (identToString)
import           Language.C.Data.Node          (NodeInfo, posOfNode)
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants   (getCInteger)
import           Language.Cimple.Diagnostics   (DiagnosticSpan (..))
import           Prettyprinter                 (pretty, (<+>))
import           Tokstyle.Analysis.Dataflow    (Dataflow (..), solve)
import           Tokstyle.C.Analysis.CFG       (CFG, Node (..), NodeKind (..),
                                                fromFunDef)
import           Tokstyle.C.Analysis.Interval  (Program (..))
import           Tokstyle.C.Analysis.Syntax    (initExprs, statExprs, stats)
import           Tokstyle.C.Analysis.Wipes     (declItems, keepContents,
                                                localDeclarators,
                                                producedLocals, subExprs,
                                                wipeTargets, wiped, wipers,
                                                writableParams)
import           Tokstyle.C.Env                (DiagnosticLevel (..), Env,
                                                posAndLen, program,
                                                recordRichError)
import           Tokstyle.C.TravUtils          (backticks)


data Effect
    = Declare String
    | Write String
    | Wipe String NodeInfo

-- | Locals that may hold data not wiped since it was written.
type Unwiped = Set String

-- | The local whose contents an assignment to this expression changes.
assigned :: CExpr -> Maybe String
assigned = \case
    CVar i _          -> Just (identToString i)
    CIndex e _ _      -> assigned e
    CMember e _ _ _   -> assigned e
    CUnary CIndOp e _ -> assigned e
    _                 -> Nothing

-- | The names an expression mentions, outside @sizeof@.
mentions :: CExpr -> [String]
mentions = \case
    CVar i _      -> [identToString i]
    CSizeofExpr{} -> []
    e             -> concatMap mentions (subExprs e)

-- | Locals whose address an expression keeps, beyond passing it to a call.
escapes :: CExpr -> [String]
escapes = \case
    CVar i _                    -> [identToString i]
    CSizeofExpr{}               -> []
    CCall f _ _                 -> escapes f
    CIndex b i _                -> base b ++ escapes i
    CMember b _ _ _             -> base b
    CAssign _ l r _             -> base l ++ escapes r
    CUnary CIndOp e _           -> base e
    CUnary op e _ | isIncDec op -> base e
    e                           -> concatMap escapes (subExprs e)
  where
    base CVar{} = []
    base e      = escapes e

isIncDec :: CUnaryOp -> Bool
isIncDec op = op `elem` [CPreIncOp, CPreDecOp, CPostIncOp, CPostDecOp]

exprEffects :: CExpr -> Trav Env [Effect]
exprEffects = \case
    CCall (CVar f _) (target : rest) ni | identToString f `elem` wipers -> do
        effs <- concat <$> mapM exprEffects rest
        return $ effs ++ maybe [] (\v -> [Wipe v ni]) (wiped target)
    CCall (CVar f _) args _ | identToString f `elem` keepContents ->
        concat <$> mapM exprEffects args
    CCall f args _ -> do
        writable <- writableParams f
        effs <- forM (zip [0 ..] args) $ \(n, arg) -> do
            inner <- exprEffects arg
            return $ inner ++ if writable n then map Write (mentions arg) else []
        return $ concat effs
    CAssign _ l r _ -> do
        effs <- concat <$> mapM exprEffects [r, l]
        return $ effs ++ maybe [] (pure . Write) (assigned l)
    CUnary op e _ | isIncDec op -> do
        effs <- exprEffects e
        return $ effs ++ maybe [] (pure . Write) (assigned e)
    CSizeofExpr{} -> return []
    e -> concat <$> mapM exprEffects (subExprs e)

isZero :: CInit -> Bool
isZero = \case
    CInitExpr (CConst (CIntConst i _)) _ -> getCInteger i == 0
    CInitList l _                        -> all (isZero . snd) l
    _                                    -> False

-- | The expressions a node evaluates, or 'Nothing' if the graph doesn't model
-- the statement it stands for.
nodeExprs :: Node -> Maybe [CExpr]
nodeExprs (Node _ kind) = case kind of
    StatNode (CExpr e _)   -> Just (maybe [] pure e)
    StatNode (CReturn e _) -> Just (maybe [] pure e)
    StatNode CBreak{}      -> Just []
    StatNode CCont{}       -> Just []
    StatNode CGoto{}       -> Just []
    StatNode CFor{}        -> Just []
    StatNode _             -> Nothing
    ExprNode e             -> Just [e]
    BranchNode e           -> Just [e]
    DeclNode d             -> Just (concat [ maybe [] initExprs ini | (_, _, ini) <- declItems d ])
    EntryNode              -> Just []
    ExitNode               -> Just []

nodeEffects :: Node -> Trav Env [Effect]
nodeEffects node@(Node _ kind) = case kind of
    DeclNode d -> concat <$> forM (declItems d) (\(i, _, ini) -> do
        effs <- concat <$> mapM exprEffects (maybe [] initExprs ini)
        let written = [ Write (identToString i) | Just v <- [ini], not (isZero v) ]
        return $ effs ++ Declare (identToString i) : written)
    _ -> concat <$> mapM exprEffects (fromMaybe [] (nodeExprs node))

apply :: Unwiped -> Effect -> Unwiped
apply s = \case
    Declare v -> Set.delete v s
    Write v   -> Set.insert v s
    Wipe v _  -> Set.delete v s

isWrite :: Effect -> Bool
isWrite = \case
    Write{} -> True
    _       -> False

-- | Whether no path from this node looks at the local again. A call in a
-- branch condition that fails often takes such a path, and need not have
-- written the local.
untouchedFrom :: CFG -> String -> Node -> Bool
untouchedFrom cfg v start = all untouched (Set.toList (reachable Set.empty [start]))
  where
    reachable seen [] = seen
    reachable seen (n : ns)
        | n `Set.member` seen = reachable seen ns
        | otherwise = reachable (Set.insert n seen) (map snd (Map.findWithDefault [] n cfg) ++ ns)
    untouched n = maybe False (notElem v . concatMap mentions) (nodeExprs n)

-- | Locals wiped where the graph doesn't follow the paths: in a @switch@,
-- which it takes as one statement, and in a @do@ loop, which it takes as one
-- that may not run.
unmodelledWipes :: CStat -> [String]
unmodelledWipes body = concatMap own (stats body)
  where
    own = \case
        CSwitch _ t _     -> concatMap wipeTargets (statExprs t)
        CWhile _ t True _ -> concatMap wipeTargets (statExprs t)
        _                 -> []

checkFunction :: FunDef -> Trav Env ()
checkFunction fd@(FunDef _ body _) = do
    let (entry, cfg) = fromFunDef fd
        nodes = Set.toList . Set.fromList $ Map.keys cfg ++ [ n | es <- Map.elems cfg, (_, n) <- es ]
    effects <- Map.fromList <$> forM nodes (\n -> (,) n <$> nodeEffects n)
    secrets <- progSecrets . program <$> getUserState
    produced <- producedLocals body
    let decls = localDeclarators body
        counts = Map.fromListWith (+) [ (identToString i, 1 :: Int) | (i, _) <- decls ]
        isObject (CDeclr _ derived _ _ _) = not (any isPtrOrFun derived)
        isPtrOrFun = \case
            CPtrDeclr{} -> True
            CFunDeclr{} -> True
            _           -> False
        objects = Set.fromList [ identToString i | (i, d) <- decls, isObject d, counts Map.! identToString i == 1 ]
        wipes = Map.fromListWith (\a b -> if posOfNode a < posOfNode b then a else b)
            [ (v, ni) | effs <- Map.elems effects, Wipe v ni <- effs ]
        excluded = Set.fromList $ unmodelledWipes body ++ concatMap escapes (statExprs body)
        -- Locals that a call fills with a secret.
        filled = Map.fromListWith const [ (v, (f, ni)) | (arg@(f, _), v, ni) <- produced, arg `Set.member` secrets ]
        fillings v = length [ () | (_, u, _) <- produced, u == v ]
        readAfter v = length (filter (== v) (concatMap mentions (statExprs body))) > fillings v
        secretLocals = Map.keysSet (Map.filterWithKey (\v _ -> readAfter v) filled)
        candidates = (objects `Set.intersection` (Map.keysSet wipes `Set.union` secretLocals)) `Set.difference` excluded
        effectsOf n = Map.findWithDefault [] n effects
        transferNode n s = case n of
            Node _ BranchNode{} -> foldl apply s (filter (not . isWrite) (effectsOf n))
            _                   -> foldl apply s (effectsOf n)
        edgeTransferNode n e s = case n of
            Node _ BranchNode{} ->
                let target = lookup e (Map.findWithDefault [] n cfg)
                    written v = maybe True (not . untouchedFrom cfg v) target
                in foldl apply s [ w | w@(Write v) <- effectsOf n, written v ]
            _ -> s
        problem = Dataflow
            { transfer     = transferNode
            , edgeTransfer = edgeTransferNode
            , merge        = Set.union
            , initial      = Set.empty
            }
        result = solve entry cfg problem
    forM_ (Map.toList result) $ \(n, s) -> do
        let unwiped = Set.unions
                [ edgeTransferNode n e (transferNode n s)
                | (e, Node _ ExitNode) <- Map.findWithDefault [] n cfg ]
        forM_ (Set.toList (unwiped `Set.intersection` candidates)) $ \v -> forM_ (nodeInfoOf n) $ \ni ->
            case (Map.lookup v wipes, Map.lookup v filled) of
                (Just wipe, _) -> recordRichError ni ErrorLevel
                    (backticks (pretty v) <+> "is wiped on other paths, but not before the function returns here")
                    [mkSpan wipe "wiped here"] []
                (Nothing, Just (f, call)) -> recordRichError ni ErrorLevel
                    (backticks (pretty v) <+> "holds a secret, but isn't wiped before the function returns here")
                    [mkSpan call ("other callers of" <+> backticks (pretty f) <+> "wipe what it writes here")] []
                (Nothing, Nothing) -> return ()
  where
    mkSpan ni label =
        let (pos, len) = posAndLen ni
        in DiagnosticSpan pos len [label]

nodeInfoOf :: Node -> Maybe NodeInfo
nodeInfoOf (Node _ kind) = case kind of
    StatNode s   -> Just (annotation s)
    ExprNode e   -> Just (annotation e)
    BranchNode e -> Just (annotation e)
    DeclNode d   -> Just (annotation d)
    _            -> Nothing

analyse :: GlobalDecls -> Trav Env ()
analyse decls = forM_ (Map.elems (gObjs decls)) $ \case
    FunctionDef fd -> checkFunction fd
    _              -> return ()


descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("memzero", Text.unlines
    [ "Checks that a local wiped with `crypto_memzero` on some paths through a"
    , "function is wiped on every path that returns after writing it."
    , ""
    , "Also checks that the output of a function that reads no input, such as a key"
    , "generator, is wiped by all callers if any caller wipes it. Outputs that some"
    , "caller returns, and locals that are never read, are not checked."
    , ""
    , "**Reason:** a wipe says the local holds a secret, such as a key or a"
    , "password. An early return that skips the wipe, often on an error path,"
    , "leaves that secret on the stack."
    ]))
