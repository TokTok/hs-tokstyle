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
import           Language.C.Analysis.SemRep    (FunDef (..), FunType (..),
                                                GlobalDecls (..),
                                                IdentDecl (..), Type (..),
                                                constant, declType)
import           Language.C.Analysis.TravMonad (Trav, lookupObject)
import           Language.C.Analysis.TypeUtils (canonicalType, typeQuals)
import           Language.C.Data.Ident         (Ident (..))
import           Language.C.Data.Node          (NodeInfo, posOfNode)
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants   (getCInteger)
import           Language.Cimple.Diagnostics   (DiagnosticSpan (..))
import           Prettyprinter                 (pretty, (<+>))
import           Tokstyle.Analysis.Dataflow    (Dataflow (..), solve)
import           Tokstyle.C.Analysis.CFG       (CFG, Node (..), NodeKind (..),
                                                fromFunDef)
import           Tokstyle.C.Analysis.Syntax    (stats)
import           Tokstyle.C.Env                (DiagnosticLevel (..), Env,
                                                posAndLen, recordRichError)
import           Tokstyle.C.TravUtils          (backticks)


-- | Functions that wipe the object their first argument points to.
wipers :: [String]
wipers = ["crypto_memzero", "sodium_memzero"]

-- | Functions that take a writable pointer but leave its contents as they are.
keepContents :: [String]
keepContents = ["crypto_memlock", "crypto_memunlock", "sodium_mlock", "sodium_munlock"]

data Effect
    = Declare String
    | Write String
    | Wipe String NodeInfo

-- | Locals that may hold data not wiped since it was written.
type Unwiped = Set String

name :: Ident -> String
name (Ident n _ _) = n

-- | The local an argument of a wiper names, as in @x@ or @&x@.
wiped :: CExpr -> Maybe String
wiped = \case
    CVar i _          -> Just (name i)
    CUnary CAdrOp e _ -> wiped e
    CCast _ e _       -> wiped e
    _                 -> Nothing

-- | The local whose contents an assignment to this expression changes.
assigned :: CExpr -> Maybe String
assigned = \case
    CVar i _          -> Just (name i)
    CIndex e _ _      -> assigned e
    CMember e _ _ _   -> assigned e
    CUnary CIndOp e _ -> assigned e
    _                 -> Nothing

subExprs :: CExpr -> [CExpr]
subExprs = \case
    CComma es _      -> es
    CAssign _ l r _  -> [l, r]
    CCond c t e _    -> c : maybe [] pure t ++ [e]
    CBinary _ l r _  -> [l, r]
    CCast _ e _      -> [e]
    CUnary _ e _     -> [e]
    CIndex b i _     -> [b, i]
    CCall f args _   -> f : args
    CMember e _ _ _  -> [e]
    CComplexReal e _ -> [e]
    CComplexImag e _ -> [e]
    _                -> []

-- | The names an expression mentions, outside @sizeof@.
mentions :: CExpr -> [String]
mentions = \case
    CVar i _      -> [name i]
    CSizeofExpr{} -> []
    e             -> concatMap mentions (subExprs e)

-- | Locals whose address an expression keeps, beyond passing it to a call.
escapes :: CExpr -> [String]
escapes = \case
    CVar i _                    -> [name i]
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

-- | Whether a call's argument at each position can be written through.
writableParams :: CExpr -> Trav Env (Int -> Bool)
writableParams = \case
    CVar i _ -> lookupObject i >>= \case
        Just d | FunctionType (FunType _ params _) _ <- canonicalType (declType d) ->
            let writable = map (isWritable . declType) params
            in return $ \n -> fromMaybe True (lookup n (zip [0 ..] writable))
        _ -> return (const True)
    _ -> return (const True)
  where
    isWritable ty = case canonicalType ty of
        PtrType t _ _     -> not (constant (typeQuals t))
        ArrayType t _ _ _ -> not (constant (typeQuals t))
        _                 -> False

exprEffects :: CExpr -> Trav Env [Effect]
exprEffects = \case
    CCall (CVar f _) (target : rest) ni | name f `elem` wipers -> do
        effs <- concat <$> mapM exprEffects rest
        return $ effs ++ maybe [] (\v -> [Wipe v ni]) (wiped target)
    CCall (CVar f _) args _ | name f `elem` keepContents ->
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

declItems :: CDecl -> [(Ident, CDeclr, Maybe CInit)]
declItems = \case
    CDecl _ ds _ -> [ (i, d, ini) | (Just d@(CDeclr (Just i) _ _ _ _), ini, _) <- ds ]
    _            -> []

initExprs :: CInit -> [CExpr]
initExprs = \case
    CInitExpr e _ -> [e]
    CInitList l _ -> concatMap (initExprs . snd) l

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
        let written = [ Write (name i) | Just v <- [ini], not (isZero v) ]
        return $ effs ++ Declare (name i) : written)
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

-- | Every expression a statement evaluates, including in nested statements.
statExprs :: CStat -> [CExpr]
statExprs s = concatMap own (stats s)
  where
    own = \case
        CExpr e _           -> maybe [] pure e
        CReturn e _         -> maybe [] pure e
        CIf c _ _ _         -> [c]
        CSwitch c _ _       -> [c]
        CWhile c _ _ _      -> [c]
        CFor i c st _ _     -> either (maybe [] pure) declInits i ++ maybe [] pure c ++ maybe [] pure st
        CCompound _ items _ -> concat [ declInits d | CBlockDecl d <- items ]
        _                   -> []
    declInits d = concat [ maybe [] initExprs ini | (_, _, ini) <- declItems d ]

-- | The locals a body declares, with their declarators.
localDecls :: CStat -> [(Ident, CDeclr)]
localDecls body = concatMap own (stats body)
  where
    own = \case
        CCompound _ items _    -> concat [ decl d | CBlockDecl d <- items ]
        CFor (Right d) _ _ _ _ -> decl d
        _                      -> []
    decl d = [ (i, declr) | (i, declr, _) <- declItems d ]

wipeTargets :: CExpr -> [String]
wipeTargets e = case e of
    CCall (CVar f _) (target : _) _ | name f `elem` wipers -> maybe [] pure (wiped target)
    _ -> concatMap wipeTargets (subExprs e)

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
    let decls = localDecls body
        counts = Map.fromListWith (+) [ (name i, 1 :: Int) | (i, _) <- decls ]
        isObject (CDeclr _ derived _ _ _) = not (any isPtrOrFun derived)
        isPtrOrFun = \case
            CPtrDeclr{} -> True
            CFunDeclr{} -> True
            _           -> False
        objects = Set.fromList [ name i | (i, d) <- decls, isObject d, counts Map.! name i == 1 ]
        wipes = Map.fromListWith (\a b -> if posOfNode a < posOfNode b then a else b)
            [ (v, ni) | effs <- Map.elems effects, Wipe v ni <- effs ]
        excluded = Set.fromList $ unmodelledWipes body ++ concatMap escapes (statExprs body)
        candidates = (objects `Set.intersection` Map.keysSet wipes) `Set.difference` excluded
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
            recordRichError ni ErrorLevel
                (backticks (pretty v) <+> "is wiped on other paths, but not before the function returns here")
                [mkSpan (wipes Map.! v) "wiped here"] []
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
    , "**Reason:** a wipe says the local holds a secret, such as a key or a"
    , "password. An early return that skips the wipe, often on an error path,"
    , "leaves that secret on the stack."
    ]))
