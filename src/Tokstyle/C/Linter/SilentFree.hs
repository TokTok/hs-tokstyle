{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.SilentFree (descr) where

import           Control.Monad                 (forM_, unless)
import qualified Data.List                     as List
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe, mapMaybe)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.C.Analysis.SemRep    (CompType (..), CompTypeRef (..),
                                                DeclAttrs (..), EnumType (..),
                                                EnumTypeRef (..), FunDef (..),
                                                FunType (..), GlobalDecls (..),
                                                IdentDecl (..), Linkage (..),
                                                MemberDecl (..), ParamDecl (..),
                                                Storage (..), TagDef (..),
                                                Type (..), TypeDef (..),
                                                TypeName (..), VarDecl (..),
                                                VarName (..), declIdent,
                                                declType)
import           Language.C.Analysis.TravMonad (Trav, getUserState)
import           Language.C.Analysis.TypeUtils (canonicalType)
import           Language.C.Data.Ident         (Ident (..), SUERef (..))
import           Language.C.Data.Node          (NodeInfo)
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants   (getCInteger)
import           Language.Cimple.Diagnostics   (DiagnosticSpan (..))
import           Prettyprinter                 (pretty, (<+>))
import           Tokstyle.Analysis.Dataflow    (Dataflow (..), solve)
import           Tokstyle.C.Analysis.CFG       (CFG, EdgeType (..), Node (..),
                                                NodeKind (..), endsPath,
                                                fromFunDef)
import           Tokstyle.C.Analysis.Syntax    (exprChildren, initExprs, stats)
import           Tokstyle.C.Env                (DiagnosticLevel (..), Env,
                                                localsOf, posAndLen,
                                                recordRichError)
import           Tokstyle.C.TravUtils          (backticks)


-- | Functions that free the object their argument at this position points to.
freers :: [(String, Int)]
freers = [("free", 0), ("mem_delete", 1)]

allocators :: [String]
allocators = ["malloc", "calloc", "realloc", "mem_alloc", "mem_balloc", "mem_valloc", "mem_vrealloc", "mem_brealloc"]

-- | Where an object gets freed: the calls it goes through, from the one in
-- the function at hand to the one that frees it.
type Site = [(NodeInfo, String)]

-- | What is known about the functions of the translation unit.
data Summary = Summary
    { notifies :: Map String (Set Int)
    -- ^ The parameters each function passes to a callback on every path.
    , frees    :: Map String (Map Int Site)
    -- ^ The parameters each function may free before passing them to a
    -- callback.
    } deriving (Eq)

data Effect
    = Notify String
    | Free String Site

name :: Ident -> String
name (Ident n _ _) = n

argVar :: CExpr -> Maybe String
argVar = \case
    CVar i _    -> Just (name i)
    CCast _ e _ -> argVar e
    _           -> Nothing

pointee :: Type -> Maybe String
pointee ty = case canonicalType ty of
    PtrType t _ _ -> case canonicalType t of
        DirectType (TyComp (CompTypeRef (NamedRef i) _ _)) _ _ -> Just (name i)
        _                                                      -> Nothing
    _ -> Nothing

structMembers :: GlobalDecls -> Map String [(String, Type)]
structMembers (GlobalDecls _ tags _) = Map.fromList
    [ (name i, [ (name m, t) | MemberDecl (VarDecl (VarName m _) _ t) _ _ <- ms ])
    | (NamedRef i, CompDef (CompType _ _ ms _ _)) <- Map.toList tags ]

-- | The structs that callbacks take pointers to and that have a @void *@
-- member pointing back to the object's owner. The owner may keep a pointer to
-- the object, so it must hear when the object goes.
exposedStructs :: GlobalDecls -> Set String
exposedStructs decls@(GlobalDecls _ _ typedefs) =
    Set.filter backRef . Set.fromList . concatMap callbackParams $ memberTypes ++ typedefTypes
  where
    members = structMembers decls
    memberTypes = concatMap (map snd) (Map.elems members)
    backRef s = any (isVoidPtr . snd) (Map.findWithDefault [] s members)
    isVoidPtr t = case canonicalType t of
        PtrType (DirectType TyVoid _ _) _ _ -> True
        _                                   -> False
    typedefTypes = [ t | TypeDef _ t _ _ <- Map.elems typedefs ]
    -- 'canonicalType' takes a function type to a pointer to it.
    callbackParams t = case canonicalType t of
        PtrType (FunctionType (FunType _ ps _) _) _ _ -> mapMaybe pointee [ pt | ParamDecl (VarDecl _ _ pt) _ <- ps ]
        _                                             -> []

-- | The effects of an expression, in the order it has them. A callback only
-- counts where the expression always calls it.
exprEffects :: Summary -> Bool -> CExpr -> [Effect]
exprEffects summary always = \case
    CCall f args ni ->
        concatMap (exprEffects summary always) args ++ callEffects f args ni
    CCond c t e _ ->
        exprEffects summary always c ++ concatMap (exprEffects summary False) (maybe [] pure t ++ [e])
    CBinary op l r _ | op `elem` [CLndOp, CLorOp] ->
        exprEffects summary always l ++ exprEffects summary False r
    CAssign CAssignOp l r _ | Just v <- argVar l, isFresh r ->
        exprEffects summary always r ++ [Notify v]
    CSizeofExpr{} -> []
    e -> concatMap (exprEffects summary always) (exprChildren e)
  where
    callEffects (CVar f _) args ni =
        let fn = name f
            vars = zip [0 ..] (map argVar args)
            freed = [ Free v [(ni, fn)] | Just i <- [lookup fn freers], Just (Just v) <- [lookup i vars] ]
                 ++ [ Free v ((ni, fn) : site)
                    | (i, site) <- Map.toList (Map.findWithDefault Map.empty fn (frees summary))
                    , Just (Just v) <- [lookup i vars] ]
            told = [ Notify v | always, i <- Set.toList (Map.findWithDefault Set.empty fn (notifies summary)), Just (Just v) <- [lookup i vars] ]
        in told ++ freed
    -- A call through a pointer is a callback.
    callEffects _ args _ = [ Notify v | always, Just v <- map argVar args ]

-- | Whether an expression gets a new object, or none.
isFresh :: CExpr -> Bool
isFresh = \case
    CCall (CVar f _) _ _   -> name f `elem` allocators
    CVar i _               -> name i `elem` ["nullptr", "NULL"]
    CConst (CIntConst i _) -> getCInteger i == 0
    CCast _ e _            -> isFresh e
    _                      -> False

declItems :: CDecl -> [(String, Maybe CInit)]
declItems = \case
    CDecl _ ds _ -> [ (name i, ini) | (Just (CDeclr (Just i) _ _ _ _), ini, _) <- ds ]
    _            -> []

-- | A local made from a new object, or none, counts as passed to a callback:
-- nothing outside the function can know of it yet.
nodeEffects :: Summary -> Node -> [Effect]
nodeEffects summary n@(Node _ kind) = concatMap (exprEffects summary True) (nodeExprs n) ++ case kind of
    DeclNode d -> [ Notify v | (v, Just (CInitExpr e _)) <- declItems d, isFresh e ]
    _          -> []

nodeExprs :: Node -> [CExpr]
nodeExprs (Node _ kind) = case kind of
    StatNode (CExpr e _)   -> maybe [] pure e
    StatNode (CReturn e _) -> maybe [] pure e
    ExprNode e             -> [e]
    BranchNode e           -> [e]
    DeclNode d             -> concatMap (maybe [] initExprs . snd) (declItems d)
    _                      -> []

-- | What a function does to the pointers to exposed objects it has.
data Findings = Findings
    { toldAtExit  :: Set String
    , freedUntold :: [(String, Site)]
    }

analyseFunction :: Summary -> Set NodeInfo -> Set String -> FunDef -> Findings
analyseFunction summary exhaustive tracked fd =
    Findings
        { toldAtExit = fromMaybe Set.empty (Map.findWithDefault Nothing exit told)
        , freedUntold = concat
            [ untold s (effects n)
            | (n, Just s) <- Map.toList told, not (endsPath n) ]
        }
  where
    (entry, cfg) = fromFunDef fd
    nodes = Set.toList . Set.fromList $ Map.keys cfg ++ [ n | es <- Map.elems cfg, (_, n) <- es ]
    exit = case [ n | n@(Node _ ExitNode) <- nodes ] of
        (n : _) -> n
        []      -> entry
    effects n = filter relevant (nodeEffects summary n)
    relevant = \case
        Notify v -> v `Set.member` tracked
        Free v _ -> v `Set.member` tracked
    -- The variables passed to a callback on every path to a node, or
    -- 'Nothing' where no path reaches it.
    told :: Map Node (Maybe (Set String))
    told = solve entry (cfg :: CFG) problem
    apply s = \case
        Notify v -> Set.insert v s
        Free _ _ -> s
    problem = Dataflow
        { transfer     = \n s -> if endsPath n then Nothing else (\t -> List.foldl' apply t (effects n)) <$> s
        , edgeTransfer = \n e s -> case (n, e) of
            (Node _ (BranchNode c), DefaultBranch) | annotation c `Set.member` exhaustive -> Nothing
            _ -> s
        , merge        = \a b -> case (a, b) of
            (Nothing, _)     -> b
            (_, Nothing)     -> a
            (Just x, Just y) -> Just (Set.intersection x y)
        , initial      = Just Set.empty
        }
    untold s0 = go s0
      where
        go _ [] = []
        go s (Notify v : es) = go (Set.insert v s) es
        go s (Free v site : es)
            | v `Set.member` s = go s es
            | otherwise = (v, site) : go s es

data Function = Function
    { fnName       :: String
    , fnDef        :: FunDef
    , fnStatic     :: Bool
    , fnParams     :: [String]
    , fnTracked    :: Set String
    -- ^ Its pointers to exposed objects.
    , fnExhaustive :: Set NodeInfo
    -- ^ The conditions of the switches without a @default@ that have a case
    -- for every value of their enum, so that leaving them unmatched is not a
    -- path.
    }

functionsOf :: Set String -> Map String [IdentDecl] -> GlobalDecls -> [Function]
functionsOf exposed locals decls =
    [ Function fn fd (isStatic storage) ps tracked (exhaustiveSwitches decls (Map.fromList (paramDecls ++ localTys)) body)
    | FunctionDef fd@(FunDef (VarDecl (VarName i _) (DeclAttrs _ storage _) ty) body _) <- Map.elems (gObjs decls)
    , let fn = name i
          paramDecls = case ty of
              FunctionType (FunType _ pds _) _ -> [ (name pi', pt) | ParamDecl (VarDecl (VarName pi' _) _ pt) _ <- pds ]
              _ -> []
          ps = map fst paramDecls
          localTys = [ (name (declIdent d), declType d) | d <- Map.findWithDefault [] fn locals ]
          tracked = Set.fromList [ v | (v, t) <- paramDecls ++ localTys, Just s <- [pointee t], s `Set.member` exposed ]
    ]
  where
    isStatic = \case
        FunLinkage InternalLinkage -> True
        Static InternalLinkage _   -> True
        _                          -> False

data Label = Case | Default
    deriving (Eq)

enumSizes :: GlobalDecls -> Map SUERef Int
enumSizes (GlobalDecls _ tags _) = Map.fromList [ (r, length es) | (r, EnumDef (EnumType _ es _ _)) <- Map.toList tags ]

exhaustiveSwitches :: GlobalDecls -> Map String Type -> CStat -> Set NodeInfo
exhaustiveSwitches decls vars body = Set.fromList
    [ annotation c
    | CSwitch c s _ <- stats body
    , not (hasDefault s)
    , Just (DirectType (TyEnum (EnumTypeRef r _)) _ _) <- [canonicalType <$> typeOf c]
    , Just n <- [Map.lookup r (enumSizes decls)]
    , caseCount s >= n ]
  where
    members = structMembers decls
    typeOf = \case
        CVar i _ -> Map.lookup (name i) vars
        CMember e m arrow _ -> do
            t <- typeOf e
            s <- if arrow then pointee t else direct t
            lookup (name m) =<< Map.lookup s members
        _ -> Nothing
    direct t = case canonicalType t of
        DirectType (TyComp (CompTypeRef (NamedRef i) _ _)) _ _ -> Just (name i)
        _                                                      -> Nothing
    -- The labels of a switch body, not counting those of nested switches.
    labels = \case
        CCase _ t _         -> Case : labels t
        CDefault t _        -> Default : labels t
        CLabel _ t _ _      -> labels t
        CCompound _ items _ -> concat [ labels t | CBlockStmt t <- items ]
        CIf _ t e _         -> labels t ++ maybe [] labels e
        CWhile _ t _ _      -> labels t
        CFor _ _ _ t _      -> labels t
        _                   -> []
    hasDefault = elem Default . labels
    caseCount = length . labels

summarise :: [Function] -> Summary
summarise fns = go (Summary Map.empty Map.empty) (length fns + 2 :: Int)
  where
    go s 0 = s
    go s k =
        let s' = Summary
                { notifies = Map.fromList
                    [ (fnName f, Set.fromList [ i | (i, p) <- zip [0 ..] (fnParams f), p `Set.member` toldAtExit r ])
                    | (f, r) <- results s ]
                , frees = Map.fromList
                    [ (fnName f, Map.fromList [ (i, site) | (v, site) <- reverse (freedUntold r), Just i <- [List.elemIndex v (fnParams f)] ])
                    | (f, r) <- results s, fnStatic f ]
                }
        in if s' == s then s else go s' (k - 1)
    results s = [ (f, analyseFunction s (fnExhaustive f) (fnTracked f) (fnDef f)) | f <- fns ]

analyse :: GlobalDecls -> Trav Env ()
analyse decls = do
    env <- getUserState
    let exposed = exposedStructs decls
        fns = functionsOf exposed (localsOf env) decls
        summary = summarise fns
    unless (Set.null exposed) $ forM_ fns $ \f -> do
        let r = analyseFunction summary (fnExhaustive f) (fnTracked f) (fnDef f)
            -- A function that frees its own parameters is a destructor, and
            -- its caller knows they go.
            destructor = any ((`elem` fnParams f) . fst)
                (freedUntold (analyseFunction summary (fnExhaustive f) (Set.fromList (fnParams f)) (fnDef f)))
        unless destructor $ forM_ (freedUntold r) $ \(v, site) ->
            unless (v `elem` fnParams f) $ report (fnName f) v site

report :: String -> String -> Site -> Trav Env ()
report fn v = \case
    [] -> return ()
    (at, callee) : deeper ->
        recordRichError at ErrorLevel
            (backticks (pretty v) <+> "is freed through" <+> backticks (pretty callee) <+> "in" <+> backticks (pretty fn)
                <+> "without first being passed to a callback")
            [ mkSpan ni ("then" <+> backticks (pretty c)) | (ni, c) <- deeper ]
            [(NoteLevel, "callbacks take pointers to this object, and its owner may still hold one")]
  where
    mkSpan ni label =
        let (pos, len) = posAndLen ni
        in DiagnosticSpan pos len [label]


descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("silent-free", Text.unlines
    [ "Checks that an object is passed to a callback before it is freed, if"
    , "callbacks take pointers to its struct and the struct has a `void *` member"
    , "pointing back to the object's owner. A function may still free an object it"
    , "allocated itself or that its caller passed to it."
    , ""
    , "**Reason:** the owner may keep a pointer to the object. When the object is"
    , "freed without a callback telling the owner, as on an error path, the owner's"
    , "pointer dangles, and its next use is a use after free."
    ]))
