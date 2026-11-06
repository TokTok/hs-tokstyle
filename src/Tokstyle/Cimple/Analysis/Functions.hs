{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
-- | Function definitions with their success conventions, and helpers for
-- analysing function bodies.
module Tokstyle.Cimple.Analysis.Functions
    ( Function (..)
    , Shape (..)
    , Outcome (..)
    , definitions
    , callees
    , bottomUp
    , freers
    , allocators
    , bare
    , accessPath
    , mapRoot
    , returnOutcomes
    , branchOn
    , calleeLexeme
    , universe
    , strip
    , unFix'
    , intLit
    , constName
    , isNullLit
    , nullTest
    , isEnumConst
    , flagHolds
    , localDecls
    , assignments
    , nodeExprs
    , rootOf
    , throughPointer
    , render
    ) where

import           Data.Fix                     (Fix (..))
import qualified Data.Graph                   as Graph
import qualified Data.List                    as List
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (mapMaybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Language.Cimple              (BinaryOp (..), Lexeme (..),
                                               NodeF (..), UnaryOp (..))
import qualified Language.Cimple              as C
import           Language.Cimple.Pretty       (showNodePlain)
import           Prettyprinter                (Doc, pretty)
import           Tokstyle.Analysis.AccessPath (AccessPath (..))
import           Tokstyle.Cimple.Analysis.CFG (EdgeType (..), Node (..),
                                               NodeKind (..), getFuncName)


-- | The convention a function's return value uses to report success.
data Shape
    = BoolShape
    -- ^ Returns @true@ on success and @false@ on failure.
    | ZeroShape
    -- ^ Returns 0 on success and a negative number on failure.
    | IdShape
    -- ^ Returns a negative number on failure and anything else on success.
    | PtrShape
    -- ^ Returns @nullptr@ on failure.
    | NoShape

data Outcome = Success | Failure
    deriving (Eq, Ord, Show, Enum, Bounded)

data Function = Function
    { fnFile   :: FilePath
    , fnName   :: Text
    , fnParams :: [Text]
    , fnShape  :: Shape
    , fnDefn   :: C.Node (Lexeme Text)
    }

universe :: C.Node (Lexeme Text) -> [C.Node (Lexeme Text)]
universe node@(Fix n) = node : concatMap universe (foldr (:) [] n)

lexemeText :: Lexeme Text -> Text
lexemeText (L _ _ t) = t

strip :: C.Node (Lexeme Text) -> C.Node (Lexeme Text)
strip = \case
    Fix (ParenExpr e) -> strip e
    e                 -> e

-- | Get the value of an integer literal, including negative ones.
intLit :: C.Node (Lexeme Text) -> Maybe Integer
intLit e = case unFix' (strip e) of
    LiteralExpr C.Int (L _ _ t)                    -> readInt t
    UnaryExpr UopMinus (Fix (LiteralExpr C.Int l)) -> negate <$> readInt (lexemeText l)
    _                                              -> Nothing
  where
    readInt t = case reads (Text.unpack t) of
        [(n, "")] -> Just n
        _         -> Nothing

unFix' :: C.Node (Lexeme Text) -> NodeF (Lexeme Text) (C.Node (Lexeme Text))
unFix' (Fix n) = n

-- | Get the name of a named constant, e.g. @true@, @nullptr@ or @TOX_OK@.
constName :: C.Node (Lexeme Text) -> Maybe Text
constName e = case unFix' (strip e) of
    LiteralExpr _ (L _ _ t) | Text.all (\c -> c == '_' || c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9']) t
                            , not (Text.null t) -> Just t
    _ -> Nothing

isNullLit :: C.Node (Lexeme Text) -> Bool
isNullLit e = constName e `elem` [Just "nullptr", Just "NULL"]

-- | Find the expression a condition tests for null, mapped through @view@,
-- and the branch on which it is null.
nullTest :: (C.Node (Lexeme Text) -> Maybe a) -> C.Node (Lexeme Text) -> Maybe (a, EdgeType)
nullTest view cond = case unFix' (strip cond) of
    UnaryExpr UopNot x | Just v <- view x -> Just (v, TrueBranch)
    -- Both operands of @&&@ hold on its true branch.
    BinaryExpr l BopAnd r -> case (nullTest view l, nullTest view r) of
        (Just t@(_, TrueBranch), _) -> Just t
        (_, Just t@(_, TrueBranch)) -> Just t
        _                           -> Nothing
    BinaryExpr x op r | isNullLit r, Just v <- view x -> case op of
        BopEq -> Just (v, TrueBranch)
        BopNe -> Just (v, FalseBranch)
        _     -> Nothing
    _ | Just v <- view cond -> Just (v, FalseBranch)
    _ -> Nothing

isEnumConst :: Text -> Bool
isEnumConst t = Text.all (\c -> c == '_' || c `elem` ['A'..'Z'] || c `elem` ['0'..'9']) t
    && Text.any (`elem` ['A'..'Z']) t

-- | Whether a branch on an error flag can be taken with the enum constants
-- the flags hold.
flagHolds :: Map String Text -> C.Node (Lexeme Text) -> EdgeType -> Bool
flagHolds flags cond e = case unFix' (strip cond) of
    BinaryExpr (Fix (VarExpr (L _ _ v))) op r
        | Just c <- constName r, Just c' <- Map.lookup (Text.unpack v) flags, op `elem` [BopEq, BopNe] ->
            let equal = c == c'
            in case e of
                TrueBranch  -> if op == BopEq then equal else not equal
                FalseBranch -> if op == BopEq then not equal else equal
                _           -> True
    _ -> True

shapeOf :: C.Node (Lexeme Text) -> C.Node (Lexeme Text) -> Shape
shapeOf retType body
    | "bool" `Text.isInfixOf` ty = BoolShape
    | "*" `Text.isInfixOf` ty = PtrShape
    | any (`Text.isInfixOf` ty) ["int", "Index", "Number"] = intShape
    | otherwise = NoShape
  where
    ty = showNodePlain retType
    returned = [ e | Fix (Return (Just e)) <- universe body ]
    lits = map intLit returned
    intShape
        | all (maybe False (<= 0)) lits && Just 0 `elem` lits = ZeroShape
        | any (maybe False (< 0)) lits = IdShape
        | otherwise = NoShape

returnOutcomes :: Shape -> Maybe (C.Node (Lexeme Text)) -> [Outcome]
returnOutcomes shape ret = case (shape, ret) of
    (BoolShape, Just e) | constName e == Just "true"  -> [Success]
                        | constName e == Just "false" -> [Failure]
    (ZeroShape, Just e) | intLit e == Just 0               -> [Success]
                        | maybe False (< 0) (intLit e)     -> [Failure]
    (IdShape, Just e)   | maybe False (< 0) (intLit e)     -> [Failure]
                        | otherwise                        -> [Success]
    (PtrShape, Just e)  | isNullLit e                      -> [Failure]
                        | otherwise                        -> [Success]
    (NoShape, _) -> [Success]
    _ -> [Success, Failure]

-- | For a condition on a call's return value, get the call and the outcome it
-- reported on the true branch.
branchOn :: Map Text Shape -> C.Node (Lexeme Text) -> Maybe (Lexeme Text, Outcome)
branchOn shapeMap cond = case unFix' (strip cond) of
    UnaryExpr UopNot e -> fmap flipOutcome <$> branchOn shapeMap e
    BinaryExpr l op r
        | Just (f, s) <- call l, Just n <- intLit r -> compareInt f s op n
        | Just (f, PtrShape) <- call l, isNullLit r -> case op of
            BopEq -> Just (f, Failure)
            BopNe -> Just (f, Success)
            _     -> Nothing
    _ | Just (f, s) <- call cond -> case s of
        BoolShape -> Just (f, Success)
        PtrShape  -> Just (f, Success)
        ZeroShape -> Just (f, Failure)
        _         -> Nothing
    _ -> Nothing
  where
    call e = case unFix' (strip e) of
        FunctionCall callee _ | Just name <- getFuncName callee, Just s <- Map.lookup name shapeMap ->
            Just (calleeLexeme callee, s)
        _ -> Nothing
    flipOutcome Success = Failure
    flipOutcome Failure = Success
    compareInt f s op n = case (s, op, n) of
        (ZeroShape, BopNe, 0) -> Just (f, Failure)
        (ZeroShape, BopEq, 0) -> Just (f, Success)
        (BoolShape, BopNe, 0) -> Just (f, Success)
        (BoolShape, BopEq, 0) -> Just (f, Failure)
        (_, BopLt, 0)         -> Just (f, Failure)
        (_, BopGe, 0)         -> Just (f, Success)
        (_, BopEq, -1)        -> Just (f, Failure)
        (_, BopNe, -1)        -> Just (f, Success)
        _                     -> Nothing

calleeLexeme :: C.Node (Lexeme Text) -> Lexeme Text
calleeLexeme e = case unFix' (strip e) of
    VarExpr l       -> l
    LiteralExpr _ l -> l
    _               -> L (C.AlexPn 0 0 0) C.IdVar ""

definitions :: [(FilePath, [C.Node (Lexeme Text)])] -> [Function]
definitions tus = [ f | (file, nodes) <- tus, node <- nodes, f <- go file node ]
  where
    go file node@(Fix n) = case n of
        FunctionDefn _ (Fix (FunctionPrototype ret (L _ _ name) params)) body ->
            [Function file name (mapMaybe paramName params) (shapeOf ret body) node]
        _ -> concatMap (go file) (foldr (:) [] n)
    paramName = \case
        Fix (VarDecl _ (L _ _ p) _) -> Just p
        Fix (NonNullParam p)        -> paramName p
        Fix (NullableParam p)       -> paramName p
        _                           -> Nothing

-- | Get the local variables of a function with their initialisers.
localDecls :: C.Node (Lexeme Text) -> [(String, Maybe (C.Node (Lexeme Text)))]
localDecls defn = [ (Text.unpack v, ini) | Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ v) _)) ini) <- universe defn ]

-- | Get the values assigned to each local variable after its declaration.
assignments :: C.Node (Lexeme Text) -> Map String [C.Node (Lexeme Text)]
assignments defn = Map.fromListWith (++)
    [ (Text.unpack v, [r]) | Fix (AssignExpr (Fix (VarExpr (L _ _ v))) _ r) <- universe defn ]

rootOf :: AccessPath -> Maybe String
rootOf = \case
    PathVar v     -> Just v
    PathField p _ -> rootOf p
    PathIndex p _ -> rootOf p
    PathDeref p   -> rootOf p
    _             -> Nothing

-- | Check if a path goes through a pointer, i.e. names memory that outlives
-- the function.
throughPointer :: AccessPath -> Bool
throughPointer = \case
    PathDeref{}   -> True
    PathField p _ -> throughPointer p
    PathIndex p _ -> throughPointer p
    _             -> False

render :: AccessPath -> Doc ann
render = pretty . go
  where
    go = \case
        PathVar v                 -> v
        PathField (PathDeref p) m -> go p <> "->" <> m
        PathField p m             -> go p <> "." <> m
        PathIndex p i             -> go p <> "[" <> i <> "]"
        PathDeref p               -> "*" <> go p
        p                         -> show p

nodeExprs :: Node -> [C.Node (Lexeme Text)]
nodeExprs (Node _ kind) = case kind of
    BranchNode e -> [e]
    StmtNode (Fix s) -> case s of
        ExprStmt e             -> [e]
        Return (Just e)        -> [e]
        VarDeclStmt _ (Just e) -> [e]
        _ | isExpr s           -> [Fix s]
        _                      -> []
    _ -> []
  where
    isExpr = \case
        FunctionCall{} -> True
        AssignExpr{}   -> True
        UnaryExpr{}    -> True
        _              -> False

-- | Get the functions a function calls directly by name.
callees :: Function -> [Text]
callees fn = [ name | Fix (FunctionCall callee _) <- universe (fnDefn fn), Just name <- [getFuncName callee] ]

-- | Compute function summaries bottom-up over the strongly connected
-- components of the call graph. @add@ adds a function's summary, and each
-- cycle is iterated until @look@ stops changing, at most @length + 2@ times.
bottomUp :: Eq k => (Function -> [Text]) -> (s -> Function -> s) -> (s -> Function -> k) -> [Function] -> s -> s
bottomUp calls add look fns s0 = List.foldl' component s0 (Graph.stronglyConnComp graph)
  where
    graph = [ (f, fnName f, calls f) | f <- fns ]
    component s = \case
        Graph.AcyclicSCC f -> add s f
        Graph.CyclicSCC fs -> cycleFix s fs (length fs + 2 :: Int)
    cycleFix s _ 0 = s
    cycleFix s fs k =
        let s' = List.foldl' add s fs
        in if map (look s') fs == map (look s) fs then s' else cycleFix s' fs (k - 1)

-- | Deallocation functions, with the position of the argument they free.
freers :: Map Text Int
freers = Map.fromList [("free", 0), ("mem_delete", 1)]

-- | Allocation functions returning fresh memory, with the position of their
-- element count or size argument.
allocators :: Map Text Int
allocators = Map.fromList
    [("malloc", 0), ("calloc", 0), ("mem_alloc", 1), ("mem_balloc", 1), ("mem_valloc", 1)]

bare :: C.Node (Lexeme Text) -> C.Node (Lexeme Text)
bare = \case
    Fix (ParenExpr e)  -> bare e
    Fix (CastExpr _ e) -> bare e
    e                  -> e

-- | Get the access path of an expression naming a variable, field, element
-- or pointee.
accessPath :: C.Node (Lexeme Text) -> Maybe AccessPath
accessPath (Fix node) = case node of
    VarExpr (L _ _ v)         -> Just (PathVar (Text.unpack v))
    PointerAccess e (L _ _ m) -> PathField . PathDeref <$> accessPath e <*> pure (Text.unpack m)
    MemberAccess e (L _ _ m)  -> PathField <$> accessPath e <*> pure (Text.unpack m)
    ArrayAccess e i           -> PathIndex <$> accessPath e <*> pure (Text.unpack (showNodePlain i))
    UnaryExpr UopDeref e      -> PathDeref <$> accessPath e
    ParenExpr e               -> accessPath e
    CastExpr _ e              -> accessPath e
    _                         -> Nothing

mapRoot :: (String -> String) -> AccessPath -> AccessPath
mapRoot f = \case
    PathVar v     -> PathVar (f v)
    PathField p m -> PathField (mapRoot f p) m
    PathIndex p i -> PathIndex (mapRoot f p) i
    PathDeref p   -> PathDeref (mapRoot f p)
    p             -> p
