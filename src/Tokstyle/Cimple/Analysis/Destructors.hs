{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
-- | Inference of destructors: functions that free their arguments.
module Tokstyle.Cimple.Analysis.Destructors
    ( Destructors
    , destructors
    , destroyed
    ) where

import           Data.Fix                           (Fix (..))
import qualified Data.List                          as List
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Maybe                         (isNothing)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Language.Cimple                    (BinaryOp (..), Lexeme (..),
                                                     NodeF (..), UnaryOp (..))
import qualified Language.Cimple                    as C
import           Tokstyle.Analysis.Dataflow         (Dataflow (..), solve)
import           Tokstyle.Cimple.Analysis.CFG       (EdgeType (..), Node (..),
                                                     NodeKind (..),
                                                     fromFunction)
import           Tokstyle.Cimple.Analysis.Functions


data Destroys = All | Fields (Set Text)
    deriving (Eq)

type Destructors = Map Text Done

-- | Get the arguments a call destroys, wholly or partly.
destroyed :: Destructors -> C.Node (Lexeme Text) -> [C.Node (Lexeme Text)]
destroyed ds e = map snd (destroyedBy ds e)

destroyedBy :: Destructors -> C.Node (Lexeme Text) -> [(Destroys, C.Node (Lexeme Text))]
destroyedBy ds e = case unFix' (strip e) of
    FunctionCall callee args | L _ _ g <- calleeLexeme callee, Just ps <- Map.lookup g ds ->
        [ (d, a) | (i, a) <- zip [0 ..] args, Just d <- [Map.lookup i ps] ]
    _ -> []

-- | Infer the destructors of a program: functions that destroy a parameter or
-- some of its fields on every exit except failure returns.
destructors :: [(FilePath, [C.Node (Lexeme Text)])] -> Destructors
destructors tus = bottomUp callees add look (definitions tus) (Map.map (`Map.singleton` All) freers)
  where
    add ds f
        | Map.null ps = ds
        | otherwise   = Map.insert (fnName f) ps ds
      where ps = destroys ds f
    look ds f = Map.lookup (fnName f) ds

-- | Dataflow state: what every path to a node has destroyed of each
-- parameter.
type Done = Map Int Destroys

-- | Meet of two path states: what both paths destroyed.
meet :: Done -> Done -> Done
meet = Map.mergeWithKey (\_ a b -> both a b) (const Map.empty) (const Map.empty)
  where
    both All d                     = Just d
    both d All                     = Just d
    both (Fields a) (Fields b)
        | Set.null c = Nothing
        | otherwise  = Just (Fields c)
      where c = Set.intersection a b

destroy :: Part -> Done -> Done
destroy = \case
    Whole i   -> Map.insert i All
    Field i f -> Map.insertWith more i (Fields (Set.singleton f))
  where
    more _ All                 = All
    more (Fields a) (Fields b) = Fields (Set.union a b)
    more All _                 = All

replace :: Part -> Done -> Done
replace = \case
    Whole i   -> Map.delete i
    Field i f -> Map.update (without f) i
  where
    without _ All = Just All
    without f (Fields fs)
        | Set.null fs' = Nothing
        | otherwise    = Just (Fields fs')
      where fs' = Set.delete f fs

destroys :: Destructors -> Function -> Done
destroys ds fn = case ends of
    [] -> Map.empty
    _  -> List.foldl1' meet ends
  where
    (entry, cfg) = fromFunction (fnDefn fn)
    params = Map.fromList (zip (fnParams fn) [0 ..])
    -- Locals only ever holding a parameter, e.g. @Foo *foo = (Foo *)user_data;@.
    aliases = Map.fromList
        [ (Text.pack v, i)
        | (v, Just ini) <- localDecls (fnDefn fn)
        , null (Map.findWithDefault [] v (assignments (fnDefn fn)))
        , Just i <- [varIn params ini] ]
    vars = Map.union params aliases
    owner = partOf vars
    under = fieldUnder vars

    step n done = List.foldl' (flip expr) done (nodeExprs n)
    -- A field assigned new memory after it is freed was replaced, not
    -- destroyed.
    expr e done = List.foldl' (flip replace) (List.foldl' (flip destroy) done gone) replaced
      where
        gone = concat
            [ parts d p | c <- universe e, (d, a) <- destroyedBy ds c, Just p <- [owner a] ]
        replaced =
            [ p | Fix (AssignExpr l _ r) <- universe e
                , isNothing (constName r), isNothing (intLit r), Just p <- [under l] ]
    -- Map what a callee destroys of an argument to fields of the parameter
    -- passed as that argument.
    parts d p = case (d, p) of
        (Fields fs, Whole i) -> [ Field i f | f <- Set.toList fs ]
        _                    -> [p]
    -- A null pointer needs no destroying, so it counts as destroyed on the
    -- branch where it is null, e.g. both @a@ and @b@ on the true branch of
    -- @a == nullptr || b == nullptr@.
    edge n e done = case n of
        Node _ (BranchNode cond)
            | e == TrueBranch, Just ps <- nullAmong cond -> List.foldl' (flip destroy) done ps
            | Just (p, nullOn) <- nullTest owner cond, nullOn == e -> destroy p done
        _ -> done
    nullAmong cond = case unFix' (strip cond) of
        BinaryExpr l BopOr r -> (++) <$> nullAmong l <*> nullAmong r
        _ | Just (p, TrueBranch) <- nullTest owner cond -> Just [p]
        _ -> Nothing

    result = solve entry cfg Dataflow
        { transfer     = step
        , edgeTransfer = edge
        , merge        = meet
        , initial      = Map.empty
        }

    ends =
        [ edge n e (step n st)
        | (n, out) <- Map.toList cfg
        , (e, Node _ ExitNode) <- out
        , not (failing n)
        , Just st <- [Map.lookup n result]
        ]
    failing = \case
        Node _ (StmtNode (Fix (Return e))) -> returnOutcomes (fnShape fn) e == [Failure]
        _                                  -> False

varIn :: Map Text Int -> C.Node (Lexeme Text) -> Maybe Int
varIn vars e = case unFix' (strip e) of
    VarExpr (L _ _ v) -> Map.lookup v vars
    CastExpr _ x      -> varIn vars x
    _                 -> Nothing

data Part = Whole Int | Field Int Text
    deriving (Eq, Ord)

-- | Get the parameter or parameter field an expression names, looking
-- through casts and @&@.
partOf :: Map Text Int -> C.Node (Lexeme Text) -> Maybe Part
partOf vars e = case unFix' (strip e) of
    UnaryExpr UopAddress x    -> partOf vars x
    CastExpr _ x              -> partOf vars x
    PointerAccess x (L _ _ f) -> (`Field` f) <$> varIn vars x
    MemberAccess x (L _ _ f)  -> (`Field` f) <$> varIn vars x
    _                         -> Whole <$> varIn vars e

-- | Get the parameter, or its field, that an expression accesses at any
-- depth.
fieldUnder :: Map Text Int -> C.Node (Lexeme Text) -> Maybe Part
fieldUnder vars e = case unFix' (strip e) of
    PointerAccess x (L _ _ f) | Just i <- varIn vars x -> Just (Field i f)
    MemberAccess x (L _ _ f) | Just i <- varIn vars x  -> Just (Field i f)
    PointerAccess x _                                  -> fieldUnder vars x
    MemberAccess x _                                   -> fieldUnder vars x
    ArrayAccess x _                                    -> fieldUnder vars x
    UnaryExpr UopDeref x                               -> fieldUnder vars x
    CastExpr _ x                                       -> fieldUnder vars x
    _                                                  -> Whole <$> varIn vars e
