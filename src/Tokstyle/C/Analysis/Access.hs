{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict     #-}
-- | Accesses to memory through variables, and how far into it they reach.
module Tokstyle.C.Analysis.Access
    ( Access (..)
    , AccessBy (..)
    , accesses
    , changedNames
    ) where

import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (isJust)
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Language.C.Data.Ident        (identToString)
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants  (getCInteger)
import           Tokstyle.C.Analysis.Interval (Fact (..), Program (..),
                                               Term (..), TermKind (..))
import           Tokstyle.C.Analysis.Known    (Reach (..), libraryReaches)
import           Tokstyle.C.Analysis.Syntax   (exprChildren, statExprs)

data AccessBy
    = ByElement CExpr
    | ByCall String

-- | An access through a variable: the variable, the terms, each times a
-- coefficient, and the constant whose sum is how far the access reaches, and
-- what makes it.
data Access = Access
    { accBuffer :: String
    , accTerms  :: [(Integer, Term)]
    , accConst  :: Integer
    , accBy     :: AccessBy
    }

-- | The accesses a term makes, in calls to the functions in 'progReaches'
-- and the library's.
accesses :: Program -> Term -> [Access]
accesses prog t = case termKind t of
    TCall f args | CVar g _ <- termExpr f, fn <- identToString g, Just rs <- reaches fn ->
        [ acc
        | Reach p l c <- rs, p < length args, all ((< length args) . fst) l
        , Just acc <- [call fn [ (k, args !! i) | Just (i, k) <- [l] ] c (args !! p)] ]
    _ | Just (b, ts, k) <- element t -> [Access b ts (k + 1) (ByElement (termExpr t))]
    _ -> []
  where
    reaches fn = case termFact t of
        Just (CallFact k) | Just rs <- Map.lookup k (progReaches prog) -> Just rs
        _                                                             -> Map.lookup fn libraryReaches
    call fn ns c p = case termKind p of
        TCast a -> call fn ns c a
        _       -> (\(b, ts, k) -> Access b (ts ++ ns) (k + c) (ByCall fn)) <$> pointsInto p

-- | The variable a pointer points into, and the terms and constant it adds.
pointsInto :: Term -> Maybe (String, [(Integer, Term)], Integer)
pointsInto t = case termKind t of
    TBin CAddOp a k | isJust (termTy k) -> (\(b, ts, c) -> (b, ts ++ [(1, k)], c)) <$> pointsInto a
    TUnary CAdrOp x -> element x
    _ | CVar i _ <- termExpr t -> Just (identToString i, [], 0)
    _ -> Nothing

-- | The variable an element is in, and the terms and constant that index it.
-- An element at a constant index is a path the analysis tracks.
element :: Term -> Maybe (String, [(Integer, Term)], Integer)
element t = case (termKind t, termExpr t) of
    (TIndex a i _, _) -> (\(b, ts, c) -> (b, ts ++ [(1, i)], c)) <$> pointsInto a
    (TLoad _, CIndex (CVar b _) (CConst (CIntConst k _)) _) -> Just (identToString b, [], getCInteger k)
    _ -> Nothing

-- | The variables a function body changes or takes the address of.
changedNames :: CStat -> Set String
changedNames body = Set.fromList [ identToString i | e <- concatMap everyExpr (statExprs body), CVar i _ <- written e ]
  where
    everyExpr e = e : concatMap everyExpr (exprChildren e)
    written = \case
        CAssign _ l _ _ -> [l]
        CUnary op l _ | op `elem` [CPreIncOp, CPreDecOp, CPostIncOp, CPostDecOp, CAdrOp] -> [l]
        _ -> []
