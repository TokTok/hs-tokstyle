{-# LANGUAGE LambdaCase #-}
module Tokstyle.C.Analysis.IntervalSpec (spec) where

import           Control.Monad                (guard)
import qualified Data.Bits                    as Bits
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Language.C.Data.Ident        (internalIdent)
import           Language.C.Data.Node         (undefNode)
import           Language.C.Syntax.AST        (CAssignOp (..), CBinaryOp (..),
                                               CExpr, CExpression (..),
                                               CUnaryOp (..))
import           Test.Hspec                   (Spec, describe)
import           Test.Hspec.QuickCheck        (modifyMaxSuccess, prop)
import           Test.QuickCheck
import           Tokstyle.Analysis.AccessPath (AccessPath (..))
import           Tokstyle.C.Analysis.Interval


-- | Variables with their types and values, what the facts know about each,
-- and how much more one can be than another.
data World = World
    { worldVars  :: Map String (IntTy, Integer, Maybe (Integer, Integer))
    , worldDiffs :: [((String, String), Integer)]
    }
    deriving (Show)

data Expr
    = Const IntTy Integer
    | Var String
    | Bin CBinaryOp Expr Expr
    | Cast IntTy Expr
    | Neg Expr
    deriving (Show)

types :: [IntTy]
types = [ intTy s b | s <- [False, True], b <- [8, 16, 32, 64] ]

int :: IntTy
int = intTy True 32

comparisons :: [CBinaryOp]
comparisons = [CLeOp, CGrOp, CLeqOp, CGeqOp, CEqOp, CNeqOp]

------------------------------------------------------------------------------
-- What C does, on x86_64 with GCC's conversions to signed types.

promote :: IntTy -> IntTy
promote t = if tyBits t < 32 then int else t

usual :: IntTy -> IntTy -> IntTy
usual a b
    | tySigned a == tySigned b = if tyBits a >= tyBits b then a else b
    | otherwise = let (u, s) = if tySigned a then (b, a) else (a, b)
                  in if tyBits s > tyBits u then s else u

typeOf :: World -> Expr -> IntTy
typeOf w = \case
    Const t _ -> t
    Var n -> let (t, _, _) = worldVars w Map.! n in t
    Cast t _ -> t
    Neg e -> promote (typeOf w e)
    Bin op a b
        | op `elem` comparisons -> int
        | op == CShrOp -> promote (typeOf w a)
        | otherwise -> usual (promote (typeOf w a)) (promote (typeOf w b))

fits :: IntTy -> Integer -> Bool
fits t v = lo (tyRange t) <= v && v <= hi (tyRange t)

converted :: IntTy -> Integer -> Integer
converted t v
    | tySigned t = (v + half) `mod` m - half
    | otherwise = v `mod` m
  where
    m = 2 ^ tyBits t
    half = 2 ^ (tyBits t - 1)

-- | The result of arithmetic in a type, or 'Nothing' if it is undefined, or
-- leaves the range of an unsigned type of 64 bits, which the analysis assumes
-- values never do.
result :: IntTy -> Integer -> Maybe Integer
result t r
    | tySigned t || tyBits t >= 64 = if fits t r then Just r else Nothing
    | otherwise = Just (converted t r)

-- | Whether values an unsigned type of 64 bits computes with are below the
-- top half of its range, as the analysis assumes they are.
modest :: IntTy -> [Integer] -> Bool
modest t vs = tySigned t || tyBits t < 64 || all (< 2 ^ (tyBits t - 1)) vs

value :: World -> Expr -> Maybe Integer
value w = \case
    Const _ v -> Just v
    Var n -> let (_, v, _) = worldVars w Map.! n in Just v
    Cast t e -> converted t <$> value w e
    Neg e -> value w e >>= \x -> let t = promote (typeOf w e) in
        guard (modest t [converted t x]) >> result t (negate (converted t x))
    Bin op a b -> do
        x <- value w a
        y <- value w b
        let common = usual (promote (typeOf w a)) (promote (typeOf w b))
            (x', y') = (converted common x, converted common y)
        guard (modest common [x', y'])
        case op of
            _ | op `elem` comparisons -> Just (if compared op x' y' then 1 else 0)
            CShrOp -> do
                let t = promote (typeOf w a)
                guard (y >= 0 && y < tyBits t)
                Just (converted t x `Bits.shiftR` fromInteger y)
            CAddOp -> result common (x' + y')
            CSubOp -> result common (x' - y')
            CMulOp -> result common (x' * y')
            CDivOp -> guard (y' /= 0) >> result common (x' `quot` y')
            CRmdOp -> guard (y' /= 0) >> result common (x' `rem` y')
            CAndOp -> result common (x' Bits..&. y')
            _ -> Nothing
  where
    compared = \case
        CLeOp  -> (<)
        CGrOp  -> (>)
        CLeqOp -> (<=)
        CGeqOp -> (>=)
        CEqOp  -> (==)
        _      -> (/=)

------------------------------------------------------------------------------
-- What the analysis sees.

facts :: World -> Facts
facts w = Facts 0 False
    (Map.fromList [ (PathVar n, (t, [Range l h Plain])) | (n, (t, _, Just (l, h))) <- Map.toList (worldVars w) ])
    (Map.fromList [ ((PathVar p, PathVar q), c) | ((p, q), c) <- worldDiffs w ])

dummy :: CExpr
dummy = CVar (internalIdent "e") undefNode

term :: World -> Expr -> Term
term w e = Term dummy (Just (typeOf w e)) Nothing Nothing $ case e of
    Const _ v  -> TConst v
    Var n      -> TLoad (PathVar n)
    Bin op a b -> TBin op (term w a) (term w b)
    Cast _ a   -> TCast (term w a)
    Neg a      -> TUnary CMinOp (term w a)

-- | Whether the variables' values are among those the facts allow.
holds :: World -> Facts -> Bool
holds w f =
    and [ any (\r -> lo r <= v && v <= hi r) rs | (PathVar n, (_, rs)) <- Map.toList (factsValues f), let v = val n ]
    && and [ val p - val q <= c | ((PathVar p, PathVar q), c) <- Map.toList (factsDiffs f) ]
  where
    val n = let (_, v, _) = worldVars w Map.! n in v

sumValue :: World -> Linear -> Integer
sumValue w (Linear ps c) = c + sum [ k * v | (PathVar n, (k, _)) <- Map.toList ps, let (_, v, _) = worldVars w Map.! n ]

------------------------------------------------------------------------------
-- Generators

valueOf :: IntTy -> Gen Integer
valueOf t = frequency
    [ (1, elements [lo (tyRange t), hi (tyRange t)])
    , (2, elements (filter (fits t) [-2, -1, 0, 1, 2, 16]))
    , (3, choose (lo (tyRange t), hi (tyRange t)))
    ]

world :: Gen World
world = do
    vars <- mapM var ["a", "b", "c"]
    let vals = Map.fromList [ (n, v) | (n, (_, v, _)) <- vars ]
    diffs <- sublistOf [ (p, q) | p <- Map.keys vals, q <- Map.keys vals, p /= q ]
    slacks <- mapM (const (elements [0, 0, 1, 7])) diffs
    return $ World (Map.fromList vars)
        [ (pq, vals Map.! p - vals Map.! q + s) | (pq@(p, q), s) <- zip diffs slacks ]
  where
    var n = do
        t <- elements types
        v <- valueOf t
        known <- frequency [(1, pure Nothing), (3, Just <$> around t v)]
        return (n, (t, v, known))
    around t v = (,) <$> oneof [pure v, choose (lo (tyRange t), v)] <*> oneof [pure v, choose (v, hi (tyRange t))]

expr :: Int -> Gen Expr
expr n
    | n <= 0 = leaf
    | otherwise = frequency
        [ (2, leaf)
        , (6, Bin <$> elements [CAddOp, CSubOp, CMulOp, CDivOp, CRmdOp, CAndOp, CShrOp] <*> sub <*> sub)
        , (2, Bin <$> elements comparisons <*> sub <*> sub)
        , (1, Cast <$> elements types <*> sub)
        , (1, Neg <$> sub)
        ]
  where
    sub = expr (n `div` 2)
    leaf = frequency
        [ (3, Var <$> elements ["a", "b", "c"])
        , (2, elements types >>= \t -> Const t <$> valueOf t)
        ]

linearExpr :: Int -> Gen Expr
linearExpr n
    | n <= 0 = leaf
    | otherwise = frequency
        [ (1, leaf)
        , (2, Bin <$> elements [CAddOp, CSubOp] <*> sub <*> sub)
        , (1, Bin CMulOp <$> sub <*> (Const int <$> choose (0, 5)))
        , (1, Cast <$> elements types <*> sub)
        ]
  where
    sub = linearExpr (n `div` 2)
    leaf = oneof [Var <$> elements ["a", "b", "c"], Const int <$> choose (-3, 20)]

shrinkExpr :: Expr -> [Expr]
shrinkExpr = \case
    Bin op a b -> [a, b] ++ [ Bin op a' b | a' <- shrinkExpr a ] ++ [ Bin op a b' | b' <- shrinkExpr b ]
    Cast t a -> a : map (Cast t) (shrinkExpr a)
    Neg a -> a : map Neg (shrinkExpr a)
    Const t v -> [ Const t v' | v' <- shrink v, fits t v' ]
    Var _ -> []

defined :: World -> Expr -> (Integer -> Property) -> Property
defined w e p = maybe discard p (value w e)


spec :: Spec
spec = modifyMaxSuccess (const 2000) $ describe "the interval analysis" $ do
    prop "eval gives the values a term can have" $
        forAll world $ \w -> forAllShrink (sized expr) shrinkExpr $ \e -> defined w e $ \v ->
            let rs = eval (facts w) (term w e)
            in counterexample (show (v, rs)) $
                maybe False (any (\r -> lo r <= v && v <= hi r)) rs

    prop "a term's exact sum is its value" $
        forAll world $ \w -> forAllShrink (sized linearExpr) shrinkExpr $ \e -> defined w e $ \v ->
            case exactSum (facts w) (term w e) of
                Nothing -> property True
                Just s  -> counterexample (show (v, sumValue w s)) $ v == sumValue w s

    prop "a sum is at most a bound when atMost says so" $
        forAll world $ \w -> forAllShrink (sized linearExpr) shrinkExpr $ \e -> forAllShrink (sized linearExpr) shrinkExpr $ \b ->
            forAll (choose (-2, 3)) $ \k -> forAll (choose (-3, 3)) $ \c ->
                defined w e $ \v -> defined w b $ \bv ->
                    case exactSum (facts w) (term w b) of
                        Just bound | atMost (facts w) [(k, term w e)] c bound ->
                            counterexample (show (k * v + c, bv)) $ k * v + c <= bv
                        _ -> property True

    prop "assume keeps the values with which a condition has its truth" $
        forAll world $ \w -> forAllShrink (Bin <$> elements comparisons <*> sized linearExpr <*> sized linearExpr) shrinkExpr $ \cond ->
            defined w cond $ \v ->
                case assume (v /= 0) (term w cond) (facts w) of
                    Nothing -> counterexample "can't hold" False
                    Just f  -> property (holds w f)

    prop "effects keep what the facts say true after an assignment" $
        forAll world $ \w -> forAll (elements [CAssignOp, CAddAssOp, CSubAssOp]) $ \op ->
            forAllShrink (sized linearExpr) shrinkExpr $ \e ->
                let (t, _, known) = worldVars w Map.! "a"
                    rhs = case op of
                        CAddAssOp -> Bin CAddOp (Var "a") e
                        CSubAssOp -> Bin CSubOp (Var "a") e
                        _         -> e
                    assign = Term dummy (Just t) Nothing Nothing
                        (TAssign (Just (PathVar "a")) op (term w (Var "a")) (term w e))
                in defined w rhs $ \v ->
                    let after = w { worldVars = Map.insert "a" (t, converted t v, known) (worldVars w) }
                    in property (holds after (effects True assign (facts w)))

    prop "convert gives the values C's conversions do" $
        forAll (elements types) $ \from -> forAll (elements types) $ \to ->
            forAll (valueOf from) $ \v -> forAll (valueOf from) $ \v' ->
                let r = Range (min v v') (max v v') Plain
                in forAll (choose (lo r, hi r)) $ \x ->
                    let rs = convert undefNode to r
                        c = converted to x
                    in counterexample (show (c, rs)) $ any (\y -> lo y <= c && c <= hi y) rs
