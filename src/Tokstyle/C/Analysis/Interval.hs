{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict     #-}
-- | Interval analysis of the integer values in a function.
--
-- Each value is a set of ranges, and each range remembers whether arithmetic
-- that wrapped around its type produced it. Branch conditions narrow the
-- ranges of the locals and fields they test.
--
-- The analysis assumes that unsigned values of 64 bits stay below 2^63, and
-- that a function a local's address is passed to doesn't keep the address.
module Tokstyle.C.Analysis.Interval
    ( IntTy (..)
    , intTy
    , Origin (..)
    , Range (..)
    , wrapSite
    , Linear (..)
    , atMost
    , exactSum
    , anySum
    , pathSum
    , constantSum
    , plus
    , scale
    , times
    , Term (..)
    , TermKind (..)
    , Facts (..)
    , Fact (..)
    , Program (..)
    , emptyProgram
    , Function
    , translate
    , solveFunction
    , dependsOn
    , canWrap
    , eval
    , assume
    , effects
    , subTerms
    , subTermsWith
    , normalise
    , convert
    , intType
    ) where

import           Control.Monad                    (foldM, forM, forM_, join)
import           Data.List                        (sortOn)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe, isJust, isNothing,
                                                   listToMaybe, mapMaybe)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Language.C.Analysis.AstAnalysis  (defineParams)
import           Language.C.Analysis.ConstEval    (constEval, iSize, intValue)
import           Language.C.Analysis.DeclAnalysis (analyseTypeDecl)
import           Language.C.Analysis.DefTable     (defineScopedIdent)
import           Language.C.Analysis.MachineDescs (x86_64)
import           Language.C.Analysis.SemRep       (ArraySize (..),
                                                   Enumerator (..), FunDef (..),
                                                   FunType (..), IdentDecl (..),
                                                   IntType (..), ParamDecl (..),
                                                   Type (..), TypeName (..),
                                                   VarDecl (..), VarName (..),
                                                   declIdent, declType)
import           Language.C.Analysis.TravMonad    (MonadTrav, catchTravError,
                                                   enterBlockScope,
                                                   enterFunctionScope,
                                                   leaveBlockScope,
                                                   leaveFunctionScope,
                                                   lookupObject, withDefTable)
import           Language.C.Analysis.TypeUtils    (canonicalType)
import           Language.C.Data.Ident            (identToString)
import           Language.C.Data.Name             (Name)
import           Language.C.Data.Node             (NodeInfo, nameOfNode,
                                                   undefNode)
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants      (getCInteger)
import           Tokstyle.Analysis.AccessPath     (AccessPath (..),
                                                   isPathPrefixOf)
import           Tokstyle.Analysis.Dataflow       (Dataflow (..), solve)
import           Tokstyle.C.Analysis.CFG          (CFG, EdgeType (..),
                                                   Node (..), NodeKind (..),
                                                   endsPath, fromFunDef)
import           Tokstyle.C.Analysis.Known        (Reach (..))
import           Tokstyle.C.Analysis.Syntax       (exprChildren, initExprs,
                                                   localDecls, statExprs)
import           Tokstyle.C.Analysis.Types        (FieldKey, exprType, fieldKey,
                                                   funKey, funType)


data IntTy = IntTy
    { tySigned :: Bool
    , tyBits   :: Integer
    , tyRange  :: Range
    -- ^ The values the type can hold.
    }
    deriving (Eq, Show)

-- | Where the values in a range come from.
data Origin
    = Plain
    -- ^ Any value of the type, or a constant.
    | Reduced
    -- ^ An index reduced with @%@ or @&@.
    | Arith
    -- ^ Arithmetic, which may leave the type's range.
    | Wrapped NodeInfo
    -- ^ Arithmetic that went below zero and wrapped around to the top of an
    -- unsigned type's range, at the given expression.
    | Overflowed NodeInfo
    -- ^ Arithmetic that went past the top of an unsigned type's range and
    -- wrapped around to zero, at the given expression.
    deriving (Eq, Show)

data Range = Range
    { lo     :: Integer
    , hi     :: Integer
    , origin :: Origin
    }
    deriving (Eq, Show)

data Term = Term
    { termExpr    :: CExpr
    , termTy      :: Maybe IntTy
    , termFact    :: Maybe Fact
    , termDefault :: Maybe [Range]
    -- ^ The values the term can have when nothing else is known about it.
    , termKind    :: TermKind
    }

data TermKind
    = TConst Integer
    | TLoad AccessPath
    | TBin CBinaryOp Term Term
    | TUnary CUnaryOp Term
    | TCast Term
    | TCond Term (Maybe Term) Term
    | TAssign (Maybe AccessPath) CAssignOp Term Term
    | TIndex Term Term (Maybe Integer)
    -- ^ An array access, with the array's length if it is known.
    | TCall Term [Term]
    | TOther [Term]
    | TUnknown
    -- ^ An expression the analysis doesn't model, which may change anything.

-- | The values of the locals and fields known at a point in a function. A
-- path that isn't in the map can have any value of its type.
data Facts = Facts
    { factsChanges  :: Int
    , factsBackEdge :: Bool
    -- ^ Whether the facts arrive at a loop head from the end of the loop.
    , factsValues   :: Map AccessPath (IntTy, [Range])
    , factsDiffs    :: Map (AccessPath, AccessPath) Integer
    -- ^ For paths @p@ and @q@, the most @p - q@ can be.
    }
    deriving (Eq)

-- | What a node of the control flow graph evaluates.
data NodeTerms
    = Evaluates [Term]
    | Declares [(AccessPath, IntTy, Maybe Term)] [Term] [Term]
    -- ^ The locals a declaration initialises, the initialisers it evaluates,
    -- and the terms to check, where an integer local's initialiser is an
    -- assignment to the local.
    | Opaque
    -- ^ A statement the graph doesn't model, which may change anything.

intType :: Type -> Maybe IntTy
intType ty = case canonicalType ty of
    DirectType (TyIntegral TyBool) _ _ -> Just (intTy False 1)
    DirectType (TyIntegral it) _ _     -> Just (intTy (isSigned it) (8 * iSize x86_64 it))
    DirectType (TyEnum _) _ _          -> Just (intTy True 32)
    _                                  -> Nothing
  where
    isSigned = (`notElem` [TyUChar, TyUShort, TyUInt, TyUInt128, TyULong, TyULLong])

intTy :: Bool -> Integer -> IntTy
intTy True bits  = IntTy True bits (Range (-(2 ^ (bits - 1))) (2 ^ (bits - 1) - 1) Plain)
intTy False bits = IntTy False bits (Range 0 (2 ^ bits - 1) Plain)

isWrapped :: Origin -> Bool
isWrapped = \case
    Wrapped _ -> True
    _         -> False

-- | The values a conversion to an integer type at an expression turns a
-- range into.
convert :: NodeInfo -> IntTy -> Range -> [Range]
convert _ t@IntTy{tySigned = True} r
    | lo r >= lo full && hi r <= hi full = [r]
    | otherwise = [full]
  where full = tyRange t
convert site t@IntTy{tySigned = False} (Range l h o)
    -- Unsigned values of 64 bits count things in memory, which stay below
    -- 2^63, so arithmetic on them doesn't go past the top of their range.
    | tyBits t >= 64, l < m, h >= m = convert site t (Range l (m - 1) o)
    | h - l >= m =
        [ Range (m + max l (-m)) (m - 1) (moved (-1)) | l < 0 ]
        ++ [ Range (max l 0) (min h (m - 1)) o | l < m, h >= 0 ]
        ++ [ Range 0 (min (h - m) (m - 1)) (moved 1) | h >= m ]
    | ql == qh = [Range (l `mod` m) (h `mod` m) (moved ql)]
    | otherwise = [Range (l `mod` m) (m - 1) (moved ql), Range 0 (h `mod` m) (moved qh)]
  where
    m = hi (tyRange t) + 1
    ql = l `div` m
    qh = h `div` m
    moved :: Integer -> Origin
    moved q
        | q == 0 || isWrapped o = o
        | o `elem` [Plain, Reduced] = Plain
        | q < 0 = Wrapped site
        | otherwise = Overflowed site

-- | Where arithmetic that produced values wrapped around, either way.
wrapSite :: Origin -> Maybe NodeInfo
wrapSite = \case
    Wrapped site    -> Just site
    Overflowed site -> Just site
    _               -> Nothing

joinOrigin :: Origin -> Origin -> Origin
joinOrigin a b
    | a == b = a
    | isWrapped a = a
    | isWrapped b = b
    | Just _ <- wrapSite a = a
    | Just _ <- wrapSite b = b
    | Arith `elem` [a, b] = Arith
    | otherwise = Plain

-- | How many ranges from the same place a value keeps before they become one.
maxRanges :: Int
maxRanges = 3

-- | Sorts ranges and merges those that overlap and come from the same place.
-- Ranges that come from different places can overlap.
normalise :: [Range] -> [Range]
normalise rs = concatMap (cap . mergeRuns . sortOn lo) (groupByOrigin rs)
  where
    groupByOrigin xs = [ [ x | x <- xs, group (origin x) == g ] | g <- distinct (map (group . origin) xs) ]
    -- Values overflow in many places, so they merge whichever it was.
    group = \case
        Overflowed _ -> Nothing
        o            -> Just o
    distinct = foldr (\x seen -> if x `elem` seen then seen else x : seen) []
    mergeRuns (a : b : rest)
        | lo b <= hi a + 1 = mergeRuns (Range (lo a) (max (hi a) (hi b)) (origin a) : rest)
        | otherwise = a : mergeRuns (b : rest)
    mergeRuns short = short
    cap xs
        | length xs > maxRanges = [hull xs]
        | otherwise = xs

hull :: [Range] -> Range
hull xs = Range (minimum (map lo xs)) (maximum (map hi xs)) (foldr1 joinOrigin (map origin xs))

------------------------------------------------------------------------------
-- Translating expressions

-- | What the program as a whole knows about a term: the values of the field it
-- reads, or what the function it calls returns and how far it reaches into
-- its arguments.
data Fact = FieldFact FieldKey | CallFact String

-- | What is known about the program as a whole.
data Program = Program
    { progFields  :: Map FieldKey [Range]
    -- ^ The values each field can have, for the fields the program only
    -- changes in ways the analysis sees.
    , progReturns :: Map String [Range]
    -- ^ The values each function the program defines can return.
    , progParams  :: Map String [Maybe [Range]]
    -- ^ The values each parameter of a static function can have, for the
    -- functions the program only calls directly.
    , progReaches :: Map String [Reach]
    -- ^ How far each function the program defines can reach into the memory
    -- its pointer parameters point to.
    }
    deriving (Eq)

emptyProgram :: Program
emptyProgram = Program Map.empty Map.empty Map.empty Map.empty

-- | Where a function is analysed. The locals it can track are its parameters
-- and the locals it declares once, if it never takes their address. The
-- translation unit tells static functions apart.
data Scope = Scope
    { trackable :: Set String
    , scopeUnit :: FilePath
    }

pathOf :: Scope -> CExpr -> Maybe AccessPath
pathOf scope = \case
    CVar i _ | identToString i `Set.member` trackable scope -> Just (PathVar (identToString i))
    CMember e f True _ -> PathField . PathDeref <$> basePath e <*> pure (identToString f)
    CMember e f False _ -> PathField <$> pathOf scope e <*> pure (identToString f)
    CIndex e (CConst (CIntConst k _)) _ -> PathIndex <$> basePath e <*> pure (show (getCInteger k))
    _ -> Nothing
  where
    basePath = \case
        CVar i _ -> Just (PathVar (identToString i))
        e        -> pathOf scope e

-- | Whether the memory a path names can change behind the function's back.
isMemory :: AccessPath -> Bool
isMemory = \case
    PathVar _     -> False
    PathField p _ -> isMemory p
    _             -> True

constValue :: MonadTrav m => CExpr -> m (Maybe Integer)
constValue e = (intValue <$> constEval x86_64 Map.empty e) `catchTravError` const (return Nothing)

-- | The value of a constant expression, as far as it can be found without
-- evaluating every subexpression again.
literal :: MonadTrav m => CExpr -> m (Maybe Integer)
literal = \case
    CConst (CIntConst i _) -> return (Just (getCInteger i))
    e@(CConst CCharConst{}) -> constValue e
    CUnary CMinOp e _ -> fmap negate <$> literal e
    CUnary CPlusOp e _ -> literal e
    e@CSizeofExpr{} -> constValue e
    e@CSizeofType{} -> constValue e
    e@CAlignofExpr{} -> constValue e
    e@CAlignofType{} -> constValue e
    CVar i _ -> (lookupObject i `catchTravError` const (return Nothing)) >>= \case
        Just (EnumeratorDef (Enumerator _ v _ _)) -> constValue v
        Just d | identToString i `elem` limits -> return (hi . tyRange <$> intType (declType d))
        _ -> return Nothing
    _ -> return Nothing
  where
    -- The maximum values of the integer types, which tokstyle's own headers
    -- declare as objects of those types.
    limits =
        [ "UINT8_MAX", "INT8_MAX", "UINT16_MAX", "INT16_MAX"
        , "UINT32_MAX", "INT32_MAX", "UINT64_MAX", "INT64_MAX" ]

term :: MonadTrav m => Scope -> CExpr -> m Term
term scope e = literal e >>= \case
    Just v -> do
        ty <- (>>= intType) <$> exprType e
        return (Term e ty Nothing Nothing (TConst v))
    Nothing -> do
        k <- kind
        ty <- typeOf k
        fact <- case (ty, e) of
            (Just _, CMember{}) -> fmap FieldFact <$> fieldKey e
            (_, CCall (CVar f _) _ _) -> fmap CallFact <$> funKey (scopeUnit scope) f
            _ -> return Nothing
        return . Term e ty fact Nothing $ case (pathOf scope e, ty) of
            (Just p, Just _) -> TLoad p
            _                -> k
  where
    sub = term scope
    kind = case e of
        CBinary op a b _ -> TBin op <$> sub a <*> sub b
        CUnary op a _    -> TUnary op <$> sub a
        CCast _ a _      -> TCast <$> sub a
        CCond c t f _    -> TCond <$> sub c <*> traverse sub t <*> sub f
        CAssign op l r _ -> TAssign (pathOf scope l) op <$> sub l <*> sub r
        CIndex a i _     -> TIndex <$> sub a <*> sub i <*> arrayLength a
        CCall f args _   -> TCall <$> sub f <*> mapM sub args
        CComma es _      -> TOther <$> mapM sub es
        CMember a _ _ _  -> TOther . pure <$> sub a
        CStatExpr{}      -> return TUnknown
        _                -> return (TOther [])

    -- The integer type of the expression, from the types of its operands
    -- where C's conversions give it, and from its named declarations.
    typeOf k = case (e, k) of
        (CBinary op _ _ _, TBin _ a b)
            | Just x <- termTy a, Just y <- termTy b -> return (Just (binaryType op x y))
        (CUnary op _ _, TUnary _ a)
            | op `elem` [CMinOp, CPlusOp, CCompOp], Just x <- termTy a -> return (Just (promote x))
            | op `elem` [CPreIncOp, CPreDecOp, CPostIncOp, CPostDecOp] -> return (termTy a)
            | op == CNegOp -> return (Just int)
            | op == CAdrOp -> return Nothing
        (CCast d _ _, _) -> (>>= intType) <$> declaredType d
        (CCond{}, TCond _ (Just t) f)
            | Just x <- termTy t, Just y <- termTy f -> return (Just (usual (promote x) (promote y)))
        (CAssign{}, TAssign _ _ l _) -> return (termTy l)
        (CComma{}, TOther ts@(_ : _)) -> return (termTy (last ts))
        (CVar i _, _) -> objectType i
        (CCall (CVar f _) _ _, _) -> declTypeOf f >>= \case
            Just t | Just ft <- funType t -> return (intType (resultType ft))
            _                             -> fallback
        _ -> fallback

    fallback = (>>= intType) <$> exprType e

    objectType i = (>>= intType) <$> declTypeOf i

    declTypeOf i = (fmap declType <$> lookupObject i) `catchTravError` const (return Nothing)

    resultType = \case
        FunType rt _ _       -> rt
        FunTypeIncomplete rt -> rt

    declaredType d = (Just <$> analyseTypeDecl d) `catchTravError` const (return Nothing)

    arrayLength a = exprType a >>= \case
        Just (ArrayType _ (ArraySize _ size) _ _) -> constValue size
        _                                         -> return Nothing

int :: IntTy
int = intTy True 32

-- | Type integer promotion gives a value of a type.
promote :: IntTy -> IntTy
promote t
    | tyBits t < 32 = int
    | otherwise = t

-- | Type the usual arithmetic conversions give two promoted types.
usual :: IntTy -> IntTy -> IntTy
usual a b
    | tySigned a == tySigned b = if tyBits a >= tyBits b then a else b
    | otherwise =
        let (u, sg) = if tySigned a then (b, a) else (a, b)
        in if tyBits u >= tyBits sg then u else sg

binaryType :: CBinaryOp -> IntTy -> IntTy -> IntTy
binaryType op a b
    | op `elem` [CLeOp, CGrOp, CLeqOp, CGeqOp, CEqOp, CNeqOp, CLndOp, CLorOp] = int
    | op `elem` [CShlOp, CShrOp] = promote a
    | otherwise = usual (promote a) (promote b)

subTerms :: Term -> [Term]
subTerms t = t : concatMap subTerms (children t)

-- | Each term within a term, with the facts where it is evaluated. The right
-- operand of @&&@ or @||@ and the branches of @?:@ are only evaluated where
-- what comes before them has the value that leads to them.
subTermsWith :: Facts -> Term -> [(Facts, Term)]
subTermsWith facts t = (facts, t) : case termKind t of
    TBin CLndOp a b -> subTermsWith facts a ++ after True a b
    TBin CLorOp a b -> subTermsWith facts a ++ after False a b
    TCond c m f     -> subTermsWith facts c ++ maybe [] (after True c) m ++ after False c f
    _               -> concatMap (subTermsWith facts) (children t)
  where
    after truth c x = maybe [] (`subTermsWith` x) (assume truth c facts)

------------------------------------------------------------------------------
-- Evaluating terms


-- | The values a term can have, if it is an integer.
eval :: Facts -> Term -> Maybe [Range]
eval facts t = do
    ty <- termTy t
    let unknown = fromMaybe [tyRange ty] (termDefault t)
    return . normalise . concatMap (convert site ty) $ case termKind t of
        TConst v -> [Range v v Plain]
        TLoad p -> maybe unknown snd (Map.lookup p (factsValues facts))
        TBin op a b -> fromMaybe unknown (binary ty op a b)
        TUnary op a -> fromMaybe unknown (unary op a)
        TCast a -> fromMaybe unknown (eval facts a)
        TCond _ m f -> fromMaybe unknown ((++) <$> maybe (Just []) (eval facts) m <*> eval facts f)
        TAssign _ op l r -> fromMaybe unknown (assigned facts site op l r)
        _ -> unknown
  where
    site = annotation (termExpr t)

    binary ty op a b
        | op `elem` [CLeOp, CGrOp, CLeqOp, CGeqOp, CEqOp, CNeqOp, CLndOp, CLorOp] = Just [Range 0 1 Plain]
        | otherwise = do
            ra <- eval facts a
            rb <- eval facts b
            arith facts site ty op ra rb a b

    unary op a = case op of
        CMinOp -> map (\r -> Range (negate (hi r)) (negate (lo r)) (arithOrigin r r)) <$> eval facts a
        CPlusOp -> eval facts a
        CNegOp -> Just [Range 0 1 Plain]
        CPreIncOp -> step 1
        CPreDecOp -> step (-1)
        CPostIncOp -> eval facts a
        CPostDecOp -> eval facts a
        _ -> Nothing
      where step d = map (\r -> Range (lo r + d) (hi r + d) (arithOrigin r r)) <$> eval facts a

arithOrigin :: Range -> Range -> Origin
arithOrigin x y
    | isWrapped (origin x) = origin x
    | isWrapped (origin y) = origin y
    | Just _ <- wrapSite (origin x) = origin x
    | Just _ <- wrapSite (origin y) = origin y
    | all constant [x, y] = Plain
    | otherwise = Arith
  where
    constant r = lo r == hi r && origin r == Plain

-- | The ranges arithmetic produces, before converting them to its type.
arith :: Facts -> NodeInfo -> IntTy -> CBinaryOp -> [Range] -> [Range] -> Term -> Term -> Maybe [Range]
arith facts site ty op ra rb a b = case op of
    CAddOp -> Just [ Range (lo x + lo y) (hi x + hi y) (arithOrigin x y) | x <- ra, y <- rb ]
    CSubOp -> Just (difference [ Range (lo x - hi y) (hi x - lo y) (arithOrigin x y) | x <- ra, y <- rb ])
    CMulOp -> Just [ products x y | x <- ra, y <- rb ]
    CRmdOp -> divisor >>= \n ->
        if n > 0 && all ((>= 0) . lo) ra'
        then Just [Range 0 (min (n - 1) (maximum (map hi ra'))) Reduced]
        else Nothing
    CDivOp -> divisor >>= \n ->
        if n > 0
        then Just [ Range (lo x `quot` n) (hi x `quot` n) (origin x) | x <- ra' ]
        else Nothing
    CShrOp -> divisor >>= \n ->
        if n >= 0 && n < tyBits ty && all ((>= 0) . lo) ra'
        then Just [ Range (lo x `div` (2 ^ n)) (hi x `div` (2 ^ n)) (origin x) | x <- ra' ]
        else Nothing
    CAndOp -> case [ v | [Range v v' _] <- [ra, rb], v == v', v >= 0 ] of
        v : _ -> Just [Range 0 v Reduced]
        []    -> Nothing
    _ -> Nothing
  where
    -- What is known about how the paths in the operands relate gives bounds
    -- on their difference.
    difference rs = case (linear facts a, linear facts b) of
        (Just (Just p, ka), Just (Just q, kb)) ->
            let lower = maybe (lo (hull rs)) (\c -> ka - kb - c) (Map.lookup (q, p) (factsDiffs facts))
                upper = maybe (hi (hull rs)) (\c -> ka - kb + c) (Map.lookup (p, q) (factsDiffs facts))
                clipped = clipRanges lower upper rs
            in if null clipped then rs else clipped
        _ -> rs

    -- Division and shifts work on values converted to the result type.
    ra' = concatMap (convert site ty) ra
    divisor = case termKind b of
        TConst v -> Just v
        _        -> case eval facts b of
            Just [Range v v' _] | v == v' -> Just v
            _                             -> Nothing
    products x y =
        let ps = [ p * q | p <- [lo x, hi x], q <- [lo y, hi y] ]
        in Range (minimum ps) (maximum ps) (arithOrigin x y)

-- | The value an assignment at an expression stores.
assigned :: Facts -> NodeInfo -> CAssignOp -> Term -> Term -> Maybe [Range]
assigned facts site op l r = do
    lt <- termTy l
    rr <- eval facts r
    vals <- case compound op of
        Nothing -> Just rr
        Just bop -> do
            rl <- eval facts l
            arith facts site lt bop rl rr l r
    return (concatMap (convert site lt) vals)
  where
    compound = \case
        CAssignOp -> Nothing
        CMulAssOp -> Just CMulOp
        CDivAssOp -> Just CDivOp
        CRmdAssOp -> Just CRmdOp
        CAddAssOp -> Just CAddOp
        CSubAssOp -> Just CSubOp
        CShlAssOp -> Just CShlOp
        CShrAssOp -> Just CShrOp
        CAndAssOp -> Just CAndOp
        CXorAssOp -> Just CXorOp
        COrAssOp  -> Just COrOp

------------------------------------------------------------------------------
-- Effects

-- | Records the values of a path, keeping how it relates to other paths.
narrowPath :: AccessPath -> IntTy -> [Range] -> Facts -> Facts
narrowPath p ty v facts = facts { factsValues = Map.insert p (ty, v) (factsValues facts) }

-- | Records a write of the values to a path.
setPath :: AccessPath -> IntTy -> [Range] -> Facts -> Facts
setPath p ty v facts = narrowPath p ty v (forget p facts)

-- | Records a write that adds a constant to a path, which keeps how it relates
-- to other paths.
shiftPath :: AccessPath -> IntTy -> [Range] -> Integer -> Facts -> Facts
shiftPath p ty v d facts = narrowPath p ty v shifted
  where
    others = forgetWhere (\q -> q /= p && overwrittenBy p q) facts
    shifted = others { factsDiffs = Map.mapWithKey move (factsDiffs others) }
    move (a, b) c
        | a == p = c + d
        | b == p = c - d
        | otherwise = c

-- | Whether writing the first path can change what the second holds: its
-- parts, and memory that may be the same.
overwrittenBy :: AccessPath -> AccessPath -> Bool
overwrittenBy p q = p `isPathPrefixOf` q || isMemory p && isMemory q && mayAlias p q
  where
    mayAlias (PathField _ f) (PathField _ g) = f == g
    mayAlias _ _                             = True

-- | Forgets what is known about a path and what writing it can change.
forget :: AccessPath -> Facts -> Facts
forget p = forgetWhere (overwrittenBy p)

forgetWhere :: (AccessPath -> Bool) -> Facts -> Facts
forgetWhere gone facts = facts
    { factsValues = Map.filterWithKey (\q _ -> not (gone q)) (factsValues facts)
    , factsDiffs = Map.filterWithKey (\(a, b) _ -> not (gone a || gone b)) (factsDiffs facts)
    }

forgetMemory :: Facts -> Facts
forgetMemory = forgetWhere isMemory

forgetAll :: Facts -> Facts
forgetAll = forgetWhere (const True)

-- | Records that @p - q@ is at most a constant, or 'Nothing' if what is known
-- says it can't be.
relate :: AccessPath -> AccessPath -> Integer -> Facts -> Maybe Facts
relate p q c facts
    | p == q = if c >= 0 then Just facts else Nothing
    | Just c' <- Map.lookup (q, p) (factsDiffs facts), c + c' < 0 = Nothing
    | otherwise = Just facts { factsDiffs = Map.insertWith min (p, q) c (factsDiffs facts) }

-- | A term whose value is a path plus a constant, or a constant, computed
-- without wrapping around. A path counts as itself even if its value wrapped.
linear :: Facts -> Term -> Maybe (Maybe AccessPath, Integer)
linear facts t = exactSum facts t >>= \(Linear ps c) -> case Map.toList ps of
    []            -> Just (Nothing, c)
    [(p, (1, _))] -> Just (Just p, c)
    _             -> Nothing

-- | A sum of paths, each times a coefficient, and a constant.
data Linear = Linear
    { linPaths :: Map AccessPath (Integer, Maybe (Integer, Integer))
    -- ^ Each path's coefficient, and the least and most it can be.
    , linConst :: Integer
    }

constantSum :: Integer -> Linear
constantSum = Linear Map.empty

-- | A path as a sum, with the values the facts know it has, or any of its
-- type's.
pathSum :: Facts -> AccessPath -> IntTy -> Linear
pathSum facts p ty = Linear (Map.singleton p (1, valueSpan (maybe [tyRange ty] snd (Map.lookup p (factsValues facts))))) 0

valueSpan :: [Range] -> Maybe (Integer, Integer)
valueSpan = \case
    [] -> Nothing
    rs -> Just (minimum (map lo rs), maximum (map hi rs))

plus :: Linear -> Linear -> Linear
plus (Linear a c) (Linear b d) = Linear (Map.filter ((/= 0) . fst) (Map.unionWith add a b)) (c + d)
  where add (x, r) (y, _) = (x + y, r)

scale :: Integer -> Linear -> Linear
scale k (Linear ps c) = Linear (Map.filter ((/= 0) . fst) (Map.map (\(x, r) -> (k * x, r)) ps)) (k * c)

-- | The product of two sums, if one of them is a constant.
times :: Linear -> Linear -> Maybe Linear
times x y
    | Map.null (linPaths x) = Just (scale (linConst x) y)
    | Map.null (linPaths y) = Just (scale (linConst y) x)
    | otherwise = Nothing

-- | The least and most a sum can be.
extent :: Linear -> Maybe (Integer, Integer)
extent (Linear ps c) = foldM add (c, c) (Map.elems ps)
  where
    add (l, h) (k, r) = (\(pl, ph) -> (l + min (k * pl) (k * ph), h + max (k * pl) (k * ph))) <$> r

-- | A term as a sum, where computing each part doesn't wrap around, as its
-- type holds all the values the paths in it give it.
exactSum :: Facts -> Term -> Maybe Linear
exactSum facts = sumWith facts $ \u s -> case (termTy u, extent s) of
    (Just ty, Just (l, h)) -> lo (tyRange ty) <= l && h <= hi (tyRange ty)
    _                      -> False

-- | A term as a sum, even where computing it can wrap around.
anySum :: Facts -> Term -> Maybe Linear
anySum facts = sumWith facts (\_ _ -> True)

-- | A term as a sum, where each part of it that is arithmetic is one that
-- the check accepts.
sumWith :: Facts -> (Term -> Linear -> Bool) -> Term -> Maybe Linear
sumWith facts accepts = go
  where
    go u = case termKind (stripCasts u) of
        TConst k -> Just (constantSum k)
        TLoad p -> Just (Linear (Map.singleton p (1, valueSpan =<< eval facts u)) 0)
        TBin CAddOp a b -> checked u =<< plus <$> go a <*> go b
        TBin CSubOp a b -> checked u =<< plus <$> go a <*> (scale (-1) <$> go b)
        TBin CMulOp a b -> checked u =<< join (times <$> go a <*> go b)
        _ | Just [Range k k' Plain] <- eval facts u, k == k' -> Just (constantSum k)
        _ -> Nothing
    checked u s = if accepts (stripCasts u) s then Just s else Nothing

-- | Whether the sum of some terms, each times a coefficient, and a constant is
-- always at most a sum, by how the paths they read relate, or by the values
-- of those paths.
atMost :: Facts -> [(Integer, Term)] -> Integer -> Linear -> Bool
atMost facts ts k bound = case foldr plus (constantSum k) <$> mapM part ts of
    Just total -> nonPositive (total `plus` scale (-1) bound)
    Nothing    -> False
  where
    part (c, t) = scale c <$> exactSum facts t
    nonPositive d = case Map.toList (linPaths d) of
        [(p, (a, _)), (q, (b, _))]
            | a == negate b, Just c <- Map.lookup (if a > 0 then (p, q) else (q, p)) (factsDiffs facts) ->
                abs a * c + linConst d <= 0
        _ -> maybe False ((<= 0) . snd) (extent d)

-- | Records how an assigned path relates to the path it was assigned from.
relateAssigned :: Facts -> AccessPath -> IntTy -> Term -> Facts -> Facts
relateAssigned before p ty r after = case (linear before r, eval before r) of
    (Just (Just q, c), Just v) | q /= p, all (fitsIn (Just ty)) v ->
        fromMaybe after (relate p q c after >>= relate q p (negate c))
    _ -> after

clipRanges :: Integer -> Integer -> [Range] -> [Range]
clipRanges a b = mapMaybe $ \x ->
    let x' = Range (max a (lo x)) (min b (hi x)) (origin x)
    in if lo x' <= hi x' then Just x' else Nothing

-- | What evaluating a term does to the facts. Writes that only happen on
-- some paths through the term forget the paths they write.
effects :: Bool -> Term -> Facts -> Facts
effects definite t facts = case termKind t of
    TAssign mp op l r ->
        let after = effects definite l (effects definite r facts)
        in case (mp, termTy l) of
            (Just p, Just ty) | definite, Just v <- assigned facts site op l r ->
                let selfShift = case (op, linear facts r) of
                        (CAssignOp, Just (Just q, c)) | q == p -> Just c
                        (CAddAssOp, Just (Nothing, c))         -> Just c
                        (CSubAssOp, Just (Nothing, c))         -> Just (negate c)
                        _                                      -> Nothing
                in case selfShift of
                    Just c | shifts facts l ty c -> shiftPath p ty v c after
                    _ | op == CAssignOp -> relateAssigned facts p ty r (setPath p ty v after)
                    _ -> setPath p ty v after
            (Just p, _) -> forget p after
            (Nothing, _) -> forgetWrite l after
    TUnary op a | op `elem` [CPreIncOp, CPreDecOp, CPostIncOp, CPostDecOp] ->
        let after = effects definite a facts
            d = if op `elem` [CPreIncOp, CPostIncOp] then 1 else -1
        in case (termKind a, termTy a, eval facts a) of
            (TLoad p, Just ty, Just v) | definite ->
                let v' = normalise (concatMap (convert site ty) [ Range (lo r + d) (hi r + d) (arithOrigin r r) | r <- v ])
                in if shifts facts a ty d then shiftPath p ty v' d after else setPath p ty v' after
            (TLoad p, _, _) -> forget p after
            _ -> forgetWrite a after
    TCall f args ->
        let passed = [ PathVar n | a <- args, Just e <- [passedAddress (termExpr a)], n <- addressed e ]
        in foldr forget (forgetMemory (foldl (flip (effects definite)) (effects definite f facts) args)) passed
    TBin op a b | op `elem` [CLndOp, CLorOp] -> effects False b (effects definite a facts)
    TCond c m f -> foldl (flip (effects False)) (effects definite c facts) (maybe [] pure m ++ [f])
    TUnknown -> forgetAll facts
    _ -> foldl (flip (effects definite)) facts (children t)
  where
    site = annotation (termExpr t)
    -- Adding a constant to a path shifts it if every value it has stays in
    -- its type.
    shifts fs l ty c = maybe False (all (fitsIn (Just ty)) . shift c) (eval fs l)
    forgetWrite l = case termExpr l of
        CMember _ f _ _ -> forgetField (identToString f)
        _               -> forgetMemory

-- | Forgets the field with this name of any object in memory, and memory it
-- may be part of.
forgetField :: String -> Facts -> Facts
forgetField f = forgetWhere $ \q -> isMemory q && case q of
    PathField _ g -> g == f
    _             -> True

children :: Term -> [Term]
children t = case termKind t of
    TBin _ a b      -> [a, b]
    TUnary _ a      -> [a]
    TCast a         -> [a]
    TCond c m f     -> c : maybe [] pure m ++ [f]
    TAssign _ _ l r -> [l, r]
    TIndex a i _    -> [a, i]
    TCall f args    -> f : args
    TOther ts       -> ts
    _               -> []

------------------------------------------------------------------------------
-- Branch conditions

swapOp :: CBinaryOp -> CBinaryOp
swapOp = \case
    CLeOp  -> CGrOp
    CGrOp  -> CLeOp
    CLeqOp -> CGeqOp
    CGeqOp -> CLeqOp
    op     -> op

-- | The comparison that holds when this one doesn't.
negateOp :: CBinaryOp -> CBinaryOp
negateOp = \case
    CLeOp  -> CGeqOp
    CGrOp  -> CLeqOp
    CLeqOp -> CGrOp
    CGeqOp -> CLeOp
    CEqOp  -> CNeqOp
    _      -> CEqOp

joinFacts :: Facts -> Facts -> Facts
joinFacts a b = Facts
    { factsChanges = max (factsChanges a) (factsChanges b)
    , factsBackEdge = False
    , factsValues = Map.intersectionWith (\(ty, x) (_, y) -> (ty, normalise (x ++ y))) (factsValues a) (factsValues b)
    , factsDiffs = Map.intersectionWith max (factsDiffs a) (factsDiffs b)
    }

joinMaybe :: Maybe Facts -> Maybe Facts -> Maybe Facts
joinMaybe (Just a) (Just b) = Just (joinFacts a b)
joinMaybe a Nothing         = a
joinMaybe Nothing b         = b

-- | The facts on the paths where a condition has the given truth value, or
-- 'Nothing' if it can't have it.
assume :: Bool -> Term -> Facts -> Maybe Facts
assume truth t facts = case termKind t of
    TUnary CNegOp a -> assume (not truth) a facts
    TBin CLndOp a b
        | truth -> assume True a facts >>= assume True b
        | otherwise -> assume False a facts `joinMaybe` (assume True a facts >>= assume False b)
    TBin CLorOp a b
        | truth -> assume True a facts `joinMaybe` (assume False a facts >>= assume True b)
        | otherwise -> assume False a facts >>= assume False b
    TBin op a b | op `elem` [CLeOp, CGrOp, CLeqOp, CGeqOp, CEqOp, CNeqOp] ->
        let op' = if truth then op else negateOp op
        in compareFacts op' a b facts
    TCast a | Just v <- eval facts a, all (fitsIn (termTy t)) v -> assume truth a facts
    TLoad _ -> compareFacts (if truth then CNeqOp else CEqOp) t zero facts
    _ -> Just facts
  where
    zero = Term (termExpr t) (termTy t) Nothing Nothing (TConst 0)

-- | Narrows the paths each side of a comparison reads, given that it holds,
-- and records how the sides relate.
compareFacts :: CBinaryOp -> Term -> Term -> Facts -> Maybe Facts
compareFacts op l r facts = case (eval facts l, eval facts r) of
    (Just lv@(_ : _), Just rv@(_ : _)) | exact lv rv ->
        narrowSide op l lv rv facts >>= narrowSide (swapOp op) r rv lv >>= relateSides
    _ -> Just facts
  where
    -- The comparison is between the values themselves, rather than values
    -- converted to an unsigned type, if neither side can be negative or the
    -- conversions C makes before comparing give a signed type.
    exact lv rv = all ((>= 0) . lo) lv && all ((>= 0) . lo) rv
        || maybe False tySigned (usual <$> (promote <$> termTy l) <*> (promote <$> termTy r))

    relateSides f = case (linear facts l, linear facts r) of
        (Just (Just p, a), Just (Just q, b)) -> case op of
            CLeOp  -> relate p q (b - a - 1) f
            CLeqOp -> relate p q (b - a) f
            CGrOp  -> relate q p (a - b - 1) f
            CGeqOp -> relate q p (a - b) f
            CEqOp  -> relate p q (b - a) f >>= relate q p (a - b)
            _      -> Just f
        _ -> Just f

-- | Narrows the path one side of a comparison reads, given that the
-- comparison holds and the values of both sides, or 'Nothing' if it can't
-- hold.
narrowSide :: CBinaryOp -> Term -> [Range] -> [Range] -> Facts -> Maybe Facts
narrowSide op t tv ov facts = case (termKind (stripCasts t), termTy (stripCasts t)) of
    (TLoad p, Just ty) | not (null narrowed) -> Just (narrowPath p ty narrowed facts)
                       | otherwise -> Nothing
      where narrowed = narrow ty
    _ | Just (l, k) <- offsetLoad facts t, k /= 0 ->
        narrowSide op l (shift (-k) tv) (shift (-k) ov) facts
    _ | TCast a <- termKind t, Just av <- eval facts a, all (fitsIn (termTy t)) av ->
        narrowSide op a tv ov facts
    _ | op == CEqOp, null (clipRanges ol oh tv) -> Nothing
      | otherwise -> Just facts
  where
    Range ol oh _ = hull ov
    narrow ty = case op of
        CLeOp  -> clipRanges (lo (tyRange ty)) (oh - 1) tv
        CLeqOp -> clipRanges (lo (tyRange ty)) oh tv
        CGrOp  -> clipRanges (ol + 1) (hi (tyRange ty)) tv
        CGeqOp -> clipRanges ol (hi (tyRange ty)) tv
        CEqOp  -> clipRanges ol oh tv
        _ | ol == oh -> concatMap (remove ol) tv
          | otherwise -> tv
    remove v x
        | v < lo x || v > hi x = [x]
        | otherwise = [ y | y <- [Range (lo x) (v - 1) (origin x), Range (v + 1) (hi x) (origin x)], lo y <= hi y ]

fitsIn :: Maybe IntTy -> Range -> Bool
fitsIn ty r = maybe False (\t -> lo r >= lo (tyRange t) && hi r <= hi (tyRange t)) ty

-- | The path a term loads and the constant it adds to it, if its value is
-- that sum, computed without wrapping around.
offsetLoad :: Facts -> Term -> Maybe (Term, Integer)
offsetLoad facts t = case linear facts t of
    Just (Just p, k) -> (\l -> (l, k)) <$> listToMaybe [ u | u <- subTerms t, TLoad q <- [termKind u], q == p ]
    _                -> Nothing

shift :: Integer -> [Range] -> [Range]
shift d = map (\r -> r { lo = lo r + d, hi = hi r + d })

-- | The term a value-preserving cast converts: one to a type that can hold
-- every value of the type it converts from.
stripCasts :: Term -> Term
stripCasts t = case termKind t of
    TCast a | Just from <- termTy a, Just to <- termTy t
            , lo (tyRange to) <= lo (tyRange from), hi (tyRange from) <= hi (tyRange to) -> stripCasts a
    _ -> t

------------------------------------------------------------------------------
-- The analysis

-- | How many times the facts at a loop head can change before ranges that keep
-- growing widen to the next bound the function compares them against, or to
-- their type's range.
roundsBeforeWidening :: Int
roundsBeforeWidening = 3

-- | Joins the facts from two paths, widening at loop heads to the bounds each
-- path is compared against.
mergeFacts :: Map AccessPath (Set Integer) -> Maybe Facts -> Maybe Facts -> Maybe Facts
mergeFacts bounds (Just old) (Just new)
    | joined == old = Just old
    | factsBackEdge new && factsChanges old >= roundsBeforeWidening = Just (widen bounds old joined)
    | otherwise = Just joined { factsChanges = factsChanges old + 1 }
  where
    joined = (joinFacts old new) { factsChanges = factsChanges old }
mergeFacts _ a b = joinMaybe a b

widen :: Map AccessPath (Set Integer) -> Facts -> Facts -> Facts
widen bounds old joined = joined
    { factsChanges = factsChanges old + 1
    , factsValues = Map.mapWithKey grow (factsValues joined)
    , factsDiffs = Map.filterWithKey steady (factsDiffs joined)
    }
  where
    steady k c = maybe False (>= c) (Map.lookup k (factsDiffs old))
    grow p (ty, v) = case Map.lookup p (factsValues old) of
        Just (_, ov) | v == ov -> (ty, v)
        Just (_, ov) ->
            let Range nl nh o = hull v
                Range ol oh _ = hull ov
                ks = Map.findWithDefault Set.empty p bounds
                l | nl < ol = maybe (lo (tyRange ty)) (max (lo (tyRange ty))) (Set.lookupLE nl ks)
                  | otherwise = nl
                h | nh > oh = maybe (hi (tyRange ty)) (min (hi (tyRange ty))) (Set.lookupGE nh ks)
                  | otherwise = nh
            in (ty, [Range l h o])
        Nothing -> (ty, v)

paramNames :: FunDef -> [String]
paramNames (FunDef (VarDecl _ _ ty) _ _) = case ty of
    FunctionType (FunType _ params _) _ ->
        [ identToString i | p <- params, VarName i _ <- [paramVarName p] ]
    _ -> []

paramVarName :: ParamDecl -> VarName
paramVarName = \case
    ParamDecl (VarDecl n _ _) _         -> n
    AbstractParamDecl (VarDecl n _ _) _ -> n

-- | The locals a function declares, once for each declaration.
declaredNames :: CStat -> [String]
declaredNames body =
    [ identToString i | CDecl _ ds _ <- localDecls body, (Just (CDeclr (Just i) _ _ _ _), _, _) <- ds ]

-- | The locals a function declares @static@ or @extern@, whose values outlive
-- a call.
storedElsewhere :: CStat -> Set String
storedElsewhere body = Set.fromList
    [ identToString i
    | CDecl specs ds _ <- localDecls body, any isStorage specs
    , (Just (CDeclr (Just i) _ _ _ _), _, _) <- ds ]
  where
    isStorage = \case
        CStorageSpec CStatic{} -> True
        CStorageSpec CExtern{} -> True
        _                      -> False

-- | The names whose address a statement takes, other than to pass it straight
-- to a call, which only changes them in the call.
addressTaken :: CStat -> Set String
addressTaken = Set.fromList . everything
  where
    everything s = concatMap exprAddrs (statExprs s)
    exprAddrs = \case
        CUnary CAdrOp e _ -> addressed e ++ exprAddrs e
        CVar{}            -> []
        CCall f args _    -> exprAddrs f ++ concatMap argAddrs args
        e                 -> concatMap exprAddrs (exprChildren e)
    argAddrs a = case passedAddress a of
        Just e  -> exprAddrs e
        Nothing -> exprAddrs a

-- | The expression whose address an argument passes.
passedAddress :: CExpr -> Maybe CExpr
passedAddress = \case
    CCast _ e _       -> passedAddress e
    CUnary CAdrOp e _ -> Just e
    _                 -> Nothing

-- | The variable whose memory an expression is in, if any.
addressed :: CExpr -> [String]
addressed = \case
    CVar i _        -> [identToString i]
    CMember e _ _ _ -> addressed e
    CIndex e _ _    -> addressed e
    _               -> []

-- | Whether a function does anything that can make a value wrap around below
-- zero: subtract, decrement or negate. Values from the rest of the program
-- never come wrapped, so only these can.
canWrap :: FunDef -> Bool
canWrap (FunDef _ body _) = any (any wraps . everyExpr) (statExprs body)
  where
    everyExpr e = e : concatMap everyExpr (exprChildren e)
    wraps = \case
        CBinary CSubOp _ _ _    -> True
        CAssign CSubAssOp _ _ _ -> True
        CUnary CPreDecOp _ _    -> True
        CUnary CPostDecOp _ _   -> True
        CUnary CMinOp _ _       -> True
        _                       -> False

nodeTermsOf :: MonadTrav m => Scope -> Node -> m NodeTerms
nodeTermsOf scope (Node _ kind) = case kind of
    StatNode (CExpr e _) -> Evaluates <$> mapM (term scope) (maybe [] pure e)
    StatNode (CReturn e _) -> Evaluates <$> mapM (term scope) (maybe [] pure e)
    StatNode CBreak{} -> return (Evaluates [])
    StatNode CCont{} -> return (Evaluates [])
    StatNode CGoto{} -> return (Evaluates [])
    StatNode CFor{} -> return (Evaluates [])
    StatNode _ -> return Opaque
    ExprNode e -> Evaluates . pure <$> term scope e
    BranchNode e -> Evaluates . pure <$> term scope e
    DeclNode (CDecl _ ds _) -> do
        items <- forM ds $ \case
            (Just (CDeclr (Just i) derived _ _ ni), Just (CInitExpr e _), _) -> do
                ty <- (>>= intType) <$> exprType (CVar i ni)
                sizes <- arraySizes derived
                t <- term scope e
                let path = if identToString i `Set.member` trackable scope then Just (PathVar (identToString i)) else Nothing
                    var = Term (CVar i ni) ty Nothing Nothing (maybe (TOther []) TLoad path)
                    assign = Term (CAssign CAssignOp (CVar i ni) e ni) ty Nothing Nothing (TAssign path CAssignOp var t)
                return ( [ (p, vt, Just t) | Just p <- [path], Just vt <- [ty] ], sizes ++ [t]
                       , sizes ++ [maybe t (const assign) ty] )
            (Just (CDeclr (Just i) derived _ _ ni), ini, _) -> do
                ty <- (>>= intType) <$> exprType (CVar i ni)
                sizes <- arraySizes derived
                ts <- mapM (term scope) (maybe [] initExprs ini)
                return ( [ (PathVar (identToString i), vt, Nothing) | Just vt <- [ty], identToString i `Set.member` trackable scope ]
                       , sizes ++ ts, sizes ++ ts )
            (_, ini, _) -> do
                ts <- mapM (term scope) (maybe [] initExprs ini)
                return ([], ts, ts)
        return $ Declares (concat [ v | (v, _, _) <- items ]) (concat [ e | (_, e, _) <- items ])
            (concat [ c | (_, _, c) <- items ])
    DeclNode _ -> return Opaque
    EntryNode -> return (Evaluates [])
    ExitNode -> return (Evaluates [])
  where
    -- A variable length array's declaration evaluates its size.
    arraySizes derived = mapM (term scope) [ e | CArrDeclr _ (CArrSize _ e) _ <- derived ]

mapTerms :: (Term -> Term) -> NodeTerms -> NodeTerms
mapTerms f = \case
    Evaluates ts           -> Evaluates (map f ts)
    Declares vars inits ts -> Declares [ (p, ty, f <$> t) | (p, ty, t) <- vars ] (map f inits) (map f ts)
    Opaque                 -> Opaque

termsOf :: NodeTerms -> [Term]
termsOf = \case
    Evaluates ts    -> ts
    Declares _ _ ts -> ts
    Opaque          -> []

transferTerms :: NodeTerms -> Facts -> Facts
transferTerms nt facts = case nt of
    Evaluates ts -> foldl (flip (effects True)) facts ts
    Declares vars inits _ ->
        let after = foldl (flip (effects True)) facts inits
        in foldl (\f (p, ty, ini) -> case ini >>= eval facts of
                     Just v  -> setPath p ty (normalise (concatMap (convert (initSite ini) ty) v)) f
                     Nothing -> forget p f) after vars
    Opaque -> forgetAll facts
  where
    initSite = maybe undefNode (annotation . termExpr)

-- | A function as the analysis sees it: its control flow graph and the terms
-- each node evaluates.
data Function = Function
    { fnEntry  :: Node
    , fnCfg    :: CFG
    , fnTerms  :: Map Node NodeTerms
    , fnCases  :: Map Name Integer
    -- ^ The value of each case label.
    , fnParams :: [Maybe (AccessPath, IntTy)]
    -- ^ The paths of the parameters the analysis tracks.
    , fnSelf   :: Maybe String
    }

-- | A function in a translation unit, given the declarations of its locals.
translate :: MonadTrav m => FilePath -> [IdentDecl] -> FunDef -> m Function
translate unit locals fd@(FunDef decl _ ni) = do
    enterFunctionScope
    defineParams ni decl
    enterBlockScope
    forM_ locals $ \d -> withDefTable $ \dt -> ((), snd (defineScopedIdent (declIdent d) d dt))
    result <- translateBody unit fd
    leaveBlockScope
    leaveFunctionScope
    return result

translateBody :: MonadTrav m => FilePath -> FunDef -> m Function
translateBody unit fd@(FunDef (VarDecl self _ ty) body _) = do
    let (entry, cfg) = fromFunDef fd
        nodes = Set.toList . Set.fromList $ Map.keys cfg ++ [ n | es <- Map.elems cfg, (_, n) <- es ]
        declared = paramNames fd ++ declaredNames body
        counts = Map.fromListWith (+) [ (n, 1 :: Int) | n <- declared ]
        scope = Scope
            { trackable = Map.keysSet (Map.filter (== 1) counts)
                `Set.difference` (addressTaken body <> storedElsewhere body)
            , scopeUnit = unit
            }
    terms <- Map.fromList <$> forM nodes (\n -> (,) n <$> nodeTermsOf scope n)
    selfKey <- case self of
        VarName i _ -> funKey unit i
        NoName      -> return Nothing
    caseValues <- caseConstants cfg
    return Function
        { fnEntry = entry
        , fnCfg = cfg
        , fnTerms = terms
        , fnCases = caseValues
        , fnParams =
            [ case (paramVarName p, intType (declType p)) of
                (VarName i _, Just t) | identToString i `Set.member` trackable scope -> Just (PathVar (identToString i), t)
                _ -> Nothing
            | FunctionType (FunType _ params _) _ <- [ty], p <- params ]
        , fnSelf = selfKey
        }
  where
    caseConstants :: MonadTrav m => CFG -> m (Map Name Integer)
    caseConstants cfg = fmap (Map.fromList . concat) . forM [ v | es <- Map.elems cfg, (CaseBranch v, _) <- es ] $ \v ->
        constValue v >>= \k -> return [ (n, x) | Just n <- [nameOfNode (annotation v)], Just x <- [k] ]

-- | Whether what is known about the program says more about a function than
-- knowing nothing about it.
dependsOn :: Program -> Function -> Bool
dependsOn prog fn = maybe False (`Map.member` progParams prog) (fnSelf fn)
    || any (maybe False known . termFact) (concatMap (concatMap subTerms . termsOf) (Map.elems (fnTerms fn)))
  where
    known = \case
        FieldFact k -> k `Map.member` progFields prog
        CallFact k  -> k `Map.member` progReturns prog

-- | A term with its defaults from what is known about the program.
withProgram :: Program -> Term -> Term
withProgram prog = go
  where
    go t = t { termDefault = termFact t >>= known, termKind = kind (termKind t) }
    kind = \case
        TBin op a b      -> TBin op (go a) (go b)
        TUnary op a      -> TUnary op (go a)
        TCast a          -> TCast (go a)
        TCond c m f      -> TCond (go c) (go <$> m) (go f)
        TAssign p op l r -> TAssign p op (go l) (go r)
        TIndex a i n     -> TIndex (go a) (go i) n
        TCall f args     -> TCall (go f) (map go args)
        TOther ts        -> TOther (map go ts)
        k                -> k
    known = \case
        FieldFact k -> Map.lookup k (progFields prog)
        CallFact k  -> Map.lookup k (progReturns prog)

-- | The facts before each reachable node of a function, with the terms the
-- node evaluates, given what is known about the program.
solveFunction :: Program -> Function -> [(Node, Facts, [Term])]
solveFunction prog fn = [ (n, facts, termsOf (termsAt n)) | (n, Just facts) <- Map.toList result ]
  where
    entry = fnEntry fn
    cfg = fnCfg fn
    terms = Map.map (mapTerms (withProgram prog)) (fnTerms fn)
    entryFacts = paramFacts (fnParams fn) (fnSelf fn >>= (`Map.lookup` progParams prog))
    termsAt n = Map.findWithDefault (Evaluates []) n terms
    -- The constants branch conditions compare paths against, shifted by what
    -- they add to the paths, and their neighbours.
    bounds = Map.fromListWith Set.union
        [ (p, Set.fromList [ k - c + d | d <- [-1, 0, 1] ])
        | (Node _ BranchNode{}, nt) <- Map.toList terms, t <- concatMap subTerms (termsOf nt)
        , TBin op a b <- [termKind t], op `elem` [CLeOp, CGrOp, CLeqOp, CGeqOp, CEqOp, CNeqOp]
        , (x, y) <- [(a, b), (b, a)]
        , Just (Just p, c) <- [linear nothingKnown x]
        , Just (Nothing, k) <- [linear nothingKnown y] ]
    nothingKnown = Facts 0 False Map.empty Map.empty
    condOf n = case termsAt n of
        Evaluates [c] | Node _ BranchNode{} <- n -> Just c
        _                                        -> Nothing
    back = backEdges entry cfg
    isBack n e = or [ (n, v) `Set.member` back | (e', v) <- Map.findWithDefault [] n cfg, sameEdge e e' ]
    edgeTransferNode n e s = (\f -> f { factsBackEdge = isBack n e }) <$> (s >>= \facts -> case (condOf n, e) of
        (Just c, TrueBranch) -> assume True c facts
        (Just c, FalseBranch) -> assume False c facts
        (Just c, CaseBranch v) | Just k <- caseValue v ->
            compareFacts CEqOp c (Term v (termTy c) Nothing Nothing (TConst k)) facts
        (Just c, DefaultBranch) ->
            foldM (\f (v, k) -> compareFacts CNeqOp c (Term v (termTy c) Nothing Nothing (TConst k)) f) facts
                [ (v, k) | (CaseBranch v, _) <- Map.findWithDefault [] n cfg, Just k <- [caseValue v] ]
        _ -> Just facts)
      where caseValue v = nameOfNode (annotation v) >>= (`Map.lookup` fnCases fn)
    problem = Dataflow
        { transfer     = \n s -> if endsPath n then Nothing else (\f -> (transferTerms (termsAt n) f) { factsBackEdge = False }) <$> s
        , edgeTransfer = edgeTransferNode
        , merge        = mergeFacts bounds
        , initial      = Just entryFacts
        }
    result = solve entry cfg problem

sameEdge :: EdgeType -> EdgeType -> Bool
sameEdge (CaseBranch a) (CaseBranch b) = nameOfNode (annotation a) == nameOfNode (annotation b)
sameEdge a b                           = a == b

-- | The edges that go back to a node the depth-first search from the entry is
-- still inside. Every cycle in the graph has one.
backEdges :: Node -> CFG -> Set (Node, Node)
backEdges entry cfg = snd (visit Set.empty entry (Set.empty, Set.empty))
  where
    visit stack n acc@(done, back)
        | n `Set.member` done = acc
        | otherwise =
            let stack' = Set.insert n stack
                step acc'@(d, b) (_, m)
                    | m `Set.member` stack' = (d, Set.insert (n, m) b)
                    | otherwise = visit stack' m acc'
                (done', back') = foldl step (Set.insert n done, back) (Map.findWithDefault [] n cfg)
            in (done', back')

-- | The facts at the entry of a function whose parameters are known to have
-- the given values.
paramFacts :: [Maybe (AccessPath, IntTy)] -> Maybe [Maybe [Range]] -> Facts
paramFacts params known = Facts 0 False values Map.empty
  where
    values = Map.fromList [ (p, (t, v)) | (Just (p, t), Just v) <- zip params (fromMaybe [] known) ]
