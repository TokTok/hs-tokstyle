{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict     #-}
-- | Facts about a whole program, found by analysing each of its translation
-- units without knowing anything about the rest, and joining what they show.
--
-- A field's values are the values the program stores in it, as long as it
-- only changes the field in ways the analysis sees. A function's results are
-- the values it returns, and a static function's parameters are the values its
-- direct calls pass, as long as nothing takes its address.
module Tokstyle.C.Analysis.Program
    ( Observed
    , Analysed
    , observe
    , programFrom
    ) where

import           Control.Monad                    (forM)
import           Data.List                        (isPrefixOf)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (catMaybes, fromMaybe, isJust,
                                                   isNothing, maybeToList)
import qualified Data.Set                         as Set
import           Language.C.Analysis.DeclAnalysis (analyseTypeDecl)
import           Language.C.Analysis.SemRep       (CompType (..), FunDef (..),
                                                   FunType (..),
                                                   GlobalDecls (..),
                                                   IdentDecl (..), ObjDef (..),
                                                   TagDef (..), Type (..),
                                                   TypeName (..), VarDecl (..),
                                                   VarName (..), constant,
                                                   declType)
import           Language.C.Analysis.TravMonad    (MonadTrav, catchTravError,
                                                   lookupObject)
import           Language.C.Analysis.TypeUtils    (canonicalType, typeQuals)
import           Language.C.Data.Ident            (SUERef (..), identToString)
import           Language.C.Data.Node             (posOfNode, undefNode)
import           Language.C.Data.Position         (isSourcePos, posFile)
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants      (getCInteger)
import           Tokstyle.C.Analysis.CFG          (Node (..), NodeKind (..))
import           Tokstyle.C.Analysis.Interval     (Facts, Function, IntTy (..),
                                                   Origin (..), Program (..),
                                                   Range (..), Term (..),
                                                   TermKind (..), canWrap,
                                                   convert, emptyProgram, eval,
                                                   intType, normalise,
                                                   solveFunction, subTerms,
                                                   translate)
import           Tokstyle.C.Analysis.Syntax       (exprChildren, initExprs,
                                                   localDecls, statExprs)
import           Tokstyle.C.Analysis.Types        (FieldKey, exprType, fieldKey,
                                                   funKey, funType, structTag)


-- | Values seen for a field, a result or a parameter, with their type if
-- known.
type Values = (Maybe IntTy, [Range])

-- | Something a translation unit shows about the program.
data Observation
    = Stored FieldKey Values
    -- ^ Values stored in a field.
    | Escaped FieldKey
    -- ^ A field changed in ways the analysis doesn't see.
    | EscapedTag String
    -- ^ A struct whose fields change in ways the analysis doesn't see.
    | HandedTo String [String]
    -- ^ Structs a function gets a pointer to that it may write through, or
    -- returns. Unless the program defines the function, their fields change
    -- in ways the analysis doesn't see.
    | Returned String Values
    | Passed String [Maybe Values]
    -- ^ The values a static function gets for each parameter.
    | Addressed String
    -- ^ A function used other than by calling it.
    | Defined String

-- | Everything a translation unit shows about the program.
type Observed = [Observation]

-- | A function, with the facts before each of its nodes when nothing is known
-- about the rest of the program.
type Analysed = (Function, [(Node, Facts, [Term])])

-- | External functions that don't change the objects their pointers point to
-- in ways the analysis needs to see.
harmless :: [String]
harmless = ["free", "memset", "memcpy", "memmove", "crypto_memzero", "sodium_memzero"]

-- | Forgets where values came from: facts about the program carry bounds and
-- which values are reduced indices, not which ones wrapped around.
plain :: [Range] -> [Range]
plain = map $ \r -> if origin r == Reduced then r else r { origin = Plain }

-- | The facts about the program its translation units show. Facts that say no
-- more than a value's type are left out.
programFrom :: Observed -> Program
programFrom obs = Program
    { progFields = Map.fromList
        [ (k, v)
        | (k@(tag, _), (ty, vs)) <- Map.toList (Map.fromListWith joinValues [ (k', v) | Stored k' v <- obs ])
        , tag `Set.notMember` escapedTags
        , k `Set.notMember` escaped
        , let v = invariant vs
        , not (trivial ty v)
        ]
    , progReturns = Map.fromList
        [ (k, v)
        | (k, (ty, vs)) <- Map.toList (Map.fromListWith joinValues [ (k', v) | Returned k' v <- obs ])
        , k `Set.member` defined
        , let v = normalise vs
        , not (trivial ty v)
        ]
    , progParams = Map.fromList
        [ (k, ps)
        | (k, args) <- Map.toList (Map.fromListWith (zipWith (\x y -> joinValues <$> x <*> y)) [ (k', a) | Passed k' a <- obs ])
        , k `Set.member` defined, k `Set.notMember` addressed
        , let ps = map known args
        , any isJust ps
        ]
    }
  where
    defined = Set.fromList [ f | Defined f <- obs ]
    addressed = Set.fromList [ f | Addressed f <- obs ]
    escaped = Set.fromList [ k | Escaped k <- obs ]
    escapedTags = Set.fromList $ [ tag | EscapedTag tag <- obs ]
        ++ [ tag | HandedTo f tags <- obs, f `Set.notMember` defined, tag <- tags ]

    joinValues (t1, a) (t2, b) = (maybe t2 Just t1, a ++ b)

    known = \case
        Just (ty, vs) | let v = normalise vs, not (trivial ty v) -> Just v
        _                                                        -> Nothing

    trivial ty v = case (ty, v) of
        (Just t, [Range l h Plain]) -> l <= lo (tyRange t) && h >= hi (tyRange t)
        _                           -> False

    -- Zero is always among a field's values: memory starts out zeroed. A
    -- constant within an index range keeps it an index range.
    invariant vs =
        let vals = Range 0 0 Plain : vs
            reduced = [ r | r <- vals, origin r == Reduced ]
            absorbed r = lo r == hi r && origin r == Plain
                && any (\x -> lo x <= lo r && hi r <= hi x) reduced
        in normalise (filter (not . absorbed) vals)

-- | What a translation unit shows about the program, and the analysis of its
-- functions that can wrap around.
observe :: MonadTrav m => FilePath -> Map String [IdentDecl] -> GlobalDecls -> m (Observed, [Analysed])
observe unit locals decls = do
    perFunction <- forM [ (i, fd) | FunctionDef fd@(FunDef (VarDecl (VarName i _) _ _) _ _) <- objs ] $ \(i, fd) -> do
        (o, analysed) <- observeFunction unit (Map.findWithDefault [] (identToString i) locals) fd
        return (o, [ analysed | canWrap fd ])
    perObject <- forM [ od | ObjectDef od <- objs ] (observeObject unit)
    return ( concatMap fst perFunction ++ concat perObject ++ map EscapedTag (Set.toList externalTags)
           , concatMap snd perFunction )
  where
    objs = Map.elems (gObjs decls)
    -- Structs the project doesn't define, like the system's, change in ways
    -- the analysis doesn't see.
    externalTags = Set.fromList
        [ identToString i
        | (NamedRef i, CompDef (CompType _ _ _ _ ni)) <- Map.toList (gTags decls)
        , let pos = posOfNode ni
        , not (isSourcePos pos) || "/" `isPrefixOf` posFile pos
        ]

observeObject :: MonadTrav m => FilePath -> ObjDef -> m Observed
observeObject unit (ObjDef (VarDecl _ _ ty) ini _) = case ini of
    Just i  -> (<>) <$> initializer ty i <*> foldMapM (observeExpr unit) (initExprs i)
    Nothing -> return mempty

foldMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
foldMapM f xs = mconcat <$> mapM f xs

-- | What an initialiser for an object of a type stores in its fields.
initializer :: MonadTrav m => Type -> CInit -> m Observed
initializer ty = \case
    CInitList items _ | Just tag <- structTag ty -> return $ mconcat (map (item tag) items)
    _ -> return mempty
  where
    item tag = \case
        ([CMemberDesig f _], CInitExpr e _) -> case literalValue e of
            Just v  -> [Stored (tag, identToString f) (Nothing, [Range v v Plain])]
            Nothing -> [Escaped (tag, identToString f)]
        ([], CInitExpr e _) | literalValue e == Just 0 -> mempty
        _ -> [EscapedTag tag]
    literalValue = \case
        CConst (CIntConst i _) -> Just (getCInteger i)
        _                      -> Nothing

observeFunction :: MonadTrav m => FilePath -> [IdentDecl] -> FunDef -> m (Observed, Analysed)
observeFunction unit locals fd@(FunDef (VarDecl self _ fty) body _) = do
    key <- case self of
        VarName i _ -> funKey unit i
        NoName      -> return Nothing
    fn <- translate unit locals fd
    let nodes = solveFunction emptyProgram fn
    let retTy = case fty of
            FunctionType (FunType rt _ _) _ -> intType rt
            _                               -> Nothing
    perNode <- forM nodes $ \(n, facts, terms) -> do
        stores <- foldMapM (store facts) (concatMap subTerms terms)
        calls <- foldMapM (call facts) (concatMap subTerms terms)
        let returned = case (n, retTy, key) of
                (Node _ (StatNode CReturn{}), Just rt, Just k) ->
                    [Returned k (Just rt,
                        plain [ c | t <- terms, Just v <- [eval facts t], r <- v, c <- convert (annotation (termExpr t)) rt r ])]
                _ -> mempty
        return (stores <> calls <> returned)
    syntactic <- foldMapM (observeExpr unit) (statExprs body)
    inits <- foldMapM localInit [ (i, ni, ini) | CDecl _ ds _ <- localDecls body, (Just (CDeclr (Just i) _ _ _ ni), Just ini@CInitList{}, _) <- ds ]
    return (concat perNode ++ syntactic ++ inits ++ map Defined (maybeToList key), (fn, nodes))
  where
    localInit (i, ni, ini) = exprType (CVar i ni) >>= maybe (return mempty) (`initializer` ini)

    store facts t = case termKind t of
        TAssign _ _ l _ | CMember{} <- termExpr l -> stored (termExpr l) (termTy l) (eval facts t)
        TUnary op a | op `elem` [CPreIncOp, CPreDecOp, CPostIncOp, CPostDecOp], CMember{} <- termExpr a ->
            let d = if op `elem` [CPreIncOp, CPostIncOp] then 1 else -1
                after = do
                    ty <- termTy a
                    v <- eval facts a
                    return [ c | r <- v, c <- convert (annotation (termExpr t)) ty (Range (lo r + d) (hi r + d) Arith) ]
            in stored (termExpr a) (termTy a) after
        _ -> return mempty
    stored lhs ty value = fieldKey lhs >>= \k -> return $ case (k, value) of
        (Just key, Just v)  -> [Stored key (ty, plain v)]
        (Just key, Nothing) -> [Escaped key]
        _                   -> mempty
    call facts t = case termKind t of
        TCall (Term (CVar f _) _ _ _ _) args -> funKey unit f >>= \case
            Just k | (unit ++ ":") `isPrefixOf` k -> do
                params <- calleeParams f
                return $ case params of
                    Just ps | length ps == length args ->
                        [Passed k (zipWith (argValue facts) ps args)]
                    _ -> mempty
            _ -> return mempty
        _ -> return mempty
    argValue facts p a = do
        ty <- intType p
        v <- eval facts a
        return (Just ty, plain [ c | r <- v, c <- convert (annotation (termExpr a)) ty r ])
    calleeParams f = exprType (CVar f undefNode) >>= \case
        Just ty | Just (FunType _ ps False) <- funType ty -> return (Just (map declType ps))
        _ -> return Nothing

-- | What an expression shows, whatever the facts: fields whose address it
-- takes, structs it casts or hands to functions, functions it uses other than
-- by calling them, and what compound literals store.
observeExpr :: MonadTrav m => FilePath -> CExpr -> m Observed
observeExpr unit = go
  where
    go e = (<>) <$> here e <*> foldMapM go (children e)

    -- A function a call names isn't used other than by calling it.
    children = \case
        CCall CVar{} args _ -> args
        e                   -> exprChildren e

    here = \case
        CUnary CAdrOp e _ -> addressed e
        CVar i _ -> map Addressed . maybeToList <$> funKey unit i
        CCast d inner _ -> declaredType d >>= \case
            -- Only pointer casts can let the program change fields unseen.
            to@(Just t) | isJust (pointee t) -> do
                from <- exprType inner
                return $ map EscapedTag (catMaybes [castTag from to, castTag to from])
            _ -> return mempty
        CCall f args _ -> do
            callee <- case f of
                CVar i _ -> Just <$> (fromMaybe (identToString i) <$> funKey unit i)
                _        -> return Nothing
            calleeTy <- case f of
                CVar i _ -> fmap declType <$> (lookupObject i `catchTravError` const (return Nothing))
                _        -> exprType f
            let calleeName = fromMaybe "" callee
                sig = calleeTy >>= funType
            written <- case sig of
                Just (FunType _ params variadic) -> do
                    let paramTys = map (Just . declType) params ++ repeat Nothing
                    fmap catMaybes . forM (zip args paramTys) $ \(arg, pty) -> case pty of
                        -- A parameter's own type says what the callee can write,
                        -- unless it is a pointer to bytes or to anything.
                        Just t | Just p <- pointee t, isConst p -> return Nothing
                        Just t | Just p <- pointee t, isJust (structTag p) ->
                            return (writableTag t)
                        Just t | Nothing <- pointee t -> return Nothing
                        _ | isNothing pty && not variadic -> return Nothing
                        _ -> (>>= writableTag) <$> exprType arg
                _ -> catMaybes . map (>>= writableTag) <$> mapM exprType args
            let returned = case sig of
                    Just (FunType rt _ _) -> catMaybes [returnedTag rt]
                    _                     -> []
                tags = if calleeName `elem` harmless then [] else written ++ returned
            return [HandedTo calleeName tags]
        CCompoundLit d i ni -> declaredType d >>= maybe (return mempty) (`initializer` CInitList i ni)
        _ -> return mempty

    declaredType d = (Just <$> analyseTypeDecl d) `catchTravError` const (return Nothing)

    addressed e = case stripIndex e of
        m@CMember{} -> map Escaped . maybeToList <$> fieldKey m
        _           -> return mempty

    stripIndex = \case
        CIndex b _ _ -> stripIndex b
        e            -> e

    -- A pointer to a struct cast to a pointer to anything but void or the
    -- same struct lets the program change the struct's fields unseen.
    castTag from to = do
        f <- from >>= pointee
        t <- to >>= pointee
        tag <- structTag f
        if isVoid t || structTag t == Just tag then Nothing else Just tag

    writableTag ty = do
        p <- pointee ty
        if isConst p then Nothing else structTag p

    returnedTag ty = structTag ty `orElse` (pointee ty >>= structTag)

    orElse (Just x) _ = Just x
    orElse Nothing y  = y

    pointee ty = case canonicalType ty of
        PtrType p _ _ -> Just p
        _             -> Nothing

    isConst = constant . typeQuals

    isVoid t = case canonicalType t of
        DirectType TyVoid _ _ -> True
        _                     -> False
