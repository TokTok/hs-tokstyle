{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}
module Tokstyle.Linter.TypeCheck (descr) where

import           Control.Monad                         (forM, forM_, replicateM,
                                                        unless, void, zipWithM)
import           Control.Monad.State.Strict            (State, execState, gets,
                                                        modify, state)
import           Data.Fix                              (Fix (..))
import           Data.Foldable                         (toList)
import           Data.Graph                            (flattenSCC,
                                                        stronglyConnComp)
import           Data.IntMap.Strict                    (IntMap)
import qualified Data.IntMap.Strict                    as IntMap
import           Data.List                             (nub)
import           Data.Map.Strict                       (Map)
import qualified Data.Map.Strict                       as Map
import           Data.Maybe                            (fromMaybe, isJust,
                                                        listToMaybe, mapMaybe)
import           Data.Set                              (Set)
import qualified Data.Set                              as Set
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Language.Cimple                       (AssignOp (..),
                                                        BinaryOp (..),
                                                        Lexeme (..), NodeF (..),
                                                        UnaryOp (..))
import qualified Language.Cimple                       as C
import           Language.Cimple.Diagnostics           (CimplePos,
                                                        Diagnostic (..),
                                                        DiagnosticLevel (..),
                                                        DiagnosticSpan (..),
                                                        getDiagnosticInfo)
import           Language.Cimple.Pretty                (showNodePlain)
import           Prettyprinter                         (Doc, pretty, (<+>))
import           Prettyprinter.Render.Terminal         (AnsiStyle)
import           Tokstyle.Cimple.Analysis.Declarations
import           Tokstyle.Common                       (backticks)

ppType :: Type -> Doc AnsiStyle
ppType = backticks . pretty . go
  where
    go = \case
        TVoid              -> "void"
        TNum n             -> n
        TPtr (TFun ps r v) -> go r <> " (*)(" <> list ps v <> ")"
        TPtr t@TPtr{}      -> go t <> "*"
        TPtr t             -> go t <> " *"
        TFun ps r v        -> go (TPtr (TFun ps r v))
        TRec n _           -> n
        TExt n             -> n
        TNull              -> "nullptr"
        TVar _             -> "void"
        TParam _           -> "void"
        TAny               -> "void"
    list ps v = Text.intercalate ", " (map go ps ++ ["..." | v])

--------------------------------------------------------------------------------
-- Checking

-- | An access path like @a->b[i].c@.
data Path
    = PVar Text
    | PField Path Text
    -- | The index expression as text, and the variables it reads.
    | PIndex Path Text [Text]
    deriving (Eq, Ord)

-- | A type variable and the node that bound it.
data Origin = Origin Int FilePath Ast

type Site = (FilePath, Ast)

-- | The direction a value flows in when two types are unified: from the
-- second type into the first, or out of the first into the second.
data Flow = Into | OutOf

turned :: Flow -> Flow
turned Into  = OutOf
turned OutOf = Into

data St = St
    { stProg      :: Program
    -- | Maps each void pointer of a struct to the first one that shares its type.
    , stLinks     :: Map Text (IntMap Int)
    -- | The void pointers of callback types that accept any type.
    , stFree      :: Set Hole
    -- | The argument types each callback void pointer was called with.
    , stCalled    :: Map Hole (Set Type)
    -- | The callback void pointers each type variable belongs to.
    , stCallbacks :: IntMap (Set Hole)
    -- | The struct void pointers each type variable belongs to.
    , stHoles     :: IntMap (Set Hole)
    -- | The types each struct void pointer was given, and the types it was used as.
    , stGiven     :: Map Hole (Map Type Site)
    , stUsed      :: Map Hole (Map Type Site)
    , stNext      :: Int
    , stSubst     :: IntMap Type
    , stOrigins   :: IntMap Origin
    , stSchemes   :: Map Key Scheme
    , stMono      :: Map Key Type
    -- | For each function, the void pointer parameters it casts, with the type and the cast.
    , stUses      :: Map Text [(Text, Type, Origin)]
    , stInstances :: [(Text, [Type])]
    , stDiags     :: [Diagnostic CimplePos]
    -- The function being checked.
    , stFile      :: FilePath
    , stDefines   :: Map Text Define
    , stLocals    :: Map Text Type
    , stPaths     :: Map Path Type
    , stReturn    :: Type
    , stName      :: Text
    -- | The macro call being expanded.
    , stCall      :: Maybe Ast
    -- | The origins of the type variables resolved during the current comparison.
    , stTrail     :: Maybe [Origin]
    -- | The function whose arguments are being checked.
    , stCallee    :: Maybe Text
    }

type M = State St

fresh :: M Type
fresh = state $ \s -> (TVar (stNext s), s { stNext = stNext s + 1 })

freshArgs :: Text -> M [Type]
freshArgs tag = do
    record <- gets (Map.lookup tag . pRecords . stProg)
    links <- gets (Map.findWithDefault IntMap.empty tag . stLinks)
    let arity = maybe 0 recArity record
        first' i = IntMap.findWithDefault i i links
    as <- replicateM arity fresh
    forM_ (maybe [] (IntMap.toList . recCallbacks) record) $ \(i, h) -> callback (as !! first' i) h
    let args = map ((as !!) . first') [0 .. arity - 1]
    args <$ instances (TRec tag args)

callback :: Type -> Hole -> M ()
callback (TVar a) h = modify $ \s -> s { stCallbacks = IntMap.insertWith Set.union a (Set.singleton h) (stCallbacks s) }
callback _ _        = pure ()

-- | Records the struct instances in a type, and which void pointer each of
-- their type variables is.
instances :: Type -> M ()
instances t = modify $ \s -> s
    { stInstances = records t ++ stInstances s
    , stHoles = IntMap.unionWith Set.union (stHoles s) $ IntMap.fromListWith Set.union
        [ (a, Set.singleton (tag, IntMap.findWithDefault i i (Map.findWithDefault IntMap.empty tag (stLinks s))))
        | (tag, args) <- records t, (i, TVar a) <- zip [0 ..] args ]
    }

typeOf :: Ast -> M Type
typeOf ty = gets stProg >>= \prog -> fromAst prog hole freshArgs ty
  where
    hole Nothing  = fresh
    hole (Just h) = gets (Set.member h . stFree) >>= \case
        True  -> pure TAny
        False -> fresh >>= \t -> t <$ callback t h

declared :: Ast -> [Ast] -> M Type
declared ty dims = arrays (length dims) <$> typeOf ty

-- | Resolves a bound type variable, at the top level only.
shallow :: Type -> M Type
shallow = fmap (\(t, _, _) -> t) . resolve

-- | Like `shallow`, but also returns the variable's origin and the void
-- pointers it belongs to.
resolve :: Type -> M (Type, Maybe Origin, Set Hole)
resolve t@(TVar a) = do
    holes <- gets (IntMap.findWithDefault Set.empty a . stHoles)
    gets (IntMap.lookup a . stSubst) >>= \case
        Nothing -> pure (t, Nothing, holes)
        Just t' -> do
            origin <- gets (IntMap.lookup a . stOrigins)
            forM_ origin $ \o -> modify (\s -> s { stTrail = (o :) <$> stTrail s })
            (\(u, o, hs) -> (u, maybe origin Just o, Set.union holes hs)) <$> resolve t'
resolve t = pure (t, Nothing, Set.empty)

zonk :: Type -> M Type
zonk t = gets stSubst >>= \s -> pure (go s t)
  where
    go s = \case
        TVar a | Just t' <- IntMap.lookup a s -> go s t'
        TPtr t'                               -> TPtr (go s t')
        TFun ps r v                           -> TFun (map (go s) ps) (go s r) v
        TRec n as                             -> TRec n (map (go s) as)
        t'                                    -> t'

bind :: Maybe Origin -> Int -> Type -> M ()
bind origin a t = do
    t' <- zonk t
    unless (a `elem` vars t') $ modify $ \s -> s
        { stSubst = IntMap.insert a t' (stSubst s)
        , stOrigins = maybe id (IntMap.insert a) (if isVar t' then Nothing else origin) (stOrigins s)
        }

-- | Any object can be read as bytes, and bytes can hold any object.
isBytes :: Type -> Bool
isBytes = (`elem` [TNum "uint8_t", TNum "char"])

isVar :: Type -> Bool
isVar TVar{} = True
isVar _      = False

-- | Returns the two types that differ if a `got` can't be converted to a
-- `want`. With a `flow`, the value flows from `got` to `want`.
convert :: Ast -> Maybe Flow -> Type -> Type -> M (Maybe (Type, Type))
convert at flow want got = do
    w <- shallow want
    g <- shallow got
    case (w, g) of
        (TVar a, TVar b) | a == b -> ok
        (TVar a, _)               -> ok <* bind Nothing a g
        (_, TVar b)               -> ok <* bind Nothing b w
        (TExt _, _)               -> ok
        (_, TExt _)               -> ok
        (TNum _, TNum _)          -> ok
        (TPtr _, TNull)           -> ok
        (TNull, TPtr _)           -> ok
        (TNull, TNull)            -> ok
        (TPtr a, TPtr b)          -> same at flow a b
        (TPtr a@TFun{}, TFun{})   -> same at flow a g
        (TFun{}, TPtr b@TFun{})   -> same at flow w b
        (TFun{}, TNull)           -> ok
        (TNull, TFun{})           -> ok
        _                         -> same at flow w g
  where
    ok = pure Nothing

-- | Unifies two types. Returns the two types that differ if they don't
-- unify. With `Into`, a `u` flows into a `t`.
same :: Ast -> Maybe Flow -> Type -> Type -> M (Maybe (Type, Type))
same at flow t u = do
    (a, oa, ha) <- resolve t
    (b, ob, hb) <- resolve u
    file <- gets stFile
    site <- gets stCall
    let here x = Just (Origin x file (fromMaybe at site))
        place = (file, fromMaybe at site)
    forM_ flow $ \f -> flows place f ha b >> flows place (turned f) hb a
    case (a, b) of
        (TAny, _)                   -> ok
        (_, TAny)                   -> ok
        (TVar x, TVar y) | x == y   -> ok
        (TVar x, TVar y)            -> ok <* tie x y <* bind Nothing x b
        (TVar x, _)                 -> ok <* bind (maybe (here x) Just ob) x b
        (_, TVar y)                 -> ok <* bind (maybe (here y) Just oa) y a
        (TExt _, TExt _)            -> ok
        (TExt _, TNum _)            -> ok
        (TNum _, TExt _)            -> ok
        (TNum x, TNum y) | x == y   -> ok
        (TVoid, TVoid)              -> ok
        (TPtr x, TPtr y)            -> same at flow x y
        (TFun ps r _, TFun qs s _) | length ps == length qs ->
            -- A function takes its arguments, so they flow the other way.
            listToMaybe . mapMaybe id <$> ((:) <$> same at flow r s <*> zipWithM (same at (turned <$> flow)) ps qs)
        (TRec x xs, TRec y ys) | x == y ->
            listToMaybe . mapMaybe id <$> zipWithM (same at Nothing) xs ys
        _                           -> pure (Just (a, b))
  where
    ok = pure Nothing

-- | Records that a value of type `t` is stored in the void pointers (`Into`),
-- or that they are used as a `t` (`OutOf`).
flows :: Site -> Flow -> Set Hole -> Type -> M ()
flows place flow holes t = unless (Set.null holes) $ plain <$> zonk t >>= \t' ->
    unless (isVar t' || t' `elem` [TNull, TAny] || isBytes t' || not (null (vars t'))) $ modify $ \s ->
        let add m = foldr (\h -> Map.insertWith (flip Map.union) h (Map.singleton t' place)) m (Set.toList holes)
        in case flow of
            Into  -> s { stGiven = add (stGiven s) }
            OutOf -> s { stUsed = add (stUsed s) }

-- | Drops the type arguments of structs.
plain :: Type -> Type
plain = \case
    TPtr t      -> TPtr (plain t)
    TFun ps r v -> TFun (map plain ps) (plain r) v
    TRec n _    -> TRec n []
    t           -> t

-- | Called when `x` is unified with `y`: `y` also gets the void pointers of `x`.
tie :: Int -> Int -> M ()
tie x y = modify $ \s -> s
    { stHoles = IntMap.insertWith Set.union y (IntMap.findWithDefault Set.empty x (stHoles s)) (stHoles s) }

--------------------------------------------------------------------------------
-- Diagnostics

report :: Ast -> Doc AnsiStyle -> M ()
report at msg = do
    file <- gets stFile
    site <- gets stCall
    trail <- gets (fromMaybe [] . stTrail)
    uses <- gets stUses
    callee <- gets stCallee
    reasons <- forM trail $ \(Origin a f n) ->
        spanAt f n . pure . (<+> "because of this") . ppType <$> zonk (TPtr (TVar a))
    let node = fromMaybe at site
        (pos, len) = getDiagnosticInfo file node
        used = [ spanAt f n [quote fn <+> "uses" <+> quote p <+> "as" <+> ppType t]
               | fn <- maybe id (:) callee [ v | Fix (VarExpr (L _ _ v)) <- concatMap subnodes (node : [ n | Origin _ _ n <- trail ]) ]
               , (p, t, Origin _ f n) <- Map.findWithDefault [] fn uses ]
        spans = take 3 . nubOn (\s -> (spanPos s, spanLen s)) $ filter ((/= pos) . spanPos) (reasons ++ used)
    modify $ \s -> s { stDiags = Diagnostic pos len ErrorLevel msg Nothing spans [] : stDiags s }
  where
    spanAt f n = uncurry DiagnosticSpan (getDiagnosticInfo f n)
    nubOn f = Map.elems . Map.fromList . map (\x -> (f x, x)) . reverse

-- | Reports an error if a value of the second type can't be converted to the
-- first. `msg` gets the type the value has, then the type it must have.
expect :: Ast -> (Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle) -> Type -> Type -> M ()
expect = expectFlow (Just Into)

-- | Like `expect`, for two values that are only compared.
alike :: Ast -> (Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle) -> Type -> Type -> M ()
alike = expectFlow Nothing

expectFlow :: Maybe Flow -> Ast -> (Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle) -> Type -> Type -> M ()
expectFlow flow at msg want got = tracing $
    convert at flow want got >>= mapM_ (\(a, b) -> do
        w <- zonk want
        g <- zonk got
        a' <- zonk (TPtr a)
        b' <- zonk (TPtr b)
        -- Structs that differ only in their type arguments are printed the same.
        let for t inner | show (ppType w) == show (ppType g) = ppType t <+> "for" <+> ppType inner
                        | otherwise = ppType t
        report at (uncurry msg (fromMaybe (for g b', for w a') (differing g w))))

-- | The first parameter or result in which two function types differ,
-- described for each of them.
differing :: Type -> Type -> Maybe (Doc AnsiStyle, Doc AnsiStyle)
differing t u = case (function t, function u) of
    (Just (ps, r), Just (qs, s)) | length ps == length qs -> listToMaybe $
        [ (takes p, takes q) | (p, q) <- zip ps qs, plain p /= plain q ]
        ++ [ (returns r, returns s) | plain r /= plain s ]
    _ -> Nothing
  where
    function = \case
        TPtr f      -> function f
        TFun ps r _ -> Just (ps, r)
        _           -> Nothing
    takes p = "a function that takes" <+> ppType p
    returns r = "a function that returns" <+> ppType r

-- | Records the origins of the type variables resolved while `m` runs, for a
-- report.
tracing :: M a -> M a
tracing m = modify (\s -> s { stTrail = Just [] }) *> m <* modify (\s -> s { stTrail = Nothing })

code :: Ast -> Doc AnsiStyle
code = backticks . pretty . showNodePlain

quote :: Text -> Doc AnsiStyle
quote = backticks . pretty

--------------------------------------------------------------------------------
-- Expressions

path :: Ast -> Maybe Path
path (Fix node) = case node of
    VarExpr (L _ _ n)         -> Just (PVar n)
    ParenExpr e               -> path e
    MemberAccess e (L _ _ f)  -> flip PField f <$> path e
    PointerAccess e (L _ _ f) -> flip PField f . first' <$> path e
    UnaryExpr UopDeref e      -> first' <$> path e
    ArrayAccess e i           -> (\p -> PIndex p (showNodePlain i)) <$> path e <*> stable i
    _                         -> Nothing
  where
    first' p = PIndex p "0" []

-- | Returns the variables an index expression reads, or 'Nothing' if
-- evaluating it twice may give different values.
stable :: Ast -> Maybe [Text]
stable (Fix node) = case node of
    VarExpr (L _ _ v) -> Just [v]
    LiteralExpr _ _   -> Just []
    ParenExpr e       -> stable e
    MemberAccess e _  -> stable e
    PointerAccess e _ -> stable e
    ArrayAccess e i   -> (++) <$> stable e <*> stable i
    BinaryExpr l _ r  -> (++) <$> stable l <*> stable r
    _                 -> Nothing

within :: Path -> Path -> Bool
within object p = p == object || case p of
    PVar _       -> False
    PField q _   -> within object q
    PIndex q _ _ -> within object q

mentions :: Text -> Path -> Bool
mentions v = \case
    PVar n        -> n == v
    PField p _    -> mentions v p
    PIndex p _ vs -> v `elem` vs || mentions v p

root :: Path -> Text
root = \case
    PVar n       -> n
    PField p _   -> root p
    PIndex p _ _ -> root p

changed :: Text -> M ()
changed v = modify $ \s -> s { stPaths = Map.filterWithKey (const . not . mentions v) (stPaths s) }

changes :: Ast -> [Text]
changes = concatMap (go . unFix) . subnodes
  where
    go = \case
        AssignExpr (Fix (VarExpr (L _ _ v))) _ _       -> [v]
        UnaryExpr UopIncr (Fix (VarExpr (L _ _ v)))    -> [v]
        UnaryExpr UopDecr (Fix (VarExpr (L _ _ v)))    -> [v]
        UnaryExpr UopAddress (Fix (VarExpr (L _ _ v))) -> [v]
        VarDecl _ (L _ _ v) _                          -> [v]
        _                                              -> []

expr :: Ast -> M Type
expr n = case path n of
    Just PVar{} -> expr' n
    Just p      -> gets (Map.lookup p . stPaths) >>= \case
        Just t  -> pure t
        Nothing -> do
            t <- expr' n
            t <$ modify (\s -> s { stPaths = Map.insert p t (stPaths s) })
    Nothing     -> expr' n

expr' :: Ast -> M Type
expr' n@(Fix node) = case node of
    ParenExpr e -> expr e
    CommentExpr _ e -> expr e
    MacroBodyFunCall e -> expr e
    MacroBodyStmt s -> TVoid <$ stmt s
    LiteralExpr C.Int _ -> pure (TNum "int")
    LiteralExpr C.Float _ -> pure (TNum "float")
    LiteralExpr C.Bool _ -> pure (TNum "bool")
    LiteralExpr C.Char _ -> pure (TNum "char")
    LiteralExpr C.String _ -> pure (TPtr (TNum "char"))
    LiteralExpr C.ConstId (L _ _ v) -> variable n v
    VarExpr (L _ _ v) -> variable n v
    UnaryExpr op e -> unary n op e
    BinaryExpr l op r -> binary n l op r
    AssignExpr l op r -> assign n l op r
    TernaryExpr c t e -> do
        cond c
        a <- expr t
        b <- expr e
        alike e (\got want -> code e <+> "is" <+> got <> ", but" <+> code t <+> "is" <+> want) a b
        shallow a >>= \case
            TNull -> pure b
            _     -> pure a
    FunctionCall f args -> call n f args
    MemberAccess e f -> expr e >>= member n f
    PointerAccess e f -> expr e >>= pointee e >>= member n f
    ArrayAccess e i -> do
        t <- expr e
        expr i >>= number i "an index"
        pointee e t >>= element
    CastExpr ty e -> do
        t <- typeOf ty
        g <- expr e
        t <$ cast n e t g
    SizeofExpr e -> TNum "size_t" <$ expr e
    SizeofType _ -> pure (TNum "size_t")
    CompoundLiteral ty e -> do
        t <- typeOf ty
        t <$ initialise (showNodePlain ty) t (Fix (InitialiserList [e]))
    InitialiserList es -> mapM_ expr es >> fresh
    _ -> fresh

-- | Each array element gets its own type variables, so two elements don't
-- need to have the same type.
element :: Type -> M Type
element t = zonk t >>= \t' -> instantiate (Forall (nub (vars t')) t')

instantiate :: Scheme -> M Type
instantiate (Forall [] t) = pure t
instantiate (Forall as t) = do
    t' <- (`rename` t) <$> renaming as
    t' <$ instances t'

renaming :: [Int] -> M (IntMap Type)
renaming as = do
    sub <- IntMap.fromList <$> mapM (\a -> (a,) <$> fresh) as
    sub <$ modify (\s -> s { stCallbacks = IntMap.union (stCallbacks s) $ IntMap.fromList
        [ (a', hs) | (a, TVar a') <- IntMap.toList sub, Just hs <- [IntMap.lookup a (stCallbacks s)] ] })

rename :: IntMap Type -> Type -> Type
rename sub = go
  where
    go = \case
        TVar a      -> IntMap.findWithDefault (TVar a) a sub
        TPtr u      -> TPtr (go u)
        TFun ps r v -> TFun (map go ps) (go r) v
        TRec r us   -> TRec r (map go us)
        u           -> u

variable :: Ast -> Text -> M Type
variable n v
    | v `elem` ["nullptr", "NULL"] = pure TNull
    | otherwise = gets (Map.lookup v . stLocals) >>= \case
        Just t  -> pure t
        Nothing -> define v >>= \case
            Just (Constant e) -> expanding n (expr e)
            Just Macro{}      -> fresh
            Nothing           -> global v

define :: Text -> M (Maybe Define)
define v = gets $ \s -> case Map.lookup v (stDefines s) of
    Nothing -> Map.lookup v (pDefines (stProg s))
    d       -> d

-- | Errors found while expanding a macro are reported at the macro call.
expanding :: Ast -> M a -> M a
expanding at m = do
    site <- gets stCall
    modify (\s -> s { stCall = Just (fromMaybe at site) })
    m <* modify (\s -> s { stCall = site })

-- | Looks up a name: the file's static first, then the program's global.
visible :: Text -> Map Key a -> M (Maybe a)
visible v m = gets $ \s -> maybe (Map.lookup (Nothing, v) m) Just (Map.lookup (Just (stFile s), v) m)

global :: Text -> M Type
global v = do
    mono <- gets stMono >>= visible v
    scheme <- gets stSchemes >>= visible v
    enumerator <- gets (Map.lookup v . pEnumerators . stProg)
    case (mono, scheme, enumerator) of
        (Just t, _, _) -> pure t
        (_, Just s, _) -> instantiate s
        (_, _, Just e) -> pure (TNum e)
        _              -> fresh

unary :: Ast -> UnaryOp -> Ast -> M Type
unary n op e = case op of
    UopNot     -> TNum "bool" <$ cond e
    UopNeg     -> numeric
    UopMinus   -> numeric
    UopAddress -> do
        forM_ (variableName e) changed
        TPtr <$> expr e
    UopDeref   -> expr e >>= pointee e
    UopIncr    -> step
    UopDecr    -> step
  where
    numeric = do
        t <- expr e
        t <$ number e (opName n) t
    step = do
        t <- expr e
        forM_ (variableName e) changed
        shallow t >>= \case
            TPtr _ -> pure t
            _      -> t <$ number e (opName n) t

opName :: Ast -> Doc AnsiStyle
opName (Fix node) = backticks $ case node of
    UnaryExpr UopNeg _   -> "~"
    UnaryExpr UopMinus _ -> "-"
    UnaryExpr UopIncr _  -> "++"
    UnaryExpr UopDecr _  -> "--"
    BinaryExpr _ op _    -> binaryName op
    AssignExpr _ op _    -> assignName op
    _                    -> "?"

binaryName :: BinaryOp -> Doc AnsiStyle
binaryName = \case
    BopNe -> "!="; BopEq -> "=="; BopOr -> "||"; BopBitXor -> "^"; BopBitOr -> "|"
    BopAnd -> "&&"; BopBitAnd -> "&"; BopDiv -> "/"; BopMul -> "*"; BopMod -> "%"
    BopPlus -> "+"; BopMinus -> "-"; BopLt -> "<"; BopLe -> "<="; BopLsh -> "<<"
    BopGt -> ">"; BopGe -> ">="; BopRsh -> ">>"

assignName :: AssignOp -> Doc AnsiStyle
assignName = \case
    AopEq -> "="; AopMul -> "*="; AopDiv -> "/="; AopPlus -> "+="; AopMinus -> "-="
    AopBitAnd -> "&="; AopBitOr -> "|="; AopBitXor -> "^="; AopMod -> "%="
    AopLsh -> "<<="; AopRsh -> ">>="

variableName :: Ast -> Maybe Text
variableName (Fix (VarExpr (L _ _ v))) = Just v
variableName (Fix (ParenExpr e))       = variableName e
variableName _                         = Nothing

pointee :: Ast -> Type -> M Type
pointee e t = shallow t >>= \case
    TPtr u   -> pure u
    u@TFun{} -> pure u
    TVar _   -> fresh
    TExt _   -> fresh
    TAny     -> fresh
    u        -> do
        u' <- zonk u
        report e (code e <+> "is" <+> ppType u' <> ", which is not a pointer")
        fresh

member :: Ast -> Lexeme Text -> Type -> M Type
member n (L _ _ f) t = shallow t >>= \case
    TRec tag args -> gets (Map.lookup tag . pRecords . stProg) >>= \case
        Nothing -> fresh
        Just r -> case Map.lookup f (recFields r) of
            Nothing -> report n (quote tag <+> "has no member" <+> quote f) >> fresh
            Just ft -> do
                free <- gets stFree
                let field = \case
                        TParam i | maybe False (`Set.member` free) (IntMap.lookup i (recCallbacks r)) -> pure TAny
                                 | otherwise -> pure (args !! i)
                        TPtr u        -> TPtr <$> field u
                        TFun ps ret v -> TFun <$> mapM field ps <*> field ret <*> pure v
                        TRec nested _ -> TRec nested <$> freshArgs nested
                        u             -> pure u
                field ft
    TVar _ -> fresh
    TExt _ -> fresh
    TAny   -> fresh
    u -> do
        u' <- zonk u
        report n (quote f <+> "is read from" <+> ppType u' <> ", which is not a struct")
        fresh

number :: Ast -> Doc AnsiStyle -> Type -> M ()
number e what t = shallow t >>= \case
    TNum _ -> pure ()
    TVar _ -> pure ()
    TExt _ -> pure ()
    TAny   -> pure ()
    u      -> do
        u' <- zonk u
        report e (code e <+> "is" <+> ppType u' <> ", but" <+> what <+> "needs a number")

cond :: Ast -> M ()
cond e = expr e >>= shallow >>= \case
    t@TRec{} -> report e (code e <+> "is" <+> ppType t <> ", which can't be a condition")
    TVoid    -> report e (code e <+> "has no value to be a condition")
    _        -> pure ()

isPtr :: Type -> M Bool
isPtr t = shallow t >>= \case
    TPtr _ -> pure True
    _      -> pure False

binary :: Ast -> Ast -> BinaryOp -> Ast -> M Type
binary n l op r
    | op `elem` [BopAnd, BopOr] = TNum "bool" <$ (cond l >> cond r)
    | otherwise = do
        a <- expr l
        b <- expr r
        pa <- isPtr a
        pb <- isPtr b
        case op of
            _ | op `elem` [BopEq, BopNe, BopLt, BopLe, BopGt, BopGe] -> do
                alike r (\got want -> code r <+> "is" <+> got <> ", but it is compared with"
                    <+> code l <> ", which is" <+> want) a b
                pure (TNum "bool")
            BopPlus | pa -> a <$ number r (opName n) b
                    | pb -> b <$ number l (opName n) a
            BopMinus | pa && pb -> pure (TNum "ptrdiff_t")
                     | pa -> a <$ number r (opName n) b
            _ -> do
                number l (opName n) a
                number r (opName n) b
                shallow a >>= \case
                    TNum _ -> pure a
                    _      -> pure b

assign :: Ast -> Ast -> AssignOp -> Ast -> M Type
assign n l op r = do
    -- The assignment invalidates the paths within `l` and the paths indexed by
    -- its variable.
    forM_ (path l) $ \p -> modify $ \s -> s
        { stPaths = Map.filterWithKey (\q _ -> not (within p q || indexReads (root p) q)) (stPaths s) }
    forM_ (variableName l) changed
    a <- expr l
    case op of
        AopEq -> a <$ initialise (showNodePlain l) a r
        _ -> do
            b <- expr r
            pa <- isPtr a
            unless (pa && op `elem` [AopPlus, AopMinus]) $ number l (opName n) a
            a <$ number r (opName n) b

indexReads :: Text -> Path -> Bool
indexReads v = \case
    PVar _        -> False
    PField p _    -> indexReads v p
    PIndex p _ vs -> v `elem` vs || indexReads v p

initialise :: Text -> Type -> Ast -> M ()
initialise what want e@(Fix node) = case node of
    InitialiserList [Fix (LiteralExpr C.Int (L _ _ "0"))] -> pure ()
    InitialiserList es -> shallow want >>= \case
        TRec tag args | length es > 1 || not (any isList es) ->
            gets (Map.lookup tag . pRecords . stProg) >>= \case
                Just r | recStruct r, length es <= length (recOrder r) ->
                    forM_ (zip (recOrder r) es) $ \(f, x) -> do
                        ft <- member e (L (C.AlexPn 0 0 0) C.IdVar f) (TRec tag args)
                        nested ft x $ initialise (what <> "." <> f) ft x
                _ -> mapM_ expr es
        TPtr t -> forM_ es $ \x -> element t >>= \et -> nested et x (initialise what et x)
        _ -> mapM_ expr es
    _ -> do
        got <- expr e
        expect e (\g w -> code e <+> "is" <+> g <> ", but" <+> quote what <+> "is" <+> w) want got
  where
    isList (Fix InitialiserList{}) = True
    isList _                       = False
    -- A nested struct or array may be initialised without braces.
    nested t x m = shallow t >>= \case
        TRec{} | not (isList x) -> void (expr x)
        _                       -> m

cast :: Ast -> Ast -> Type -> Type -> M ()
cast n e want got = do
    w <- shallow want
    g <- shallow got
    case (w, g) of
        (TPtr b, _) | isBytes b -> pure ()
        (TPtr a, TPtr b) -> do
            a' <- shallow a
            b' <- shallow b
            case (a', b', got) of
                (TVar _, _, _)        -> pure ()
                (_, TVar _, _)        -> void (same n (Just Into) a b)
                -- The value is a void pointer that already has a type.
                (_, _, TPtr (TVar _)) -> typed
                _                     -> pure ()
        _ -> pure ()
  where
    typed = tracing $ same n (Just Into) want got >>= mapM_ (const $ do
        w <- zonk want
        g <- zonk got
        report n (code e <+> "is cast to" <+> ppType w <> ", but it is" <+> ppType g))

call :: Ast -> Ast -> [Ast] -> M Type
call n f args = case calleeName f of
    Just v -> gets (Map.member v . stLocals) >>= \case
        True -> function Nothing
        False -> define v >>= \case
            Just (Macro ps body) -> expanding n $ do
                let (fixed, rest) = splitAt (length ps) args
                expr' (substitute (Map.fromList (zip ps fixed)) rest body)
            _ -> function (Just v)
    Nothing -> function Nothing
  where
    function v = do
        ft <- expr f
        ats <- mapM expr args
        shallow ft >>= \case
            TPtr t -> shallow t >>= apply v ats
            t      -> apply v ats t

    apply v ats = \case
        TFun ps r variadic -> do
            known <- maybe (pure []) parameterNames v
            if length ats < length ps || (not variadic && length ats > length ps)
                then report n (code f <+> "takes" <+> pretty (length ps) <+> plural (length ps)
                        <> ", but is called with" <+> pretty (length ats))
                else forM_ (zip3 [0 ..] ps (zip args ats)) $ \(i, p, (a, at)) -> withCallee v $ do
                    unless (isJust v) $ called p at
                    expect a (\got want -> code a <+> "is" <+> got <> ", but"
                        <+> parameter (known ++ repeat Nothing) i <+> "of" <+> code f <+> "is" <+> want) p at
            pure r
        _ -> fresh

    called (TPtr (TVar a)) at = gets (IntMap.findWithDefault Set.empty a . stCallbacks) >>= mapM_ (\hole -> zonk at >>= \case
        TPtr x | null (vars x) -> modify (\s -> s { stCalled = Map.insertWith Set.union hole (Set.singleton x) (stCalled s) })
        _ -> pure ()) . Set.toList
    called _ _ = pure ()

    withCallee :: Maybe Text -> M () -> M ()
    withCallee v m = modify (\s -> s { stCallee = v }) *> m <* modify (\s -> s { stCallee = Nothing })

    plural :: Int -> Doc AnsiStyle
    plural 1 = "argument"
    plural _ = "arguments"

    parameter :: [Maybe Text] -> Int -> Doc AnsiStyle
    parameter known i = maybe ("argument" <+> pretty (i + 1)) quote (known !! i)

parameterNames :: Text -> M [Maybe Text]
parameterNames v = gets (pGlobals . stProg) >>= visible v >>= \case
    Just (Function (Fix (FunctionPrototype _ _ ps)) _) -> pure (map paramName (fst (parameters ps)))
    _                                                  -> pure []

calleeName :: Ast -> Maybe Text
calleeName (Fix node) = case node of
    VarExpr (L _ _ v)               -> Just v
    LiteralExpr C.ConstId (L _ _ v) -> Just v
    ParenExpr e                     -> calleeName e
    _                               -> Nothing

substitute :: Map Text Ast -> [Ast] -> Ast -> Ast
substitute ps rest = go
  where
    go (Fix node) = case node of
        VarExpr (L _ _ v) | Just a <- Map.lookup v ps -> a
        FunctionCall f as -> Fix (FunctionCall (go f) (concatMap arg as))
        MemberAccess e f -> Fix (MemberAccess (go e) (member' f))
        PointerAccess e f -> Fix (PointerAccess (go e) (member' f))
        _ -> Fix (fmap go node)
    -- A parameter can name a member.
    member' l@(L p c f) = case Map.lookup f ps of
        Just (Fix (VarExpr (L _ _ v))) -> L p c v
        _                              -> l
    arg (Fix (LiteralExpr C.ConstId (L _ _ "__VA_ARGS__"))) = rest
    arg a                                                   = [go a]

--------------------------------------------------------------------------------
-- Statements

scoped :: M a -> M a
scoped m = do
    locals <- gets stLocals
    defines <- gets stDefines
    m <* modify (\s -> s { stLocals = locals, stDefines = defines })

declare :: Text -> Type -> M ()
declare v t = changed v >> modify (\s -> s { stLocals = Map.insert v t (stLocals s) })

-- | Variables the loop body changes are invalidated before the loop, as the
-- body may run again after changing them.
loop :: [Ast] -> M a -> M a
loop body m = mapM_ changed (concatMap changes body) >> m

-- | Checks both branches of an `if` on copies of the unbound type variables,
-- and keeps only the bindings both branches agree on.
branches :: Ast -> M () -> M () -> M ()
branches at a b = do
    locals <- gets stLocals >>= mapM zonk
    paths <- gets stPaths >>= mapM zonk
    let open = nub (concatMap vars (Map.elems locals ++ Map.elems paths))
        branch :: M () -> M (IntMap Type, IntMap Type, Set Path)
        branch m = do
            sub <- renaming open
            let ls = Map.map (rename sub) locals
                ps = Map.map (rename sub) paths
            mapM_ instances (Map.elems ls ++ Map.elems ps)
            modify (\s -> s { stLocals = ls, stPaths = ps })
            m
            kept <- gets (Map.keysSet . stPaths)
            found <- mapM zonk sub
            pure (sub, found, kept)
    (sub, found, kept) <- branch a
    (_, found', kept') <- branch b
    modify $ \s -> s { stLocals = locals, stPaths = Map.restrictKeys paths (Set.intersection kept kept') }
    let groups = Map.fromListWith (++) [ ((found IntMap.! v, found' IntMap.! v), [v]) | v <- open ]
    forM_ (Map.toList groups) $ \((t, t'), vs) -> do
        -- Unify the variables that both branches unified.
        mapM_ (same at Nothing (TVar (foldr1 const vs)) . TVar) (drop 1 vs)
        unless (t /= t' || not (null (vars t))) $ mapM_ (\v -> same at Nothing (TVar v) (sub IntMap.! v)) (take 1 vs)

stmt :: Ast -> M ()
stmt n@(Fix node) = case node of
    CompoundStmt ss -> scoped (mapM_ stmt (concatMap flatten ss))
    VarDeclStmt (Fix (VarDecl ty (L _ _ v) dims)) value -> do
        t <- declared ty dims
        forM_ value (initialise v t)
        declare v t
    VLA ty (L _ _ v) size -> do
        expr size >>= number size "an array size"
        typeOf ty >>= declare v . TPtr
    Return (Just e) -> do
        t <- gets stReturn
        fn <- gets stName
        got <- expr e
        expect e (\g w -> code e <+> "is" <+> g <> ", but" <+> quote fn <+> "returns" <+> w) t got
    IfStmt c t Nothing -> cond c >> stmt t
    IfStmt c t (Just e) -> cond c >> branches n (stmt t) (stmt e)
    WhileStmt c b -> loop [c, b] (cond c >> stmt b)
    DoWhileStmt b c -> loop [b, c] (stmt b >> cond c)
    ForStmt i c s b -> scoped $ stmt i >> loop [c, s, b] (cond c >> stmt b >> void (expr s))
    SwitchStmt e cs -> expr e >> mapM_ stmt (concatMap flatten cs)
    Case e b -> expr e >> stmt b
    Default b -> stmt b
    Label _ s -> stmt s
    ExprStmt e -> void (expr e)
    MacroBodyStmt s -> stmt s
    PreprocDefineConst (L _ _ v) e -> defineLocal v (Just (Constant e))
    PreprocDefineMacro (L _ _ v) ps e -> defineLocal v (Just (Macro (macroParams ps) e))
    PreprocUndef (L _ _ v) -> defineLocal v Nothing
    Return Nothing -> pure ()
    Break -> pure ()
    Continue -> pure ()
    Goto _ -> pure ()
    _ -> void (expr n)
  where
    defineLocal :: Text -> Maybe Define -> M ()
    defineLocal v d = modify (\s -> s { stDefines = Map.alter (const d) v (stDefines s) })

--------------------------------------------------------------------------------
-- The whole program

-- | Also collects the names used by the macros and constants a body refers to.
names :: Map Text Define -> Ast -> Set Text
names defines = go Set.empty . pure
  where
    go seen [] = seen
    go seen (n : ns) = go seen' (new ++ ns)
      where
        found = Set.fromList [ v | Fix x <- subnodes n, Just v <- [ident x] ] `Set.difference` seen
        seen' = seen `Set.union` found
        new = concatMap body (mapMaybe (`Map.lookup` defines) (Set.toList found))
    ident = \case
        VarExpr (L _ _ v)               -> Just v
        LiteralExpr C.ConstId (L _ _ v) -> Just v
        _                               -> Nothing
    body = \case
        Constant e -> [e]
        Macro _ e  -> [e]

signature :: Global -> M Type
signature (Function proto _) = typeOf proto
signature (Variable ty _)    = typeOf ty

checkGlobal :: Text -> Global -> Type -> M ()
checkGlobal v g t = case (g, t) of
    (Function _ bodies, TFun pts ret _) -> forM_ bodies $ \(Body file defines ps body) -> do
        enter file defines (Map.fromList [ (p, pt) | (Just p, pt) <- zip ps pts ]) ret
        stmt body
    (Variable _ bodies, _) -> forM_ bodies $ \(Body file defines _ value) -> do
        enter file defines Map.empty TVoid
        initialise v t value
    _ -> pure ()
  where
    enter :: FilePath -> Map Text Define -> Map Text Type -> Type -> M ()
    enter file defines locals ret = modify $ \s -> s
        { stFile = file, stDefines = defines, stLocals = locals, stPaths = Map.empty
        , stReturn = ret, stName = v, stCall = Nothing }

-- | Checks one strongly connected component of globals, then generalises
-- their types.
component :: [(Key, Global)] -> M ()
component gs = do
    modify $ \s -> s { stInstances = [] }
    ts <- forM gs $ \(k, g) -> (k,) <$> signature g
    modify $ \s -> s { stMono = Map.fromList ts }
    forM_ (zip gs ts) $ \((k, g), (_, t)) -> checkGlobal (snd k) g t
    forM_ (zip gs ts) $ \((k, g), (_, t)) -> do
        t' <- zonk t
        modify $ \s -> s { stSchemes = Map.insert k (Forall (nub (vars t')) t') (stSchemes s) }
        uses k g t
    link
    modify $ \s -> s { stMono = Map.empty }
  where
    -- The void pointer parameters whose type the function determined.
    uses (_, v) (Function (Fix (FunctionPrototype _ _ ps)) _) (TFun pts _ _) = do
        us <- fmap concat . forM (zip (fst (parameters ps)) pts) $ \case
            (Fix (VarDecl ty (L _ _ p) []), TPtr (TVar a)) | isVoidPtr ty -> do
                t' <- zonk (TPtr (TVar a))
                o <- gets (IntMap.lookup a . stOrigins)
                pure [ (p, t', origin) | Just origin <- [o], t' /= TPtr (TVar a) ]
            _ -> pure []
        unless (null us) $ modify (\s -> s { stUses = Map.insert v us (stUses s) })
    uses _ _ _ = pure ()

    isVoidPtr (Fix node) = case node of
        TyPointer t  -> isVoid t
        TyConst t    -> isVoidPtr t
        TyOwner t    -> isVoidPtr t
        TyNonnull t  -> isVoidPtr t
        TyNullable t -> isVoidPtr t
        _            -> False

-- | Links two void pointers of a struct if they still share a type variable
-- after the component is checked.
link :: M ()
link = do
    found <- gets stInstances
    forM_ found $ \(tag, args) -> do
        args' <- mapM zonk args
        let groups = Map.elems (Map.fromListWith (flip (++)) [ (a, [i]) | (i, TVar a) <- zip [0 ..] args' ])
        forM_ groups $ \case
            i : is@(_ : _) -> modify $ \s -> s { stLinks = Map.insertWith IntMap.union tag (IntMap.fromList (map (, i) is)) (stLinks s) }
            _ -> pure ()

-- | Program-wide facts: the linked void pointers of each struct, and the
-- callback void pointers that accept any type.
type Facts = (Map Text (IntMap Int), Set Hole)

check :: Program -> Facts -> ([Diagnostic CimplePos], Facts)
check prog (links, free) = (reverse (stDiags st) ++ misused, (Map.map canonical (stLinks st), free'))
  where
    -- Void pointers that shared a type variable form a class.
    classes = map flattenSCC $ stronglyConnComp
        [ (h, h, Set.toList hs) | (h, hs) <- Map.toList $ Map.fromListWith Set.union
            [ (h, hs) | hs <- IntMap.elems (stHoles st), h <- Set.toList hs ] ]

    -- Values stored in a void pointer that no code in its class uses as that type.
    misused = Map.elems $ Map.fromList
        [ ((pos, len), Diagnostic pos len ErrorLevel
            (code node <+> "is" <+> ppType (TPtr t) <> ", but it is used as" <+> ppType (TPtr (fst (Map.findMin used))))
            Nothing
            [ DiagnosticSpan upos ulen ["used as" <+> ppType (TPtr u) <+> "here"]
            | (u, (ufile, unode)) <- take 2 (Map.toList used), let (upos, ulen) = getDiagnosticInfo ufile unode ]
            [])
        | holes <- classes
        , let used = Map.unions (mapMaybe (`Map.lookup` stUsed st) holes)
        , not (Map.null used)
        , (t, (file, node)) <- concatMap (maybe [] Map.toList . (`Map.lookup` stGiven st)) holes
        , Map.notMember t used
        , let (pos, len) = getDiagnosticInfo file node
        , pos `notElem` map diagPos (stDiags st)
        ]

    free' = free `Set.union` Map.keysSet (Map.filter ((> 1) . Set.size) (stCalled st))
    st = execState (mapM_ component components) St
        { stProg = prog, stLinks = links, stFree = free, stCalled = Map.empty, stCallbacks = IntMap.empty, stHoles = IntMap.empty, stGiven = Map.empty, stUsed = Map.empty, stNext = 0, stSubst = IntMap.empty, stOrigins = IntMap.empty
        , stSchemes = Map.empty, stMono = Map.empty, stUses = Map.empty, stInstances = [], stDiags = []
        , stFile = "", stDefines = Map.empty, stLocals = Map.empty, stPaths = Map.empty
        , stReturn = TVoid, stName = "", stCall = Nothing, stTrail = Nothing, stCallee = Nothing
        }

    components = map flattenSCC $ stronglyConnComp
        [ ((k, g), k, uses k g) | (k, g) <- Map.toList (pGlobals prog) ]

    uses (_, _) g = concat
        [ [ k | k <- [(Just file, v), (Nothing, v)], Map.member k (pGlobals prog) ]
        | Body file defines _ body <- bodies g
        , v <- Set.toList (names (Map.union defines (pDefines prog)) body) ]

    bodies (Function _ bs) = bs
    bodies (Variable _ bs) = bs

    -- Map each void pointer directly to the first one of its group.
    canonical m = IntMap.map first' m
      where first' i = maybe i first' (IntMap.lookup i m)

analyse :: [(FilePath, [Ast])] -> [Diagnostic CimplePos]
analyse tus = go (Map.empty, Set.empty)
  where
    prog = collect tus
    go facts = case check prog facts of
        (diags, facts') | facts' == facts -> map flag diags
                        | otherwise       -> go facts'
    flag d = d { diagFlag = Just "type-check" }

descr :: ([(FilePath, [Ast])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("type-check", Text.unlines
    [ "Checks that every value has the type its use needs. A `void *` counts as a"
    , "pointer to one type, which all uses of it have to agree on:"
    , ""
    , "- a function that casts its `void *object` to `Foo *` has to be given a"
    , "  `Foo *`;"
    , "- a callback has to be passed or stored with an object of the type it casts"
    , "  its `void *` to, whether the two are function parameters or struct fields;"
    , "- an object stored in a struct's `void *` has to be of a type that some code"
    , "  uses that `void *` as."
    , ""
    , "A callback type whose `void *` is called with two different types takes any"
    , "type there, and a `uint8_t *` or `char *` can point at any object."
    , ""
    , "**Reason:** the compiler accepts any pointer as a `void *`, so it can't see a"
    , "callback registered with the wrong object."
    ]))
