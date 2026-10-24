{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | Finds `%s` conversions whose argument can be bytes with no NUL after
-- them, so that formatting it reads past the object they're in.
--
-- It reports only what it can show. The bytes the argument points to are a
-- copy of a runtime number of bytes, the copy can fill the object they're in
-- to its end, and nothing in the program writes a NUL into that object:
-- every use of it is one this understands. Where the argument is a
-- parameter, a caller shows it, and so on up. Anything else (an unknown use
-- of the object, a copy it can't bound, a function filling it) makes it say
-- nothing.
--
-- It also reports a call returning an unwritten buffer it was given, on a
-- path no caller decides, in builds where the `#if`s on the way can hold.
module Tokstyle.Linter.NulTerminated (descr) where

import           Control.Monad.State.Strict    (State, evalState, gets, modify)
import           Data.Fix                      (Fix (..))
import           Data.Foldable                 (toList)
import           Data.List                     (sortOn)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe, listToMaybe,
                                                mapMaybe, maybeToList)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Read                as Text
import           Language.Cimple               (AssignOp (..), BinaryOp (..),
                                                Lexeme (..), LiteralType (..),
                                                Node, NodeF (..), Scope (..),
                                                UnaryOp (..), lexemeText)
import           Language.Cimple.Diagnostics   (CimplePos, Diagnostic (..),
                                                DiagnosticLevel (..),
                                                DiagnosticSpan (..),
                                                nodePosAndLen)
import           Language.Cimple.Pretty        (ppNode)
import           Prettyprinter                 (Doc, concatWith, layoutCompact,
                                                pretty, unAnnotate, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle)
import           Prettyprinter.Render.Text     (renderStrict)
import           Tokstyle.Common               (backticks, semEq)

type TranslationUnit = (FilePath, [Node (Lexeme Text)])

type Expr = Node (Lexeme Text)

-- * The program

-- | A function: static ones by the file they're in.
data FuncKey = FuncKey (Maybe FilePath) Text
    deriving (Eq, Ord, Show)

data Func = Func
    { funcId     :: Int
      -- ^ Per definition: `#if`s can define a function more than once.
    , funcConds  :: [Cond]
    , funcFile   :: FilePath
    , funcKey    :: FuncKey
    , funcName   :: Text
    , funcParams :: [(Text, CType)]
    , funcBody   :: Expr
    , funcVars   :: Map Text Var
    }

-- | A preprocessor condition in a file, and whether it holds.
data Cond = Cond FilePath Bool Expr

definitions :: FilePath -> [Cond] -> Expr -> [([Cond], Expr)]
definitions file conds n = case unFix n of
    FunctionDefn{} -> [(conds, n)]
    PreprocDefine{} -> [(conds, n)]
    PreprocIf c ts e -> branch c True ts e
    PreprocElif c ts e -> branch c True ts e
    PreprocIfdef l ts e -> branch (Fix (PreprocDefined l)) True ts e
    PreprocIfndef l ts e -> branch (Fix (PreprocDefined l)) False ts e
    _ -> concatMap (definitions file conds) (toList (unFix n))
  where
    branch x holds ts e =
        concatMap (definitions file (Cond file holds x : conds)) ts
        ++ definitions file (Cond file (not holds) x : conds) e

-- | Whether some build makes all of `conds` hold. A file's `#define X` under
-- `#if E` makes `defined(X)` follow from `E`.
satisfiable :: Program -> [Cond] -> Bool
satisfiable prog conds = length atoms <= 16 && any holdsAll (mapM (const [True, False]) atoms)
  where
    atoms = Set.toList . Set.fromList $
        concatMap atomsOf ([ x | Cond _ _ x <- conds ] ++ [ x | Cond file _ _ <- conds, (_, dconds) <- defines file, Cond _ _ x <- dconds ])

    defines file = Map.findWithDefault [] file (progDefines prog)

    holdsAll values =
        let env = Map.fromList (zip atoms values)
            global a = Map.findWithDefault False a env
            definedIn file x = global ("defined " <> x)
                || or [ all (\(Cond _ b e) -> eval global e == Just b) dconds | (d, dconds) <- defines file, d == x ]
        in all (\(Cond file b e) -> eval (\a -> maybe (global a) (definedIn file) (Text.stripPrefix "defined " a)) e == Just b) conds

    eval atom (Fix e) = case e of
        PreprocDefined l      -> Just (atom ("defined " <> name l))
        ParenExpr x           -> eval atom x
        UnaryExpr UopNot x    -> not <$> eval atom x
        BinaryExpr a BopAnd b -> (&&) <$> eval atom a <*> eval atom b
        BinaryExpr a BopOr b  -> (||) <$> eval atom a <*> eval atom b
        LiteralExpr Int l     -> Just (name l /= "0")
        _                     -> Just (atom (render (Fix e)))

    atomsOf (Fix e) = case e of
        PreprocDefined l      -> ["defined " <> name l]
        ParenExpr x           -> atomsOf x
        UnaryExpr UopNot x    -> atomsOf x
        BinaryExpr a BopAnd b -> atomsOf a ++ atomsOf b
        BinaryExpr a BopOr b  -> atomsOf a ++ atomsOf b
        LiteralExpr Int _     -> []
        _                     -> [render (Fix e)]

    render = renderStrict . layoutCompact . unAnnotate . ppNode

-- | A local variable: its type, and initialiser.
data Var = Var
    { varType :: CType
    , varInit :: Maybe Expr
    }

-- | As much of a C type as this needs.
data CType
    = TByte
    | TChar
    | TStruct Text
    | TPtr Bool CType
      -- ^ Whether what it points to is const.
    | TArray CType (Maybe Expr)
      -- ^ With its size.
    | TOther

data CallSite = CallSite
    { csFunc :: Func
    , csArgs :: [Expr]
    }

data Program = Program
    { progFuncs    :: [Func]
    , progStatics  :: Set (FilePath, Text)
    , progCalls    :: Map Text [CallSite]
    , progStructs  :: Map Text (Map Text CType)
    , progTypedefs :: Map Text CType
    , progConsts   :: Map Text Expr
    , progProtos   :: Map Text [CType]
      -- ^ The parameter types of every function declared or defined.
    , progDefines  :: Map FilePath [(Text, [Cond])]
    , progPrintf   :: Map Text (Int, Int)
      -- ^ The format and first value of functions declared with
      -- `GNU_PRINTF`, from 0.
    }

universe :: Expr -> [Expr]
universe n = n : concatMap universe (toList (unFix n))

-- | Every node, with the nodes it's in, innermost first.
withParents :: Expr -> [(Expr, [Expr])]
withParents = go []
  where
    go ps n = (n, ps) : concatMap (go (n : ps)) (toList (unFix n))

name :: Lexeme Text -> Text
name = lexemeText

readInt :: Text -> Maybe Int
readInt t = case Text.decimal t of
    Right (n, rest) | Text.null rest -> Just n
    _                                -> Nothing

buildProgram :: [TranslationUnit] -> Program
buildProgram tus = Program
    { progFuncs = funcs
    , progStatics = Set.fromList
        [ (file, name fname)
        | (file, Fix (FunctionDefn Static (Fix (FunctionPrototype _ fname _)) _)) <- nodes
        ]
    , progCalls = Map.fromListWith (flip (++))
        [ (name callee, [CallSite f args])
        | f <- funcs
        , Fix (FunctionCall (Fix (VarExpr callee)) args) <- universe (funcBody f)
        ]
    , progStructs = Map.fromListWith Map.union
        [ (name sname, Map.fromList (mapMaybe member members))
        | (_, Fix (Struct sname members)) <- nodes
        ]
    , progTypedefs = typedefs
    , progConsts = Map.fromList $
        [ (name c, value) | (_, Fix (PreprocDefineConst c value)) <- nodes ]
        ++ [ (name c, value) | (_, Fix (Enumerator c (Just value))) <- nodes ]
        ++ [ (name c, value) | (_, Fix (ConstDefn _ _ c value)) <- nodes ]
    , progProtos = Map.fromList
        [ (name fname, [ withArrays (ty' ty) arrs | Fix (VarDecl ty _ arrs) <- params ])
        | (_, Fix (FunctionPrototype _ fname params)) <- nodes
        ]
    , progDefines = Map.fromListWith (flip (++))
        [ (file, [(name d, conds)])
        | (file, top) <- tus, t <- top, (conds, Fix (PreprocDefine d)) <- definitions file [] t
        ]
    , progPrintf = Map.fromList
        [ (name fname, (fmt - 1, args - 1))
        | (_, Fix (AttrPrintf fmtL argsL decl)) <- nodes
        , Just fmt <- [readInt (name fmtL)]
        , Just args <- [readInt (name argsL)]
        , Fix (FunctionPrototype _ fname _) <- universe decl
        ]
    }
  where
    nodes = [ (file, n) | (file, top) <- tus, t <- top, n <- universe t ]

    ty' = cimpleType typedefs

    typedefs = Map.fromList $
        [ (name tname, TStruct (name sname))
        | (_, Fix (Typedef (Fix (Struct sname _)) tname _)) <- nodes ]
        ++ [ (name tname, TStruct (name sname))
           | (_, Fix (Typedef (Fix (TyStruct sname)) tname _)) <- nodes ]
        ++ [ (name tname, cimpleType Map.empty ty)
           | (_, Fix (Typedef ty tname _)) <- nodes
           , not (isStruct ty)
           ]
    isStruct = \case
        Fix Struct{}   -> True
        Fix TyStruct{} -> True
        _              -> False

    member (Fix (MemberDecl (Fix (VarDecl ty fname arrs)) _)) =
        Just (name fname, withArrays (ty' ty) arrs)
    member _ = Nothing

    funcs =
        [ Func
            { funcId = i
            , funcConds = conds
            , funcFile = file
            , funcKey = FuncKey (if scope == Static then Just file else Nothing) (name fname)
            , funcName = name fname
            , funcParams = [ (name p, withArrays (ty' ty) arrs) | Fix (VarDecl ty p arrs) <- params ]
            , funcBody = body
            , funcVars = locals params body
            }
        | (i, (file, conds, Fix (FunctionDefn scope (Fix (FunctionPrototype _ fname params)) body))) <-
            zip [0 ..] [ (file, conds, d) | (file, top) <- tus, t <- top, (conds, d@(Fix FunctionDefn{})) <- definitions file [] t ]
        ]

    -- A name declared more than once in a function, or also a parameter,
    -- is left out: which one a use means isn't known.
    locals params body =
        let decls = [ (name v, Var (withArrays (ty' ty) arrs) i)
                    | Fix (VarDeclStmt (Fix (VarDecl ty v arrs)) i) <- universe body ]
                 ++ [ (name v, Var (TArray (ty' ty) (Just size)) Nothing)
                    | Fix (VLA ty v size) <- universe body ]
            counts = Map.fromListWith (+) $
                [ (v, 1 :: Int) | (v, _) <- decls ]
                ++ [ (name p, 1) | Fix (VarDecl _ p _) <- params ]
        in Map.fromList [ d | d@(v, _) <- decls, Map.lookup v counts == Just 1 ]

withArrays :: CType -> [Expr] -> CType
withArrays = foldr arr
  where
    arr (Fix (DeclSpecArray _ size)) t = TArray t size
    arr _ t                            = t

cimpleType :: Map Text CType -> Expr -> CType
cimpleType typedefs (Fix ty) = case ty of
    TyConst t       -> cimpleType typedefs t
    TyNonnull t     -> cimpleType typedefs t
    TyNullable t    -> cimpleType typedefs t
    TyOwner t       -> cimpleType typedefs t
    TyBitwise t     -> cimpleType typedefs t
    TyForce t       -> cimpleType typedefs t
    TyPointer t     -> TPtr (isConst t) (cimpleType typedefs t)
    TyStd t | name t == "uint8_t" -> TByte
            | name t == "char" -> TChar
    TyStruct s      -> TStruct (name s)
    TyUserDefined t -> fromMaybe TOther (Map.lookup (name t) typedefs)
    _               -> TOther
  where
    isConst = \case
        Fix (TyConst _)    -> True
        Fix (TyNonnull t)  -> isConst t
        Fix (TyNullable t) -> isConst t
        Fix (TyOwner t)    -> isConst t
        _                  -> False

keyFor :: Program -> FilePath -> Text -> FuncKey
keyFor prog file fn
    | Set.member (file, fn) (progStatics prog) = FuncKey (Just file) fn
    | otherwise = FuncKey Nothing fn

callersOf :: Program -> Func -> [CallSite]
callersOf prog f =
    [ cs
    | cs <- Map.findWithDefault [] (funcName f) (progCalls prog)
    , keyFor prog (funcFile (csFunc cs)) (funcName f) == funcKey f
    ]

definitionsOf :: Program -> FilePath -> Text -> [Func]
definitionsOf prog file fn =
    [ g | g <- progFuncs prog, funcName g == fn, funcKey g == keyFor prog file fn ]

-- * Expressions

stripCasts :: Expr -> Expr
stripCasts = \case
    Fix (ParenExpr e)  -> stripCasts e
    Fix (CastExpr _ e) -> stripCasts e
    e                  -> e

typeIn :: Program -> Func -> Expr -> CType
typeIn prog f (Fix e) = case e of
    VarExpr v -> case Map.lookup (name v) (funcVars f) of
        Just var -> varType var
        Nothing  -> fromMaybe TOther (lookup (name v) (funcParams f))
    ParenExpr x -> typeIn prog f x
    CastExpr ty _ -> cimpleType (progTypedefs prog) ty
    PointerAccess b fld -> case typeIn prog f b of
        TPtr _ (TStruct s)   -> fieldType prog s (name fld)
        TArray (TStruct s) _ -> fieldType prog s (name fld)
        _                    -> TOther
    MemberAccess b fld -> case typeIn prog f b of
        TStruct s -> fieldType prog s (name fld)
        _         -> TOther
    ArrayAccess b _ -> element (typeIn prog f b)
    UnaryExpr UopDeref x -> element (typeIn prog f x)
    UnaryExpr UopAddress x -> TPtr False (typeIn prog f x)
    BinaryExpr a BopPlus _ -> typeIn prog f a
    _ -> TOther
  where
    element = \case
        TPtr _ t   -> t
        TArray t _ -> t
        _          -> TOther

fieldType :: Program -> Text -> Text -> CType
fieldType prog s fld = fromMaybe TOther $ Map.lookup fld =<< Map.lookup s (progStructs prog)

-- | The struct field an expression is: `x->f` or `x.f`, by struct and field.
fieldOf :: Program -> Func -> Expr -> Maybe (Text, Text)
fieldOf prog f e = case unFix e of
    PointerAccess b fld -> case typeIn prog f b of
        TPtr _ (TStruct s)   -> Just (s, name fld)
        TArray (TStruct s) _ -> Just (s, name fld)
        _                    -> Nothing
    MemberAccess b fld -> case typeIn prog f b of
        TStruct s -> Just (s, name fld)
        _         -> Nothing
    _ -> Nothing

evalConst :: Program -> Expr -> Maybe Integer
evalConst prog = go (32 :: Int)
  where
    go 0 _ = Nothing
    go d (Fix e) = case e of
        LiteralExpr Int l     -> toInteger <$> readInt (name l)
        LiteralExpr ConstId l -> go (d - 1) =<< Map.lookup (name l) (progConsts prog)
        ParenExpr x           -> go d x
        CastExpr _ x          -> go d x
        BinaryExpr a BopPlus b  -> (+) <$> go d a <*> go d b
        BinaryExpr a BopMinus b -> (-) <$> go d a <*> go d b
        BinaryExpr a BopMul b   -> (*) <$> go d a <*> go d b
        _                     -> Nothing

-- | Whether two expressions are the same number.
sameValue :: Program -> Expr -> Expr -> Bool
sameValue prog a b = semEq a b || case (evalConst prog a, evalConst prog b) of
    (Just x, Just y) -> x == y
    _                -> False

isNull :: Expr -> Bool
isNull e = case unFix (stripCasts e) of
    LiteralExpr ConstId l -> name l `elem` ["nullptr", "NULL"]
    LiteralExpr Int l     -> name l == "0"
    _                     -> False

callName :: Expr -> Maybe Text
callName = \case
    Fix (VarExpr f)             -> Just (name f)
    Fix (LiteralExpr ConstId f) -> Just (name f)
    _                           -> Nothing

-- | The size an allocation call asks for.
allocationSize :: Expr -> Maybe Expr
allocationSize e = case unFix (stripCasts e) of
    FunctionCall callee args -> case (callName callee, args) of
        (Just "mem_balloc", [_, size]) -> Just size
        (Just "mem_alloc", [_, size])  -> Just size
        (Just "malloc", [size])        -> Just size
        _                              -> Nothing
    _ -> Nothing

-- * Uses

-- | What one use of an object does with it.
data Use
    = Copy Expr Expr
      -- ^ `memcpy(it, src, n)`: the call, and `n`.
    | Store Expr Expr
      -- ^ `it = value`: the assignment, and the value.
    | Read
      -- ^ Reads it, or keeps, compares or frees the pointer, but doesn't
      -- write through it.
    | Unknown
      -- ^ Anything else: writing a byte of it, or giving it to a function
      -- that may.

-- | Functions that read what they're given, or free it.
readers :: [Text]
readers = ["memcmp", "memcpy", "memmove", "mem_delete", "free", "crypto_memcmp", "strlen", "strnlen"]

-- | What each use, in `f`, of the object the nodes `isIt` picks name does.
usesIn :: Program -> Func -> (Expr -> Bool) -> [Use]
usesIn prog f isIt =
    [ classify n (dropWrappers ps) | (n, ps) <- withParents (funcBody f), isIt n ]
  where
    dropWrappers = \case
        Fix ParenExpr{} : ps -> dropWrappers ps
        Fix CastExpr{} : ps  -> dropWrappers ps
        ps                   -> ps

    classify n = \case
        c@(Fix (FunctionCall callee args)) : _ -> call n c callee (map stripCasts args)
        assign@(Fix (AssignExpr lhs AopEq rhs)) : _
            | stripCasts lhs == n -> Store assign rhs
            | otherwise -> Read
        Fix (AssignExpr lhs _ _) : _ | stripCasts lhs == n -> Unknown
        access@(Fix (ArrayAccess base _)) : ps | stripCasts base == n -> case dropWrappers ps of
            Fix (AssignExpr lhs _ _) : _ | stripCasts lhs == access -> Unknown
            Fix (UnaryExpr op _) : _ | op `elem` [UopAddress, UopIncr, UopDecr] -> Unknown
            _ -> Read
        Fix ArrayAccess{} : _ -> Read
        Fix (BinaryExpr _ op _) : _ | op `elem` [BopEq, BopNe, BopAnd, BopOr] -> Read
        Fix (UnaryExpr UopNot _) : _ -> Read
        Fix (IfStmt c _ _) : _ | stripCasts c == n -> Read
        Fix SizeofExpr{} : _ -> Read
        Fix (VarDeclStmt (Fix (VarDecl ty _ _)) _) : _ | TPtr True _ <- cimpleType (progTypedefs prog) ty -> Read
        _ -> Unknown

    call n c callee args = case callName callee of
        Just "memcpy" | [dst, _, size] <- args, dst == n -> Copy c size
        Just fn
            | fn `elem` readers -> Read
            | Just (fmt, _) <- formatOf prog fn
            , all (>= fmt) [ i | (i, a) <- zip [0 ..] args, a == n ] -> Read
            | Just params <- Map.lookup fn (progProtos prog)
            , [i] <- [ i | (i, a) <- zip [0 :: Int ..] args, a == n ]
            , TPtr True _ <- fromMaybe TOther (listToMaybe (drop i params)) -> Read
        _ -> Unknown

isVar :: Text -> Expr -> Bool
isVar v = \case
    Fix (VarExpr v') -> name v' == v
    _                -> False

-- * Proofs

-- | A step of a proof: a place, and what it shows.
data Step = Step FilePath Expr (Doc AnsiStyle)

data Key = KParam FuncKey Int | KField Text Text
    deriving (Eq, Ord)

data UnwrittenKey = UnwrittenKey Int Int (Maybe Text)
    deriving (Eq, Ord)

type Proof = ([Step], [Cond])

data Memo = Memo
    { memoUnwritten  :: Map UnwrittenKey (Maybe Proof)
    , memoBusy       :: Set UnwrittenKey
    , memoDone       :: Map Key (Maybe [Step])
    , memoInProgress :: Set Key
    }

type M = State Memo

remember :: Key -> M (Maybe [Step]) -> M (Maybe [Step])
remember key act = do
    done <- gets (Map.lookup key . memoDone)
    busy <- gets (Set.member key . memoInProgress)
    case done of
        Just r -> pure r
        Nothing | busy -> pure Nothing
        Nothing -> do
            modify $ \m -> m{memoInProgress = Set.insert key (memoInProgress m)}
            r <- act
            modify $ \m -> m{memoDone = Map.insert key r (memoDone m), memoInProgress = Set.delete key (memoInProgress m)}
            pure r

firstJust :: [M (Maybe a)] -> M (Maybe a)
firstJust [] = pure Nothing
firstJust (x:xs) = x >>= \case
    Just r  -> pure (Just r)
    Nothing -> firstJust xs

maxDepth :: Int
maxDepth = 12

-- | How the bytes `e` points to, in `f`, can have no NUL after them before
-- the end of the object they're in: the steps that show it.
unterminated :: Program -> Int -> Func -> Expr -> M (Maybe [Step])
unterminated _ 0 _ _ = pure Nothing
unterminated prog depth f e0 = case unFix e of
    VarExpr v
        | Just var <- Map.lookup (name v) (funcVars f) -> local (name v) var
        | Just i <- lookup (name v) (zip (map fst (funcParams f)) [0 ..]) -> param (name v) i
    -- What's after the start of such bytes is such bytes too.
    BinaryExpr base BopPlus _ -> unterminated prog depth f base
    UnaryExpr UopAddress (Fix (ArrayAccess base _)) -> unterminated prog depth f base
    _ | Just fld <- fieldOf prog f e -> field fld
    _ -> pure Nothing
  where
    e = stripCasts e0
    file = funcFile f
    next = unterminated prog (depth - 1)

    -- A parameter: shown by a caller, if `f` doesn't change it or write
    -- what it points to. Nothing writes through a pointer to const.
    param v i
        | any (not . unchanged) (usesIn prog f (isVar v)) = pure Nothing
        | otherwise = remember (KParam (funcKey f) i) $ firstJust
            [ fmap (Step (funcFile (csFunc cs)) arg ("passed to" <+> backticks (pretty (funcName f)) <+> "as" <+> backticks (pretty v) <+> "here") :)
                <$> next (csFunc cs) arg
            | cs <- sortOn (funcFile . csFunc) (callersOf prog f)
            , arg <- take 1 (drop i (csArgs cs))
            ]

    unchanged = \case
        Read                               -> True
        Unknown | TPtr True _ <- paramType -> True
        _                                  -> False
      where
        paramType = fromMaybe TOther (lookup (nameOf e) (funcParams f))
        nameOf = \case
            Fix (VarExpr l) -> name l
            _               -> ""

    local v var = case varType var of
        TArray _ (Just size) -> pure $ filled v (usesIn prog f (isVar v)) size
        TPtr toConst _ -> case (usesIn prog f (isVar v), values v var) of
            (uses, [value]) | toConst || all knownPointerUse uses -> case allocationSize value of
                Just size -> pure $ filled v uses size
                Nothing -> fmap (Step file value (backticks (pretty v) <+> "is" <+> backticks (ppNode value) <+> "here") :)
                    <$> next f value
            _ -> pure Nothing
        _ -> pure Nothing

    values v var = filter (not . isNull) $
        maybeToList (varInit var) ++ [ value | Store _ value <- usesIn prog f (isVar v) ]

    knownPointerUse = \case
        Unknown -> False
        _       -> True

    -- An object filled to its `size` by a copy, and not otherwise written.
    filled v uses size
        | any isUnknown uses = Nothing
        | otherwise = listToMaybe
            [ [ Step file copy ("filled with" <+> backticks (ppNode n) <+> "bytes here, as many as" <+> backticks (pretty v) <+> "holds")
              ] ++ guardSteps
            | Copy copy n <- uses
            , copiesBytes prog f copy
            , Just guardSteps <- [reaches prog f copy n size]
            ]

    isUnknown = \case
        Unknown -> True
        _       -> False

    -- A field: every use of it in the program understood, and one that
    -- fills it.
    field (s, fld) = remember (KField s fld) $ do
        let uses = [ (g, u) | g <- progFuncs prog, u <- usesIn prog g (isField g) ]
            isField g n = case unFix n of
                PointerAccess{} -> fieldOf prog g n == Just (s, fld)
                MemberAccess{}  -> fieldOf prog g n == Just (s, fld)
                _               -> False
            describe = backticks (pretty s <> "." <> pretty fld)
        if any (isUnknown . snd) uses then pure Nothing else case fieldType prog s fld of
            TArray _ (Just size) -> pure $ listToMaybe
                [ Step (funcFile g) copy ("filled with" <+> backticks (ppNode n) <+> "bytes here, as many as" <+> describe <+> "holds") : guardSteps
                | (g, Copy copy n) <- uses
                , copiesBytes prog g copy
                , Just guardSteps <- [reaches prog g copy n size]
                ]
            TPtr _ _ -> firstJust
                [ fmap (Step (funcFile g) assign ("stored in" <+> describe <+> "here") :) <$> next g value
                | (g, Store assign value) <- uses
                , not (isNull value)
                ]
            _ -> pure Nothing

-- | Whether the copy `copy` of `n` bytes, in `f`, can fill `size` bytes: `n`
-- is `size`, or is only kept from being more than it.
reaches :: Program -> Func -> Expr -> Expr -> Expr -> Maybe [Step]
reaches prog f copy n size
    | sameValue prog n size = Just []
    | Fix (VarExpr v) <- stripCasts n = listToMaybe
        [ [ Step (funcFile f) guard (backticks (pretty (name v)) <+> "can be" <+> backticks (ppNode bound) <+> "after this") ]
        | Fix (IfStmt cond body _) <- map uncomment (earlier copy)
        , exits body
        , guard <- disjuncts cond
        , Just bound <- [atMost (name v) guard]
        , sameValue prog bound size
        ]
    | otherwise = Nothing
  where
    uncomment = \case
        Fix (Commented _ stmt) -> uncomment stmt
        stmt                   -> stmt

    -- The statements before `node` in the blocks it's in: run before it.
    earlier node =
        [ stmt
        | (found, ps) <- take 1 [ p | p@(n', _) <- withParents (funcBody f), n' == node ]
        , (child, Fix (CompoundStmt stmts)) <- zip (found : ps) ps
        , stmt <- takeWhile (/= child) stmts
        ]

    atMost v (Fix (BinaryExpr a BopGt b)) | isVar v (stripCasts a) = Just b
    atMost v (Fix (BinaryExpr a BopLt b)) | isVar v (stripCasts b) = Just a
    atMost _ _ = Nothing

    disjuncts x = case unFix (stripCasts x) of
        BinaryExpr a BopOr b -> disjuncts a ++ disjuncts b
        _                    -> [stripCasts x]

    exits = \case
        Fix (CompoundStmt stmts) | not (null stmts) -> exits (last stmts)
        Fix Return{}                                -> True
        Fix Goto{}                                  -> True
        _                                           -> False


-- | Whether the `memcpy` call `copy`, in `f`, copies from bytes, not text,
-- which could bring a NUL of its own.
copiesBytes :: Program -> Func -> Expr -> Bool
copiesBytes prog f = \case
    Fix (FunctionCall _ [_, src, _]) -> case typeIn prog f (stripCasts src) of
        TPtr _ TByte   -> True
        TArray TByte _ -> True
        _              -> False
    _ -> False

-- * Bytes nothing writes

-- | How `arg`, in `f`, can point to bytes nothing wrote.
unwritten :: Program -> Func -> Expr -> M (Maybe Proof)
unwritten prog f arg = case unFix (stripCasts arg) of
    FunctionCall callee args | Just fn <- callName callee -> firstJust
        [ fmap (\(steps, conds) -> (Step (funcFile f) a (backticks (pretty o) <+> "is passed here before anything writes it") : Step (funcFile g) ret ("returning" <+> backticks (ppNode retVal)) : steps, funcConds f ++ inside ++ conds))
            <$> unwrittenIn prog g i fld
        | g <- definitionsOf prog (funcFile f) fn
        , Just (i, fld, ret, retVal) <- [returnsParam prog g]
        , a <- take 1 (drop i args)
        , Just o <- [fresh fld (stripCasts a)]
        ]
    _ -> pure Nothing
  where
    inside =
        [ Cond (funcFile f) holds c
        | (found, ps) <- take 1 [ x | x@(n, _) <- withParents (funcBody f), n == arg ]
        , (child, parent) <- zip (found : ps) ps
        , (c, ts, holds') <- case unFix parent of
            PreprocIf c ts _     -> [(c, ts, True)]
            PreprocElif c ts _   -> [(c, ts, True)]
            PreprocIfdef l ts _  -> [(Fix (PreprocDefined l), ts, True)]
            PreprocIfndef l ts _ -> [(Fix (PreprocDefined l), ts, False)]
            _                    -> []
        , let holds = if child `elem` ts then holds' else not holds'
        ]

    fresh fld a = case (fld, unFix a) of
        (Just _, UnaryExpr UopAddress v@(Fix (VarExpr o)))
            | Just (TStruct _, decl) <- declaration (name o) v, unusedSince decl v -> Just (name o)
        (Nothing, VarExpr o)
            | Just (TArray{}, decl) <- declaration (name o) a, unusedSince decl a -> Just (name o)
        _ -> Nothing

    declaration o use = case
        [ (ty, arrs, i, d)
        | (found, ps) <- take 1 [ x | x@(n, _) <- withParents (funcBody f), n == use ]
        , (child, Fix (CompoundStmt stmts)) <- zip (found : ps) ps
        , d@(Fix (VarDeclStmt (Fix (VarDecl ty v arrs)) i)) <- reverse (takeWhile (/= child) stmts)
        , name v == o
        ] of
        (ty, arrs, Nothing, d) : _ -> Just (withArrays (cimpleType (progTypedefs prog) ty) arrs, d)
        _                          -> Nothing

    unusedSince decl use = null
        [ n
        | n <- universe (funcBody f)
        , isVar (varName use) n
        , n /= use
        , pos decl < pos n, pos n < pos use
        ]
    varName = \case
        Fix (VarExpr v) -> name v
        _               -> ""
    pos = fst . nodePosAndLen ""

-- | The parameter (and field) every `return` of `g` returns.
returnsParam :: Program -> Func -> Maybe (Int, Maybe Text, Expr, Expr)
returnsParam prog g = case [ (r, v) | r@(Fix (Return (Just v))) <- universe (funcBody g) ] of
    returns@((ret, retVal) : _) -> case map (aliasOf . stripCasts . snd) returns of
        Just alias : rest | all (== Just alias) rest -> let (i, fld) = alias in Just (i, fld, ret, retVal)
        _ -> Nothing
    [] -> Nothing
  where
    params = zip (map fst (funcParams g)) [0 ..]
    aliasOf = \case
        Fix (VarExpr p) -> (\i -> (i, Nothing)) <$> lookup (name p) params
        Fix (PointerAccess (Fix (VarExpr p)) fld)
            | Just i <- lookup (name p) params
            , TPtr _ (TStruct s) <- snd (funcParams g !! i)
            , TArray{} <- fieldType prog s (name fld) -> Just (i, Just (name fld))
        _ -> Nothing

-- | How `g` can return without writing what parameter `i` (field `fld`)
-- points to, on a path no caller decides.
unwrittenIn :: Program -> Func -> Int -> Maybe Text -> M (Maybe Proof)
unwrittenIn prog g i fld = case drop i (funcParams g) of
    [] -> pure Nothing
    (p, _) : _ -> do
        let key = UnwrittenKey (funcId g) i fld
        done <- gets (Map.lookup key . memoUnwritten)
        busy <- gets (Set.member key . memoBusy)
        case done of
            Just r -> pure r
            Nothing | busy -> pure Nothing
            Nothing -> do
                modify $ \m -> m{memoBusy = Set.insert key (memoBusy m)}
                r <- fmap (\(steps, conds) -> (steps, funcConds g ++ conds)) <$> through p [funcBody g] (pure (Just mempty))
                modify $ \m -> m{memoUnwritten = Map.insert key r (memoUnwritten m), memoBusy = Set.delete key (memoBusy m)}
                pure r
  where
    file = funcFile g

    through :: Text -> [Expr] -> M (Maybe Proof) -> M (Maybe Proof)
    through _ [] k = k
    through p (st : ss) k = case unFix st of
        Commented _ st' -> through p (st' : ss) k
        CompoundStmt xs -> through p xs (through p ss k)
        ExprStmt e -> effects p e `andThen` through p ss k
        VarDeclStmt (Fix (VarDecl ty _ _)) (Just e)
            | aliases p e, not (readOnly ty) -> pure Nothing
            | otherwise -> effects p e `andThen` through p ss k
        Return (Just e) -> effects p e `andThen` pure (Just mempty)
        Return Nothing -> pure (Just mempty)
        IfStmt c t e -> effects p c `andThen` do
            rest <- through p ss k
            if untouched p st then pure rest
            else if uncontrolled c then firstJust
                [ through p (maybeToList e) (pure rest) `withStep` Step file c (backticks (ppNode c) <+> "can be false here")
                , through p [t] (pure rest) `withStep` Step file c (backticks (ppNode c) <+> "can be true here")
                ]
            else pure Nothing
        _ | untouched p st -> through p ss k
        _ -> pure Nothing

    withStep act st = fmap (\(steps, conds) -> (st : steps, conds)) <$> act

    andThen a b = a >>= \case
        Nothing -> pure Nothing
        Just x  -> fmap (x <>) <$> b

    readOnly ty = case cimpleType (progTypedefs prog) ty of
        TPtr True _ -> True
        _           -> False

    into p x = case (fld, unFix (stripCasts x)) of
        (Nothing, VarExpr v) -> name v == p
        (Just f, PointerAccess (Fix (VarExpr v)) f') -> name v == p && name f' == f
        (_, BinaryExpr base BopPlus _) -> into p base
        (_, UnaryExpr UopAddress (Fix (ArrayAccess base _))) -> into p base
        _ -> False
    whole p x = case (fld, unFix (stripCasts x)) of
        (Just _, VarExpr v) -> name v == p
        _                   -> False
    aliases p x = into p x || whole p x

    writes p lhs = case unFix (stripCasts lhs) of
        VarExpr v               -> name v == p
        ArrayAccess base _      -> into p base
        UnaryExpr UopDeref base -> aliases p base
        _                       -> False

    untouched p st = and
        [ case unFix n of
            AssignExpr lhs _ _ -> not (writes p lhs)
            UnaryExpr op x | op `elem` [UopIncr, UopDecr] -> not (writes p x)
            FunctionCall _ args -> not (any (aliases p) args)
            VarDeclStmt (Fix (VarDecl ty _ _)) (Just e) -> readOnly ty || not (aliases p e)
            _ -> True
        | n <- universe st
        ]

    -- On `errno`, or on what functions outside the program returned.
    uncontrolled c = case unFix (stripCasts c) of
        LiteralExpr{} -> True
        VarExpr v
            | name v == "errno" -> True
            | Just (Var _ (Just e)) <- Map.lookup (name v) (funcVars g) -> outside e && not (assigned (name v))
        FunctionCall{} -> outside c
        x -> not (null x) && all uncontrolled x
    outside e = case unFix (stripCasts e) of
        FunctionCall callee args | Just fn <- callName callee -> null (definitionsOf prog file fn) && all uncontrolled args
        _ -> False
    assigned v = not (null [ () | Fix (AssignExpr lhs _ _) <- universe (funcBody g), isVar v lhs ])

    effects p e = foldr (andThen . effect p) (pure (Just mempty)) (universe e)

    effect p n = case unFix n of
        AssignExpr lhs _ _ | writes p lhs -> pure Nothing
        UnaryExpr op x | op `elem` [UopIncr, UopDecr], writes p x -> pure Nothing
        FunctionCall callee args -> foldr andThen (pure (Just mempty))
            [ passed n callee j a | (j, a) <- zip [0 ..] args, aliases p a ]
        _ -> pure (Just mempty)
      where
        passed call callee j a = case callName callee of
            Just fn
                | fn `elem` ["strlen", "strnlen", "memcmp", "crypto_memcmp", "assert"] -> pure (Just mempty)
                | fn `elem` ["memcpy", "memmove"], j /= 0 -> pure (Just mempty)
                | Just (fmt, _) <- formatOf prog fn, j >= fmt -> pure (Just mempty)
                | fn == "strerror_r", j == 1, gnu call -> pure $ Just
                    ([ Step file call ("with `_GNU_SOURCE`, this can return its own string without writing" <+> backticks (ppNode a)) ], [])
                | variants@(_ : _) <- definitionsOf prog file fn -> firstJust
                    [ fmap (\(steps, conds) -> (Step file a ("passed to" <+> backticks (pretty fn) <+> "here") : steps, conds))
                        <$> unwrittenIn prog h j (if into p a then Nothing else fld)
                    | h <- variants
                    ]
            _ -> pure Nothing

    -- GNU `strerror_r` returns a string; the other one returns an int.
    gnu call = or
        [ stripCasts e == call && pointer ty
        | Fix (VarDeclStmt (Fix (VarDecl ty _ _)) (Just e)) <- universe (funcBody g)
        ]
    pointer ty = case cimpleType (progTypedefs prog) ty of
        TPtr _ _ -> True
        _        -> False

-- * Formats

-- | Where the format and the first value are, in calls of `fn`.
formatOf :: Program -> Text -> Maybe (Int, Int)
formatOf prog fn
    | fn `elem` ["LOGGER_TRACE", "LOGGER_DEBUG", "LOGGER_INFO", "LOGGER_WARNING", "LOGGER_ERROR", "LOGGER_FATAL"] = Just (1, 2)
    | fn == "LOGGER_ASSERT" = Just (2, 3)
    | fn == "printf" = Just (0, 1)
    | fn `elem` ["fprintf", "sprintf"] = Just (1, 2)
    | fn == "snprintf" = Just (2, 3)
    | otherwise = Map.lookup fn (progPrintf prog)

-- | The values, by their place after the format, that `%s` conversions
-- without a precision read; `Nothing` for a format this doesn't follow.
unboundedStrings :: Text -> Maybe [Int]
unboundedStrings = go 0 . Text.unpack
  where
    go _ [] = Just []
    go i ('%':'%':rest) = go i rest
    go i ('%':rest) =
        let afterFlags = dropWhile (`elem` ("-+ #0'" :: String)) rest
            (i', afterWidth) = number i afterFlags
            (precise, i'', afterPrecision) = case afterWidth of
                '.':r -> let (j, r') = number i' r in (True, j, r')
                r     -> (False, i', r)
            afterLength = dropWhile (`elem` ("hlLqjzt" :: String)) afterPrecision
        in case afterLength of
            c:r | c == 's' -> ([ i'' | not precise ] ++) <$> go (i'' + 1) r
                | c `elem` ("diouxXcpeEfFgGaAn" :: String) -> go (i'' + 1) r
            _ -> Nothing
    go i (_:rest) = go i rest

    -- A width or precision: `*` takes a value.
    number i = \case
        '*':r -> (i + 1, r)
        r     -> (i, dropWhile (`elem` ['0' .. '9']) r)

-- | The arguments of `%s` conversions without a precision, in `f`.
stringArgs :: Program -> Func -> [(Expr, Expr)]
stringArgs prog f =
    [ (call, arg)
    | call@(Fix (FunctionCall callee args)) <- universe (funcBody f)
    , Just fn <- [callName callee]
    , Just (fmtAt, valuesAt) <- [formatOf prog fn]
    , Fix (LiteralExpr String fmt) <- take 1 (drop fmtAt args)
    , Just strings <- [unboundedStrings (Text.drop 1 (Text.dropEnd 1 (name fmt)))]
    , i <- strings
    , arg <- take 1 (drop (valuesAt + i) args)
    ]

-- * The linter

analyse :: [TranslationUnit] -> [Diagnostic CimplePos]
analyse tus = flip evalState (Memo Map.empty Set.empty Map.empty Set.empty) $ concat <$> sequence
    [ unterminated prog maxDepth f arg >>= \case
        Just steps -> pure [diagnostic f arg steps]
        Nothing -> unwritten prog f arg >>= \case
            Just (steps, conds) | satisfiable prog conds -> pure [unwrittenDiagnostic f arg steps conds]
            _ -> pure []
    | f <- sortOn funcFile (progFuncs prog)
    , (_, arg) <- stringArgs prog f
    ]
  where
    prog = buildProgram tus

diagnostic :: Func -> Expr -> [Step] -> Diagnostic CimplePos
diagnostic f arg steps =
    Diagnostic pos len WarningLevel
        ("`%s` reads" <+> backticks (ppNode arg) <+> "up to a NUL byte, and there can be none before the end of the object it points into")
        (Just "nul-terminated")
        (DiagnosticSpan pos len ["read up to a NUL here"]
            : [ DiagnosticSpan p l [note] | Step file n note <- steps, let (p, l) = nodePosAndLen file n ])
        [ (NoteLevel, "its bytes are a copy that can fill the object to its end, and nothing writes a NUL into it")
        , (HelpLevel, "print it with `%.*s` and its length, or don't print its bytes")
        ]
  where
    (pos, len) = nodePosAndLen (funcFile f) arg

unwrittenDiagnostic :: Func -> Expr -> [Step] -> [Cond] -> Diagnostic CimplePos
unwrittenDiagnostic f arg steps conds =
    Diagnostic pos len WarningLevel
        ("`%s` reads" <+> backticks (ppNode arg) <+> "up to a NUL byte, and those bytes can be ones nothing wrote")
        (Just "nul-terminated")
        (DiagnosticSpan pos len ["read up to a NUL here"]
            : [ DiagnosticSpan p l [note] | Step file n note <- steps, let (p, l) = nodePosAndLen file n ])
        ([ (NoteLevel, "they're whatever was in that memory, which may have no NUL before its end") ]
         ++ [ (NoteLevel, "this is where" <+> concatWith (\a b -> a <+> "and" <+> b) (map cond (dedupe conds))) | not (null conds) ]
         ++ [ (HelpLevel, "write the bytes on every path, or return the string that was written") ])
  where
    (pos, len) = nodePosAndLen (funcFile f) arg
    cond (Cond _ True c)  = backticks (ppNode c) <+> "holds"
    cond (Cond _ False c) = backticks (ppNode c) <+> "doesn't"
    dedupe = foldr (\c@(Cond _ a x) acc -> if any (\(Cond _ b y) -> a == b && semEq x y) acc then acc else c : acc) []

descr :: ([TranslationUnit] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("nul-terminated", Text.unlines
    [ "Checks that `%s` conversions get NUL-terminated strings."
    , ""
    , "It warns where it can show that the argument's bytes can have no NUL before"
    , "the end of the object they're in: they're a copy of a runtime number of"
    , "bytes, that copy can fill the object to its end, and nothing writes a NUL"
    , "into it. Where the argument is a parameter, a caller shows it."
    , ""
    , "It also warns where the argument is a call returning a buffer it was given,"
    , "the caller gives it one nothing has written, and the function has a path"
    , "that doesn't write it, decided only by things like `errno` (e.g. GNU"
    , "`strerror_r`, which can return its own string). It says which `#if`s have"
    , "to hold for that, and says nothing where they can't hold together."
    , ""
    , "**Reason:** `%s` reads until it finds a NUL byte, past the end of the object"
    , "if there's none in it. Byte arrays with a length, such as names and"
    , "messages from the network, have no NUL of their own, and memory nothing"
    , "wrote holds whatever was there before."
    ]))
