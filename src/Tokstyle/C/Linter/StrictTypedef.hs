{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE ViewPatterns      #-}
module Tokstyle.C.Linter.StrictTypedef (descr) where

import           Data.Functor.Identity           (Identity)
import           Data.List                       (find, intercalate, isInfixOf,
                                                  zip4)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (mapMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.IO                    as Text
import qualified Data.Text.Lazy                  as TL
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemError    (typeMismatch)
import           Language.C.Analysis.SemRep      (CompType (..),
                                                  CompTypeRef (..),
                                                  EnumType (..),
                                                  EnumTypeRef (..),
                                                  Enumerator (..), FunDef (..),
                                                  FunType (..),
                                                  GlobalDecls (..),
                                                  IdentDecl (..),
                                                  MemberDecl (..), ObjDef (..),
                                                  ParamDecl (..), TagDef (..),
                                                  Type (..), TypeDef (..),
                                                  TypeDefRef (..),
                                                  TypeName (..), TypeQuals (..),
                                                  VarDecl (..), VarName (..),
                                                  noTypeQuals)
import           Language.C.Analysis.TravMonad   (Trav, TravT, getUserState,
                                                  modifyUserState)
import           Language.C.Analysis.TypeUtils   (canonicalType, sameType)
import           Language.C.Data.Ident           (Ident (Ident),
                                                  SUERef (AnonymousRef, NamedRef))
import           Language.C.Data.Node            (NodeInfo (..), nodeInfo,
                                                  posOfNode)
import qualified Language.C.Pretty               as C
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants     (CInteger (..))
import           Prettyprinter                   (Doc, annotate, line, nest,
                                                  pretty, vsep, (<+>))
import           Prettyprinter.Render.Terminal   (AnsiStyle, Color (..), bold,
                                                  color, colorDull)
import           Tokstyle.C.Env                  (DiagnosticLevel (..),
                                                  DiagnosticSpan (..),
                                                  Env (globalDecls, mainTypedefs, retTy),
                                                  posAndLen, recordRichError)
import           Tokstyle.C.Patterns
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)
import           Tokstyle.C.TravUtils            (backticks)


-- | Get the name of an identifier.
idName :: Ident -> String
idName (Ident name _ _) = name


isStandardType :: String -> Bool
isStandardType name = name `elem`
    [ "uint8_t", "uint16_t", "uint32_t", "uint64_t"
    , "int8_t", "int16_t", "int32_t", "int64_t"
    , "size_t", "ssize_t", "uintptr_t", "intptr_t", "ptrdiff_t"
    , "int_least8_t", "int_least16_t", "int_least32_t", "int_least64_t"
    , "uint_least8_t", "uint_least16_t", "uint_least32_t", "uint_least64_t"
    , "int_fast8_t", "int_fast16_t", "int_fast32_t", "int_fast64_t"
    , "uint_fast8_t", "uint_fast16_t", "uint_fast32_t", "uint_fast64_t"
    , "intmax_t", "uintmax_t"
    , "nullptr_t", "va_list"
    , "__uint8_t", "__uint16_t", "__uint32_t", "__uint64_t"
    , "__int8_t", "__int16_t", "__int32_t", "__int64_t"
    , "__size_t", "__ssize_t", "__uintptr_t", "__intptr_t", "__ptrdiff_t"
    , "__off_t", "__off64_t", "__time_t", "__suseconds_t"
    , "__builtin_va_list", "socklen_t", "__socklen_t"
    , "opus_int16", "opus_int32"
    ]


-- | A simplified representation of a type for strictness analysis.
data Essence
    = Estrict Ident Essence
    | Estandard String
    | Eptr Essence
    | Earray Essence
    | Efunction Essence [(Maybe String, Essence)]
    | Ecomp SUERef
    | Eenum SUERef
    | Ebuiltin String
    | Evoid
    | Eanonymous
    deriving (Show, Eq)


-- | Convert a C type to its Essence.
toEssence :: Type -> Essence
toEssence ty = case ty of
    TypeDefType (TypeDefRef ident _ _) _ _
        | isStandardType (idName ident) -> Estandard (idName ident)
        | otherwise -> Estrict ident (toEssence (canonicalType ty))
    PtrType t _ _ -> Eptr (toEssence t)
    ArrayType t _ _ _ -> Earray (toEssence t)
    FunctionType (FunType r ps _) _ ->
        Efunction (toEssence r) (map getParamInfo ps)
    DirectType (TyComp (CompTypeRef ref _ _)) _ _ ->
        case ref of
            AnonymousRef _ -> Eanonymous
            _              -> Ecomp ref
    DirectType (TyEnum (EnumTypeRef ref _)) _ _ ->
        case ref of
            AnonymousRef _ -> Eanonymous
            _              -> Eenum ref
    DirectType TyVoid _ _ -> Evoid
    DirectType t _ _ -> Estandard (show (C.pretty (DirectType t noTypeQuals [])))
    _ -> Ebuiltin (show (C.pretty ty))
  where
    getParamInfo (ParamDecl (VarDecl name _ t) _)         = (getName name, toEssence t)
    getParamInfo (AbstractParamDecl (VarDecl name _ t) _) = (getName name, toEssence t)

    getName (VarName (Ident n _ _) _) = Just n
    getName _                         = Nothing


-- | Check if an essence is a function or a function pointer.
isFunctionEssence :: Essence -> Bool
isFunctionEssence = \case
    Efunction {} -> True
    Eptr e       -> isFunctionEssence e
    Estrict _ e  -> isFunctionEssence e
    _            -> False


data PathSegment
    = InReturn
    | InParam Int (Maybe String)
    | InPointer
    | InArray
    | InVariable String
    deriving (Eq, Show)


type Path = [PathSegment]


describePath :: Path -> String
describePath [] = ""
describePath (s:ss) = case s of
    InReturn           -> "the return value" ++ rest
    InParam i (Just n) -> "argument " ++ show i ++ " ('" ++ n ++ "')" ++ rest
    InParam i Nothing  -> "argument " ++ show i ++ rest
    InPointer          -> "pointer" ++ rest
    InArray            -> "array" ++ rest
    InVariable n       -> "variable '" ++ n ++ "'" ++ rest
  where
    rest = if null ss then "" else " -> " ++ describePath ss


prettyEssence :: Essence -> Doc AnsiStyle
prettyEssence = \case
    Estrict i e        -> annotate (color Blue <> bold) (pretty (idName i)) <+> annotate (colorDull White) ("(aka" <+> prettyEssence e <> ")")
    Estandard s        -> annotate (color Yellow) (pretty s)
    Eptr e             -> prettyEssence e <> "*"
    Earray e           -> prettyEssence e <> "[]"
    Efunction r ps     -> prettyEssence r <> "(" <> mconcat (intercalateDoc ", " (map (prettyEssence . snd) ps)) <> ")"
    Ecomp (NamedRef i) -> annotate (color Magenta) "struct" <+> pretty (idName i)
    Eenum (NamedRef i) -> annotate (color Magenta) "enum" <+> pretty (idName i)
    Ecomp _            -> annotate (color Magenta) "struct" <+> "<anonymous>"
    Eenum _            -> annotate (color Magenta) "enum" <+> "<anonymous>"
    Ebuiltin s         -> annotate (color Yellow) (pretty s)
    Evoid              -> annotate (color Yellow) "void"
    Eanonymous         -> "<anonymous>"
  where
    intercalateDoc _ []       = []
    intercalateDoc _ [x]      = [x]
    intercalateDoc sep (x:xs) = x : sep : intercalateDoc sep xs


prettyEssenceBrief :: Essence -> Doc AnsiStyle
prettyEssenceBrief = \case
    Estrict i _ -> annotate (color Blue <> bold) (pretty (idName i))
    e           -> prettyEssence e


-- | Match mode for strict typedefs.
data MatchMode = Strict | Lenient | Cast


data Mismatch = Mismatch
    { mismatchRootExpected :: Essence
    , mismatchRootActual   :: Essence
    , mismatchPath         :: Path
    , mismatchDeepExpected :: Essence
    , mismatchDeepActual   :: Essence
    }


recordMismatch :: NodeInfo -> Mismatch -> Trav Env ()
recordMismatch info (Mismatch rootExpected rootActual path expected actual) =
    recordRichError info ErrorLevel msg spans footer
  where
    (pos, len) = posAndLen info

    isCallbackMismatch =
        let e1 = strip rootExpected
            e2 = strip rootActual
        in case (e1, e2) of
            (Estrict _ f1, Estrict _ f2) -> isFunctionEssence f1 && isFunctionEssence f2
            _                            -> False
      where
        strip (Eptr e)      = strip e
        strip (Estrict i e) = Estrict i e -- Keep the outermost strict wrapper
        strip e             = e

    isDeepMismatch = case drop 1 path of
        [] -> False
        ss -> any (\case InParam{} -> True; InReturn -> True; _ -> False) ss

    msg | isCallbackMismatch = "incompatible callback role"
        | isDeepMismatch = case path of
            (InParam _ outerName : _) ->
                "incompatible callback type for parameter" <+> maybe "callback" (\n -> "'" <> pretty n <> "'") outerName
            _ -> "strict typedef mismatch"
        | otherwise = case path of
            (InReturn:_) ->
                "returning" <+> prettyEssenceBrief actual <+> "from a function expecting" <+> prettyEssenceBrief expected
            (InParam _ n:_) ->
                "passing" <+> prettyEssenceBrief actual <+> "to parameter" <+> maybe mempty (\name -> "'" <> pretty name <> "'") n <+> "of type" <+> prettyEssenceBrief expected
            (InVariable n:_) ->
                "assigning" <+> prettyEssenceBrief actual <+> "to variable" <+> backticks (pretty n) <+> "of type" <+> prettyEssenceBrief expected
            _ ->
                "strict typedef mismatch"

    spans = [ DiagnosticSpan pos len labels ]
    labels = if isDeepMismatch
             then [ "expected" <+> prettyEssenceBrief rootExpected
                  , "   found" <+> prettyEssence rootActual
                  ]
             else [ "expected" <+> prettyEssence expected
                  , "   found" <+> prettyEssence actual
                  ]

    deepNote = case path of
        (_ : rest) | isDeepMismatch ->
            [ (NoteLevel, "mismatch in" <+> pretty (describePath rest) <> ":" <> line <>
                          "   expected" <+> prettyEssence expected <> line <>
                          "      found" <+> prettyEssence actual) ]
        _ -> []

    footer = deepNote ++
        [ (NoteLevel, "strict typedefs prevent accidental mixing of logically distinct types")
        , (HelpLevel, "if this is intentional, use an explicit cast")
        ]


-- | Match two essences according to strict typedef rules.
matchEssence :: Map SUERef Ident -> MatchMode -> Path -> Essence -> Essence -> [Mismatch]
matchEssence mains mode initialPath rootExpected rootActual = go initialPath rootExpected rootActual
  where
    isMain i = \case
        Ecomp ref   -> Map.lookup ref mains == Just i
        Eenum ref   -> Map.lookup ref mains == Just i
        Estrict _ e -> isMain i e
        _           -> False

    isEstandardOrEnum (Estandard _) = True
    isEstandardOrEnum (Eenum _)     = True
    isEstandardOrEnum Eanonymous    = True
    isEstandardOrEnum _             = False

    isNullptr = \case
        Estandard "nullptr_t" -> True
        Estrict _ e           -> isNullptr e
        _                     -> False

    mkMismatch path expected actual = [Mismatch
        { mismatchRootExpected = rootExpected
        , mismatchRootActual = rootActual
        , mismatchPath = path
        , mismatchDeepExpected = expected
        , mismatchDeepActual = actual
        }]

    go path (Estrict i1 e1) (Estrict i2 e2)
        | idName i1 == idName i2 = []
        | isNullptr e2 = []
        | Lenient <- mode = go path e1 e2
        | otherwise = mkMismatch path (Estrict i1 e1) (Estrict i2 e2)

    go path (Estrict i1 e1) a
        | isFunctionEssence e1 && isFunctionEssence a = go path e1 a
        | a == Eanonymous = []
        | isNullptr a = []
        | Evoid <- a, InPointer `elem` path = []
        | isMain i1 a = go path e1 a
        | Lenient <- mode = go path e1 a
        | Cast <- mode, e1 == a = []
        | Cast <- mode, isEstandardOrEnum e1, isEstandardOrEnum a = []
        | otherwise = mkMismatch path (Estrict i1 e1) a

    go path e (Estrict _ a2) = go path e a2 -- ALLOW generic to strict

    go path (Eptr e1) (Eptr a1) = go (path ++ [InPointer]) e1 a1
    go path (Eptr e1) a | isFunctionEssence a = go path e1 a -- decay
    go path e (Eptr a1) | isFunctionEssence e = go path e a1 -- decay

    go path (Earray e1) (Earray a1) = go (path ++ [InArray]) e1 a1

    go path (Efunction r1 p1) (Efunction r2 p2) =
        go (path ++ [InReturn]) r1 r2 ++
        concatMap (\(i, ((n1, eP), (_, aP))) -> go (path ++ [InParam i n1]) eP aP) (zip [1..] (zip p1 p2))

    go _ _ _ = []


checkStrictMatch :: MatchMode -> Path -> Type -> Type -> CExpr -> Trav Env ()
checkStrictMatch _ _ _ _ (CConst (CIntConst CInteger{} _)) = return ()
checkStrictMatch mode path expected actual expr = do
    mains <- mainTypedefs <$> getUserState
    case matchEssence mains mode path (toEssence expected) (toEssence actual) of
        []   -> return ()
        errs -> mapM_ (recordMismatch (annotation expr)) errs


linter :: AstActions (TravT Env Identity)
linter = astActions
    { doIdentDecl = \node act -> case node of
        FunctionDef (FunDef (VarDecl _ _ (FunctionType (FunType ty _ _) _)) _ _) -> do
            modifyUserState $ \env -> env{retTy = Just ty}
            act
            modifyUserState $ \env -> env{retTy = Nothing}
        ObjectDef (ObjDef (VarDecl _ _ expected) (Just initializer) _) -> do
            checkInitializer Strict expected initializer
            act
        _ -> act

    , doExpr = \node act -> case node of
        CCast _ e _ -> do
            castTy <- tExpr [] RValue node
            exprTy <- tExpr [] RValue e
            checkStrictMatch Cast [] castTy exprTy e
            act
        CCompoundLit _ initializer _ -> do
            tExpr [] RValue node >>= \expected ->
                checkInitializer Lenient expected (CInitList initializer (annotation node))
            act
        CCall fun args _ -> do
            tExpr [] RValue fun >>= \case
                FunctionType (FunType _ params _) _ -> do
                    argTys <- mapM (tExpr [] RValue) args
                    mapM_ checkParam (zip4 [1..] params args argTys)
                PtrType (FunctionType (FunType _ params _) _) _ _ -> do
                    argTys <- mapM (tExpr [] RValue) args
                    mapM_ checkParam (zip4 [1..] params args argTys)
                _ -> return ()
            act

        CAssign _ l r _ -> do
            lTy <- tExpr [] LValue l
            rTy <- tExpr [] RValue r
            let path = case l of
                         CVar (Ident name _ _) _ -> [InVariable name]
                         _                       -> []
            checkStrictMatch Strict path lTy rTy r
            act

        _ -> act

    , doStat = \node act -> do
        case node of
            CReturn (Just e) _ -> do
                getUserState >>= \env -> case retTy env of
                    Just ty -> do
                        actualTy <- tExpr [] RValue e
                        checkStrictMatch Strict [InReturn] ty actualTy e
                    Nothing -> return ()
            _ -> return ()
        act
    }
  where
    checkParam (i, ParamDecl (VarDecl (VarName (Ident name _ _) _) _ expected) _, arg, actual) =
        checkStrictMatch Strict [InParam i (Just name)] expected actual arg
    checkParam (i, ParamDecl (VarDecl _ _ expected) _, arg, actual) =
        checkStrictMatch Strict [InParam i Nothing] expected actual arg
    checkParam _ = return ()

    checkInitializer mode ty initializer = do
        decls <- globalDecls <$> getUserState
        case initializer of
            CInitExpr e _ -> do
                actual <- tExpr [] RValue e
                checkStrictMatch mode [] ty actual e
            CInitList list _ -> do
                let mTypes = maybe [] (memberTypes ty) decls
                mapM_ (uncurry (checkInitializerList mode)) (zip mTypes list)

    checkInitializerList mode ty = \case
        (_, initializer) -> checkInitializer mode ty initializer


memberTypes :: Type -> GlobalDecls -> [Type]
memberTypes (canonicalType -> DirectType (TyComp (CompTypeRef ref _ _)) _ _) (GlobalDecls _ tags _) =
    case Map.lookup ref tags of
        Just (CompDef (CompType _ _ members _ _)) -> mapMaybe getMemberType members
        _ -> []
  where
    getMemberType (MemberDecl (VarDecl _ _ t) _ _) = Just t
    getMemberType _                                = Nothing
memberTypes (canonicalType -> ArrayType t _ _ _) _ = repeat t
memberTypes t _                                    = [t]


getMainTypedefs :: GlobalDecls -> Map SUERef Ident
getMainTypedefs (GlobalDecls _ _ gTypedefs) =
    Map.mapMaybeWithKey selectMain grouped
  where
    allTypedefs = [ (ref, i) | (i, TypeDef _ ty _ _) <- Map.toList gTypedefs
                             , Just ref <- [getSUERef ty] ]

    grouped = Map.fromListWith (++) [ (ref, [i]) | (ref, i) <- allTypedefs ]

    selectMain _ [ident] = Just ident
    selectMain ref idents = case ref of
        NamedRef name -> find (\i -> idName i == idName name) idents
        _             -> Nothing

    getSUERef = \case
        DirectType (TyComp (CompTypeRef ref _ _)) _ _ -> Just ref
        DirectType (TyEnum (EnumTypeRef ref _)) _ _   -> Just ref
        _                                             -> Nothing


analyse :: GlobalDecls -> Trav Env ()
analyse decls = do
    modifyUserState $ \env -> env{mainTypedefs = getMainTypedefs decls, globalDecls = Just decls}
    traverseAst linter decls


descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("strict-typedef", Text.unlines
    [ "Implements strict typedef checking."
    , ""
    , "This linter ensures that logically distinct types (represented by different"
    , "typedefs) are not accidentally mixed, even if they have the same underlying"
    , "representation. This is particularly useful for identifiers, indices, and"
    , "role-based types."
    ]))
