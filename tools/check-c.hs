{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Main (main) where

import           Control.Monad                   (forM_, unless)
import qualified Control.Monad.Parallel          as Par
import           Data.List                       (isPrefixOf, partition)
import qualified Data.Map                        as Map
import           Language.C
import           Language.C.Analysis.AstAnalysis
import           Language.C.Analysis.ConstEval
import           Language.C.Analysis.DefTable
import           Language.C.Analysis.SemError
import           Language.C.Analysis.SemRep
import           Language.C.Analysis.TravMonad
import           Language.C.Analysis.TypeUtils
import           Language.C.Data.Ident
import           Language.C.System.GCC
import           System.Environment              (getArgs)
import           System.Exit                     (ExitCode (..), exitWith)
import           System.IO                       (hPutStr, hPutStrLn, stderr)

typeEq :: Type -> Type -> Bool
typeEq a b = sameType (canon a) (canon b)
  where
    canon = removeQuals . canonicalType

removeQuals :: Type -> Type
removeQuals = typeQualsUpd (mergeTypeQuals noTypeQuals)

isEnum :: Type -> Bool
isEnum (canonicalType -> DirectType TyEnum{} _ _) = True
isEnum _                                          = False

isIntegral :: Type -> Bool
isIntegral (canonicalType -> DirectType TyIntegral{} _ _) = True
isIntegral _                                              = False

isFloating :: Type -> Bool
isFloating (canonicalType -> DirectType TyFloating{} _ _) = True
isFloating _                                              = False

isNumeric :: Type -> Bool
isNumeric ty = isIntegral ty || isFloating ty

checkBoolConversion :: MonadTrav m => CExpr -> m ()
checkBoolConversion expr = do
    ty <- tExpr [] RValue expr
    case ty of
      DirectType (TyIntegral TyBool) _ _ -> return ()
      DirectType (TyIntegral TyInt) _ _ -> return ()
      TypeDefType (TypeDefRef (Ident "bool" _ _) _ _) _ _ -> return ()
      _ ->
          let annot = (annotation expr, ty) in
          recordError $ typeMismatch ("implicit conversion from " <> show (pretty ty) <> " to bool") annot annot


checkConversion :: (Annotated node, MonadTrav m) => String -> (node NodeInfo, Type) -> (CExpr, Type) -> m ()
checkConversion _ (_, PtrType{}) (_, TY_void_ptr) = return ()
checkConversion _ (_, PtrType{}) (_, ArrayType{}) = return ()
checkConversion _ (_, lTy) (_, rTy)               | typeEq lTy rTy = return ()

-- Allow int to enum conversion to cover ternary operator "?:". Only actual
-- "int" is allowed, not "int32_t" or anything typedef'd. The latter would mean
-- assignment from something that didn't undergo implicit int conversions.
checkConversion _ (_, lTy) (_, rTy) | isEnumConversion (canonicalType lTy) rTy = return ()
  where
    isEnumConversion (DirectType TyEnum{} _ _) (DirectType (TyIntegral TyInt) _ _) = True
    isEnumConversion _ _ = False

checkConversion context (l, removeQuals -> lTy) (r, removeQuals -> rTy) =
    case (show $ pretty lTy, show $ pretty rTy) of
      (lTyName, rTyName) | lTyName == rTyName -> return ()
      ("const char *","char *")       -> return ()
      ("const char *","const int *")  -> return ()
      ("vpx_codec_er_flags_t", "int") -> return ()
      ("bool", "int") | relaxed r     -> return ()
      ("void *", _)                   -> return ()
      ("uint64_t","enum RTPFlags")    -> return ()

      -- TODO(iphydf): Look into these.
      ("int8_t", _)                   -> return ()
      ("uint8_t", _)                  -> return ()
      ("int16_t", _)                  -> return ()
      ("uint16_t", _)                 -> return ()
      ("int32_t", _)                  -> return ()
      ("uint32_t", _)                 -> return ()
      ("int64_t", _)                  -> return ()
      ("uint64_t", _)                 -> return ()
      ("size_t", _)                   -> return ()
      ("unsigned int", _)             -> return ()
      ("int", _)                      -> return ()
      ("long", _)                     -> return ()
      (lTyName, rTyName) ->
          recordError $ typeMismatch
              ("invalid conversion from `" <> rTyName <> "` to `" <>
                  lTyName <> "` in " <> context)
              (annotation l, lTy)
              (annotation r, rTy)
  where
      relaxed CUnary{}  = True
      relaxed CBinary{} = True
      relaxed _         = False

checkAssign :: MonadTrav m => String -> (CExpr, Type) -> (CExpr, Type) -> m ()
checkAssign _ _ (CConst{}, _) = return ()
checkAssign _ _ (CCast{}, _)  = return ()
checkAssign c l r             = checkConversion c l r

sameEnum :: MonadTrav m => Type -> Type -> (Ident, Expr) -> (Ident, Expr) -> m ()
sameEnum leftTy rightTy (leftId, leftExpr) (rightId, rightExpr) = do
    leftVal  <- getJust failMsg =<< intValue <$> constEval defaultMD Map.empty leftExpr
    rightVal <- getJust failMsg =<< intValue <$> constEval defaultMD Map.empty rightExpr
    unless (leftVal == rightVal) $
        throwTravError $ typeMismatch
            ("invalid cast: enumerator value for `"
                <> show (pretty leftId) <> " = " <> show leftVal
                <> "` does not match `"
                <> show (pretty rightId) <> " = " <> show rightVal <> "`")
            (annotation leftExpr, leftTy)
            (annotation rightExpr, rightTy)
  where
    failMsg = "invalid cast: could not determine enumerator values"

getJust :: MonadTrav m => String -> Maybe a -> m a
getJust _ (Just ok) = return ok
getJust failMsg Nothing =
    throwTravError $ userErr failMsg

checkEnumCast :: MonadTrav m => Type -> Type -> Expr -> m ()
checkEnumCast castTy exprTy _ = do
    castEnums <- enumerators (canonicalType castTy)
    exprEnums <- enumerators (canonicalType exprTy)
    unless (length castEnums == length exprEnums) $
        throwTravError $ userErr $
            "enum types `" <> show (pretty castTy) <> "` and `"
            <> show (pretty exprTy) <> "` have different a number of enumerators"
    sequence_ (zipWith (sameEnum castTy exprTy) castEnums exprEnums)

enumerators :: MonadTrav m => Type -> m [(Ident, Expr)]
enumerators (DirectType (TyEnum (EnumTypeRef name _)) _ _) = do
    defs <- getDefTable
    case lookupTag name defs of
      Just (Right (EnumDef (EnumType _ enums _ _))) ->
          return $ map (\(Enumerator i e _ _) -> (i, e)) enums
      _ ->
        throwTravError $ userErr $
            "couldn't find enum type `" <> show (pretty name) <> "`"
enumerators ty =
    throwTravError $ userErr $ "invalid enum type `" <> show (pretty ty) <> "`"

pattern TY_typedef name         <- TypeDefType (TypeDefRef (Ident name _ _) _ _) _ _
pattern TY_void_ptr             <- PtrType (DirectType TyVoid _ _) _ _
pattern TY_uint8_t_arr          <- ArrayType (TY_typedef "uint8_t") _ _ _
pattern TY_uint8_t_ptr          <- PtrType (TY_typedef "uint8_t") _ _
pattern TY_char_arr             <- ArrayType (DirectType (TyIntegral TyChar) _ _) _ _ _
pattern TY_char_ptr             <- PtrType (DirectType (TyIntegral TyChar) _ _) _ _
pattern TY_struct name          <- DirectType (TyComp (CompTypeRef (NamedRef (Ident name _ _)) _ _)) _ _
pattern TY_struct_ptr name      <- PtrType (TY_struct name) _ _
pattern TY_sockaddr_storage_ptr <- TY_struct_ptr "sockaddr_storage"
pattern TY_sockaddr_ptr         <- TY_struct_ptr "sockaddr"
pattern TY_sockaddr_in_ptr      <- TY_struct_ptr "sockaddr_in"
pattern TY_sockaddr_in6_ptr     <- TY_struct_ptr "sockaddr_in6"
pattern TY_canon_bool           <- (canonicalType -> DirectType (TyIntegral TyBool) _ _)

checkCast :: MonadTrav m => Type -> Type -> Expr -> m ()
-- Cast to void: OK.
checkCast (DirectType TyVoid _ _) _ _ = return ()
-- Casting between `void*` and `T*`: OK
checkCast PtrType{} TY_void_ptr _ = return ()
checkCast TY_void_ptr PtrType{} _ = return ()
-- Casting between `char*` and `uint8_t*`: OK
checkCast TY_uint8_t_ptr TY_char_ptr _ = return ()
checkCast TY_uint8_t_ptr TY_char_arr _ = return ()
checkCast TY_char_ptr TY_uint8_t_ptr _ = return ()
checkCast TY_char_ptr TY_uint8_t_arr _ = return ()
-- Casting literal 0 to `T*`: OK
checkCast PtrType{} _ (CConst (CIntConst (CInteger 0 _ _) _)) = return ()
-- Casting sockaddr_storage to any of the sockaddr_... types: OK
checkCast TY_sockaddr_ptr     TY_sockaddr_storage_ptr _ = return ()
checkCast TY_sockaddr_in_ptr  TY_sockaddr_storage_ptr _ = return ()
checkCast TY_sockaddr_in6_ptr TY_sockaddr_storage_ptr _ = return ()
-- Casting between numeric types: OK
checkCast castTy exprTy _ | isNumeric castTy && isNumeric exprTy = return ()
-- Casting from enum to int: OK
checkCast castTy exprTy _ | isIntegral castTy && isEnum exprTy = return ()
-- Casting between enums: check whether they have the same enumerators.
checkCast castTy exprTy e | isEnum castTy && isEnum exprTy = checkEnumCast castTy exprTy e
-- Casting to `Messenger**`: NOT OK, but toxav does this.
-- TODO(iphydf): Fix this.
checkCast (PtrType (PtrType (TY_typedef "Messenger") _ _) _ _) _ _ = return ()
-- Casting to `void**`: probably not ok, but toxav also does this.
-- TODO(iphydf): Investigate.
checkCast (PtrType TY_void_ptr _ _) _ _ = return ()
-- Casting from int to enum: actually NOT OK, but we do this a lot, so meh.
-- TODO(iphydf): Fix these.
checkCast castTy exprTy _ | isEnum castTy && isIntegral exprTy = return ()

-- Any other casts to pointer types: NOT OK
checkCast castTy@PtrType{} exprTy e =
    let annot = (annotation e, castTy) in
    recordError $ typeMismatch ("disallowed cast from " <>
        show (pretty exprTy) <> " to " <> show (pretty castTy)) annot annot

checkCast _ _ e = error (show e)

-- | This catches `sizeof(buf)` where `buf` is a pointer instead of an array.
checkSizeof :: MonadTrav m => CExpr -> Type -> m ()
checkSizeof _ (canonicalType -> TY_struct _) = return ()
checkSizeof _ (canonicalType -> TY_struct_ptr _) = return ()
checkSizeof _ ArrayType{} = return ()
checkSizeof e ty
  | isIntegral ty = return ()
  | otherwise =
      let annot = (annotation e, ty) in
      recordError $ typeMismatch
          ("disallowed sizeof argument of type `" <> show (pretty ty) <>
          "` - did you mean for `" <> show (pretty e) <> "` to be an array?") annot annot

checkNotBool :: MonadTrav m => CExpr -> Type -> Type -> m ()
checkNotBool _ TY_canon_bool TY_canon_bool = return ()
checkNotBool e nbTy ty@TY_canon_bool =
      let annot = (annotation e, ty) in
      recordError $ typeMismatch
          ("comparing `bool` value to `" <> show (pretty e)
          <> "` of non-bool type `" <> show (pretty nbTy) <> "`") annot annot
checkNotBool _ _ _ = return ()

data Env = Env
    { ctx   :: [String]
    , retTy :: Maybe Type
    }

pushCtx :: String -> Env -> Env
pushCtx s env@Env{ctx} = env{ctx = s:ctx}

popCtx :: Env -> Env
popCtx env@Env{ctx} = env{ctx = tail ctx}

setRetTy :: Type -> Env -> Env
setRetTy ty env = env{retTy = Just ty}

unsetRetTy :: Env -> Env
unsetRetTy env = env{retTy = Nothing}

checkFunc :: String -> IdentDecl -> Trav Env ()
checkFunc sysInclude (FunctionDef (FunDef (VarDecl _ _ (FunctionType (FunType ty _ _) _)) stmt info))
  | sysInclude `isPrefixOf` posFile (posOf info) = return ()
  | otherwise = do
      modifyUserState (setRetTy ty)
      checkStmt stmt
      modifyUserState unsetRetTy
checkFunc _ _ = return ()

checkBlockItem :: CBlockItem -> Trav Env ()
checkBlockItem (CBlockDecl decl) = checkDecl decl
checkBlockItem (CBlockStmt stmt) = checkStmt stmt
checkBlockItem (CNestedFunDef _) =
    throwTravError $ userErr $ "nested functions are not supported"

checkInit :: CInit -> Trav Env ()
checkInit (CInitExpr e _) = checkExpr e
checkInit (CInitList e _) = mapM_ (checkInit . snd) e

checkDecl :: CDecl -> Trav Env ()
checkDecl (CDecl _ decls _) = forM_ decls $ \(_, i, e) -> do
    maybeM i checkInit
    maybeM e checkExpr
checkDecl CStaticAssert{} =
    throwTravError $ userErr $ "static_assert not allowed in functions"


checkEq :: Expr -> Expr -> Trav Env ()
checkEq l r = do
    tyL <- tExpr [] RValue l
    tyR <- tExpr [] RValue r
    checkNotBool l tyL tyR
    checkNotBool r tyR tyL
    checkExpr l
    checkExpr r

checkExpr :: Expr -> Trav Env ()
checkExpr (CAssign CAssignOp l r _) = do
    checkExpr l
    checkExpr r
    lTy <- tExpr [] LValue l
    rTy <- tExpr [] RValue r
    checkAssign "assignment" (l, lTy) (r, rTy)
checkExpr (CCond c t e _) = do
    checkBoolConversion c
    checkExpr c
    maybeM t checkExpr
    checkExpr e
checkExpr (CUnary CNegOp e _) = do
    checkBoolConversion e
    checkExpr e
checkExpr (CBinary CLorOp l r _) = do
    checkBoolConversion l
    checkBoolConversion r
    checkExpr l
    checkExpr r
checkExpr (CBinary CLndOp l r _) = do
    checkBoolConversion l
    checkBoolConversion r
    checkExpr l
    checkExpr r
checkExpr (CBinary CEqOp l r _) = checkEq l r
checkExpr (CBinary CNeqOp l r _) = checkEq l r

checkExpr cast@(CCast t e _) = do
    castTy <- tExpr [] RValue cast
    exprTy <- tExpr [] RValue e
    Env{ctx} <- getUserState
    -- Some exemptions where weird casts like int* -> char* may happen.
    unless (head ctx `elem` ["call:getsockopt", "call:setsockopt", "call:bs_list_add", "call:bs_list_remove", "call:bs_list_find", "call:random_bytes"]) $
        checkCast castTy exprTy e
    checkDecl t
    checkExpr e
checkExpr (CVar _ _) = return ()
checkExpr (CConst _) = return ()
checkExpr (CSizeofType t _) = checkDecl t
checkExpr (CSizeofExpr e _) = do
    ty <- tExpr [] RValue e
    checkSizeof e ty
    checkExpr e
checkExpr (CIndex e i _) = do
    checkExpr e
    checkExpr i
checkExpr (CMember e _ _ _) = checkExpr e
checkExpr (CUnary _ e _) = checkExpr e
checkExpr (CBinary _ l r _) = do
    checkExpr l
    checkExpr r
checkExpr (CAssign _ l r _) = do
    checkExpr l
    checkExpr r
checkExpr (CCall (CVar (Ident fname _ _) _) a _) = do
    modifyUserState (pushCtx $ "call:"<>fname)
    mapM_ checkExpr a
    modifyUserState popCtx
checkExpr (CCall f a _) = do
    checkExpr f
    mapM_ checkExpr a
checkExpr (CComma es _) =
    mapM_ checkExpr es
checkExpr (CCompoundLit d i _) = do
    checkDecl d
    mapM_ (checkInit . snd) i
checkExpr expr = throwTravError $ userErr $ "expr: " <> show expr

checkStmt :: Stmt -> Trav Env ()
checkStmt (CIf cond t e _) = do
    checkBoolConversion cond
    checkExpr cond
    checkStmt t
    maybeM e checkStmt
checkStmt (CWhile c b _ _) = do
    checkExpr c
    checkStmt b
checkStmt (CBreak _) = return ()
checkStmt (CCont _) = return ()
checkStmt (CGoto _ _) = return ()
checkStmt (CLabel _ e _ _) = checkStmt e
checkStmt (CDefault b _) = checkStmt b
checkStmt (CCase c b _) = do
    checkExpr c
    checkStmt b
checkStmt (CSwitch c b _) = do
    checkExpr c
    checkStmt b
checkStmt (CFor i c n b _) = do
    either (flip maybeM checkExpr) checkDecl i
    maybeM c checkExpr
    maybeM n checkExpr
    checkStmt b
checkStmt (CExpr (Just expr) _) = checkExpr expr
checkStmt (CReturn Nothing _) = return ()
checkStmt (CReturn (Just expr) _) = do
    ty <- getJust "not in function context" . retTy =<< getUserState
    exprTy <- tExpr [] RValue expr
    checkConversion "return" (expr, ty) (expr, exprTy)
    checkExpr expr
checkStmt (CCompound [] stmts _) = mapM_ checkBlockItem stmts
checkStmt stmt = throwTravError $ userErr $ "stmt: " <> show stmt


checkTypes :: String -> GlobalDecls -> Trav Env ()
checkTypes sysInclude = mapM_ (checkFunc sysInclude) . Map.elems . gObjs


defaultCppOpts :: String -> [String]
defaultCppOpts sysInclude =
    [ "-nostdinc"
    , "-undef"
    , "-D__LITTLE_ENDIAN=0x4321"
    , "-D__BYTE_ORDER=__LITTLE_ENDIAN"
    , "-I" <> sysInclude
    , "-I" <> sysInclude <> "/opus"
    ]

processFile :: String -> CLanguage -> [String] -> FilePath -> IO (Bool, (String, [String]))
processFile sysInclude lang cppOpts file = do
    result <- parseCFile (newGCC "gcc") Nothing (defaultCppOpts sysInclude ++ cppOpts) file
    case result of
      Left err -> return (False, (file, ["Parse Error: " <> show err]))
      Right tu ->
          case runTrav (Env [file] Nothing) (body tu) of
            Left errs     -> return (False, (file, "Error" : map show errs))
            Right ((), _) -> return (True, (file, ["Success"]))
  where
    body tu = do
        modifyOptions (\opts -> opts { language = lang })
        decls <- analyseAST tu
        checkTypes sysInclude decls
        return ()

main :: IO ()
main = do
    args <- getArgs
    let (cppOpts, files) = partition (isPrefixOf "-") args
    let sysInclude = "/src/workspace/hs-tokstyle/include"
    result <- Par.mapM (processFile sysInclude GNU99 cppOpts) files
    mapM_ (printResult . snd) result
    unless (all fst result) $ exitWith (ExitFailure 1)
  where
    printResult (file, result) = do
        hPutStr stderr $ file <> ": "
        mapM_ (hPutStrLn stderr) result
