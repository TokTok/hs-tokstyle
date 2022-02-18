module Main (main) where

import           Control.Monad                   (forM_, unless)
import qualified Control.Monad.Parallel          as Par
import           Data.List                       (isPrefixOf, partition)
import qualified Data.Map                        as Map
import           Language.C
import           Language.C.Analysis.AstAnalysis
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
    canon = typeQualsUpd (mergeTypeQuals noTypeQuals) . canonicalType

checkBoolConversion :: MonadTrav m => CExpr -> m ()
checkBoolConversion expr = do
    ty <- tExpr [] RValue expr
    case ty of
      DirectType (TyIntegral TyBool) _ _ -> return ()
      DirectType (TyIntegral TyInt) _ _ -> return ()
      TypeDefType (TypeDefRef (Ident "bool" _ _) _ _) _ _ -> return ()
      -- TODO(iphydf): Clean these up and then disallow them.
      TypeDefType (TypeDefRef (Ident "uint8_t" _ _) _ _) _ _ -> return ()
      TypeDefType (TypeDefRef (Ident "uint16_t" _ _) _ _) _ _ -> return ()
      TypeDefType (TypeDefRef (Ident "uint32_t" _ _) _ _) _ _ -> return ()
      TypeDefType (TypeDefRef (Ident "uint64_t" _ _) _ _) _ _ -> return ()
      _ ->
          let annot = (annotation expr, ty) in
          recordError $ typeMismatch ("implicit conversion from " <> show (pretty ty) <> " to bool") annot annot


checkConversion :: (Annotated node, MonadTrav m) => (node NodeInfo, Type) -> (node NodeInfo, Type) -> m ()
checkConversion (l, lTy) (r, rTy) =
    case (show $ pretty lTy, show $ pretty rTy) of
      ("const char *","const int *")  -> return ()
      ("vpx_codec_er_flags_t", "int") -> return ()
      ("bool", "int")                 -> return ()
      ("void *", _)                   -> return ()
      ("uint64_t","enum RTPFlags")    -> return ()

      -- TODO(iphydf): Look into these.
      ("uint8_t", _)                  -> return ()
      ("uint16_t", _)                 -> return ()
      ("uint32_t", _)                 -> return ()
      ("size_t", _)                   -> return ()
      ("unsigned int", _)             -> return ()
      ("int", _)                      -> return ()
      ("long", _)                     -> return ()
      (lTyName, rTyName) ->
          recordError $ typeMismatch ("invalid conversion from `" <> rTyName <> "` to `" <> lTyName <> "` in assignment")
              (annotation l, lTy)
              (annotation r, rTy)

checkAssign :: MonadTrav m => (CExpr, Type) -> (CExpr, Type) -> m ()
checkAssign (_, PtrType{}) (_, ArrayType{}) = return ()
checkAssign (_, lTy) (_, rTy)               | typeEq lTy rTy = return ()
checkAssign _ (CConst{}, _)                 = return ()
checkAssign _ (CCast{}, _)                  = return ()
checkAssign l r                             = checkConversion l r

checkFunc :: MonadTrav m => String -> IdentDecl -> m ()
checkFunc sysInclude (FunctionDef (FunDef _ stmt info))
  | sysInclude `isPrefixOf` posFile (posOf info) = return ()
  | otherwise = checkStmt stmt
checkFunc _ _ = return ()

checkBlockItem :: MonadTrav m => CBlockItem -> m ()
checkBlockItem (CBlockDecl decl) = checkDecl decl
checkBlockItem (CBlockStmt stmt) = checkStmt stmt
checkBlockItem (CNestedFunDef _) =
    throwTravError $ userErr $ "nested functions are not supported"

checkInit :: MonadTrav m => CInit -> m ()
checkInit (CInitExpr e _) = checkExpr e
checkInit (CInitList e _) = mapM_ (checkInit . snd) e

checkDecl :: MonadTrav m => CDecl -> m ()
checkDecl (CDecl _ decls _) = forM_ decls $ \(_, i, e) -> do
    maybeM i checkInit
    maybeM e checkExpr
checkDecl CStaticAssert{} =
    throwTravError $ userErr $ "static_assert not allowed in functions"


checkExpr :: MonadTrav m => Expr -> m ()
checkExpr (CAssign CAssignOp l r _) = do
    checkExpr l
    checkExpr r
    lTy <- tExpr [] LValue l
    rTy <- tExpr [] RValue r
    checkAssign (l, lTy) (r, rTy)
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

checkExpr (CCast t e _) = do
    checkDecl t
    checkExpr e
checkExpr (CVar _ _) = return ()
checkExpr (CConst _) = return ()
checkExpr (CSizeofType t _) = checkDecl t
checkExpr (CSizeofExpr e _) = checkExpr e
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
checkExpr (CCall f a _) = do
    checkExpr f
    mapM_ checkExpr a
checkExpr (CComma es _) =
    mapM_ checkExpr es
checkExpr (CCompoundLit d i _) = do
    checkDecl d
    mapM_ (checkInit . snd) i
checkExpr expr = throwTravError $ userErr $ "expr: " <> show expr

checkStmt :: MonadTrav m => Stmt -> m ()
checkStmt (CIf CBinary{} t e _) = do
    checkStmt t
    maybeM e checkStmt
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
checkStmt (CReturn expr _) = maybeM expr checkExpr
checkStmt (CCompound [] stmts _) = mapM_ checkBlockItem stmts
checkStmt stmt = throwTravError $ userErr $ "stmt: " <> show stmt


checkTypes :: MonadTrav m => String -> GlobalDecls -> m ()
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
          case runTrav_ (body tu) of
            Left errs        -> return (False, (file, "Error" : map show errs))
            Right ((), errs) -> return (True, (file, "Success" : map show errs))
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
