module Main where

import           Control.Monad                   (forM_)
import           Data.List                       (isPrefixOf, partition)
import qualified Data.Map                        as Map
import           Language.C
import           Language.C.Analysis.AstAnalysis
import           Language.C.Analysis.SemError
import           Language.C.Analysis.SemRep
import           Language.C.Analysis.TravMonad
import           Language.C.System.GCC
import           System.Environment              (getArgs)
import           System.Exit                     (ExitCode (..), exitWith)
import           System.IO                       (hPutStr, hPutStrLn, stderr)

checkTypes :: MonadTrav m => String -> GlobalDecls -> m ()
checkTypes sysInclude =
    mapM_ checkFunc . Map.elems . gObjs
  where
    checkFunc :: MonadTrav m => IdentDecl -> m ()
    checkFunc (FunctionDef (FunDef _ stmt info))
      | sysInclude `isPrefixOf` posFile (posOf info) = return ()
      | otherwise = checkStmt stmt
    checkFunc _ = return ()

    checkBlockItem :: MonadTrav m => CBlockItem -> m ()
    checkBlockItem (CBlockDecl decl) = checkDecl decl
    checkBlockItem (CBlockStmt stmt) = checkStmt stmt
    checkBlockItem stmt = throwTravError $ userErr $ "block-item: " <> show stmt

    checkInit :: MonadTrav m => CInit -> m ()
    checkInit (CInitExpr e _) = checkExpr e
    checkInit (CInitList e _) = mapM_ (checkInit . snd) e

    checkDecl :: MonadTrav m => CDecl -> m ()
    checkDecl (CDecl _ decls _) = forM_ decls $ \(_, i, e) -> do
        maybeM i checkInit
        maybeM e checkExpr
    checkDecl decl = throwTravError $ userErr $ "decl: " <> show decl

    checkExpr :: MonadTrav m => Expr -> m ()
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
    checkExpr (CCond c t e _) = do
        checkExpr c
        maybeM t checkExpr
        checkExpr e
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
    checkExpr (CCompoundLit d i _) = do
        checkDecl d
        mapM_ (checkInit . snd) i
    checkExpr expr = throwTravError $ userErr $ "expr: " <> show expr

    checkStmt :: MonadTrav m => Stmt -> m ()
    checkStmt (CIf CBinary{} t e _) = do
        checkStmt t
        maybeM e checkStmt
    checkStmt (CIf cond t e _) = do
        ty <- tExpr [] RValue cond
        case ty of
          PtrType{} ->
              let annot = (annotation cond, ty) in
              recordError $ typeMismatch "implicit conversion from pointer to bool" annot annot
          _ -> return ()
        checkExpr cond
        checkStmt t
        maybeM e checkStmt
    checkStmt (CWhile c b _ _) = do
        checkExpr c
        checkStmt b
    checkStmt (CBreak _) = return ()
    checkStmt (CCont _) = return ()
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


defaultCppOpts :: String -> [String]
defaultCppOpts sysInclude =
    [ "-nostdinc"
    , "-undef"
    , "-D__LITTLE_ENDIAN=0x4321"
    , "-D__BYTE_ORDER=__LITTLE_ENDIAN"
    , "-I" <> sysInclude
    ]

processFile :: String -> CLanguage -> [String] -> FilePath -> IO ()
processFile sysInclude lang cppOpts file = do
    hPutStr stderr $ file ++ ": "
    result <- parseCFile (newGCC "gcc") Nothing (defaultCppOpts sysInclude ++ cppOpts) file
    case result of
      Left err -> do
          hPutStrLn stderr ('\n' : show err)
          hPutStrLn stderr "Failed: Parse Error"
          exitWith (ExitFailure 1)
      Right tu ->
          case runTrav_ (body tu) of
            Left errs        -> mapM_ (hPutStrLn stderr) ("Error" : map show errs)
            Right ((), errs) -> mapM_ (hPutStrLn stderr) ("Success" : map show errs)
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
    mapM_ (processFile sysInclude GNU99 cppOpts) files
