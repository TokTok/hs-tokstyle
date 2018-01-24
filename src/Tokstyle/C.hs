module Tokstyle.C (main) where

import           Control.Applicative             ((<$>))
import qualified Data.List                       as List
import           Language.C                      (CError, CTranslUnit,
                                                  ErrorInfo (..), InputStream,
                                                  errorInfo, initPos, parseC,
                                                  posFile, posRow)
import           Language.C.Analysis.AstAnalysis (analyseAST)
import           Language.C.Analysis.SemRep      (GlobalDecls)
import           Language.C.Analysis.TravMonad   (runTrav_)
import           Language.C.System.GCC           (newGCC)
import           Language.C.System.Preprocess    (rawCppArgs, runPreprocessor)
import           System.Environment              (getArgs)

import qualified Tokstyle.C.Naming
import           Tokstyle.Result


phaseCpp :: FilePath -> IO InputStream
phaseCpp file = do
    cppArgs <- (["-std=c99", "-U__BLOCKS__", "-D_VA_LIST", "-D_Nonnull=", "-D_Nullable=", "-D__attribute__(x)="] ++) <$> getArgs
    result <- runPreprocessor (newGCC "gcc") $ rawCppArgs cppArgs file
    case result of
        Left err -> fail $ show err
        Right ok -> return ok


phaseParse :: FilePath -> InputStream -> Result CTranslUnit
phaseParse file preprocessed =
    case parseC preprocessed (initPos file) of
        Left err -> fail $ show err
        Right tu -> return tu


phaseAnalyse :: CTranslUnit -> Result (CTranslUnit, GlobalDecls, [CError])
phaseAnalyse tu =
    case runTrav_ (analyseAST tu) of
        Left errs           -> fail $ concatMap show errs
        Right (decls, cerr) -> return (tu, decls, cerr)


phaseCheck :: (CTranslUnit, GlobalDecls, [CError]) -> [CError]
phaseCheck (tu, decls, _cerr) =
    --cerr ++
    Tokstyle.C.Naming.check tu decls


printError :: ErrorInfo -> IO ()
printError (ErrorInfo _ pos msgs) =
    putStrLn $ file ++ ":" ++ show line ++ ": " ++ List.intercalate "\n\t" msgs
  where
    file = posFile pos
    line = posRow  pos


showResult :: Result [CError] -> IO ()
showResult (Success cerr) = mapM_ (printError . errorInfo) cerr
showResult (Failure  err) = putStr err


process :: FilePath -> IO ()
process path = do
  preprocessed <- phaseCpp path
  let analysis = phaseParse path preprocessed >>= phaseAnalyse
  let results = phaseCheck <$> analysis
  showResult results

main :: [String] -> IO ()
main sources = do
    putStrLn "[=] preprocessing..."
    mapM_ process sources
