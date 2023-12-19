{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad                   (unless)
import qualified Control.Monad.Parallel          as Par
import           Data.List                       (find, isPrefixOf, partition)
import qualified Data.Maybe                      as Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.IO                    as Text
import           Language.C                      (CTranslUnit, parseCFile)
import           Language.C.Analysis.AstAnalysis (analyseAST)
import           Language.C.Analysis.TravMonad   (CLanguage (..), Trav,
                                                  TravOptions (..),
                                                  modifyOptions, runTrav)
import           Language.C.System.GCC           (newGCC)
import           System.Environment              (getArgs)
import           System.Exit                     (ExitCode (..), exitWith)
import           System.IO                       (stderr)
import           Tokstyle.C.Env                  (Env, defaultEnv)
import           Tokstyle.C.Linter               (allWarnings, analyse)


defaultCppOpts :: String -> [String]
defaultCppOpts sysInclude =
    [ "-nostdinc"  -- we have our own stdlib headers
    , "-undef"     -- no __linux__
    , "-I" <> sysInclude
    , "-I" <> sysInclude <> "/opus"
    ]

analyseGNU99 :: CTranslUnit -> Trav Env ()
analyseGNU99 tu = do
    modifyOptions (\opts -> opts { language = GNU99 })
    decls <- analyseAST tu
    analyse allWarnings decls


processFile :: String -> String -> [String] -> FilePath -> IO (Bool, [Text])
processFile cc sysInclude cppOpts file = do
    result <- parseCFile (newGCC cc) Nothing (defaultCppOpts sysInclude ++ cppOpts) file
    case result of
      Left err -> return (False, [Text.pack file <> ": Parse Error: " <> Text.pack (show err)])
      Right tu ->
          case runTrav defaultEnv $ analyseGNU99 tu of
            Left errs -> return (False, map (Text.pack . show) errs)
            Right _   -> return (True, [])


main :: IO ()
main = do
    args <- getArgs
    let (opts, rest) = partition (isPrefixOf "--") args
    let (cppOpts, files) = partition (isPrefixOf "-") rest
    let cc = Maybe.fromMaybe "clang" $ getFlag "--cc=" opts
    let sysInclude = "/src/workspace/hs-tokstyle/include"
    result <- Par.mapM (processFile cc sysInclude cppOpts) files
    mapM_ (mapM_ (Text.hPutStrLn stderr) . snd) result
    unless (all fst result) $ exitWith (ExitFailure 1)
  where
    getFlag flag = fmap (drop $ length flag) . find (isPrefixOf flag)
