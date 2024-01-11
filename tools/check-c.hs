{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Main (main) where

import           Control.Monad          (unless)
import qualified Control.Monad.Parallel as Par
import           Data.List              (find, isPrefixOf, partition)
import qualified Data.Maybe             as Maybe
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import           Language.C             (parseCFile)
import           Language.C.System.GCC  (newGCC)
import           System.Environment     (getArgs)
import           System.Exit            (ExitCode (..), exitWith)
import           System.IO              (stderr)
import           Tokstyle.C.Linter      (allWarnings, analyse)


defaultCppOpts :: String -> [String]
defaultCppOpts sysInclude =
    [ "-nostdinc"  -- we have our own stdlib headers
    , "-undef"     -- no __linux__
    , "-I" <> sysInclude
    , "-I" <> sysInclude <> "/opus"
    ]


processFile :: String -> String -> [String] -> FilePath -> IO (Bool, [Text])
processFile cc sysInclude cppOpts file = do
    result <- parseCFile (newGCC cc) Nothing (defaultCppOpts sysInclude ++ cppOpts) file
    case result of
        Left err -> return (False, [Text.pack file <> ": Parse Error: " <> Text.pack (show err)])
        Right tu -> case analyse allWarnings tu of
            []   -> return (True, [])
            errs -> return (False, errs)


main :: IO ()
main = do
    args <- getArgs
    let (opts, rest) = partition (isPrefixOf "--") args
    let (cppOpts, files) = partition (isPrefixOf "-") rest
    let cc = Maybe.fromMaybe "clang" $ getFlag "--cc=" opts
    let sysInclude = Maybe.fromMaybe "src/workspace/hs-tokstyle/include" $ getFlag "--include=" opts
    result <- Par.mapM (processFile cc sysInclude cppOpts) files
    mapM_ (mapM_ (Text.hPutStrLn stderr) . snd) result
    unless (all fst result) $ exitWith (ExitFailure 1)
  where
    getFlag flag = fmap (drop $ length flag) . find (isPrefixOf flag)
