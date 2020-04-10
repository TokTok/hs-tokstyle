{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text.IO             as Text
import           Language.Cimple.IO       (parseFile)
import           System.Environment       (getArgs)
import           Tokstyle.Sources         (sources)

import           Tokstyle.Cimple.Analysis (analyse)


processFile :: FilePath -> IO ()
processFile file = do
    ast <- parseFile file >>= getRight
    case analyse file ast of
        [] -> return ()
        diags -> do
            mapM_ Text.putStrLn diags
            fail $ "errors found in " <> file
  where
    getRight (Left err) = fail err
    getRight (Right ok) = return ok


main :: IO ()
main = do
    args <- getArgs
    mapM_ processFile $ case args of
        [] -> sources
        _  -> args
