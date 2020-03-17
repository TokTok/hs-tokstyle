module Main (main) where

import           System.Environment (getArgs)
import           Tokstyle.Cimple.IO (parseFile)
import           Tokstyle.Sources   (sources)


main :: IO ()
main = do
    args <- getArgs
    asts <- mapM getRight =<< mapM parseFile (case args of
        [] -> sources
        _  -> args)
    print . head $ asts
  where
    getRight (Left err) = fail err
    getRight (Right ok) = return ok
