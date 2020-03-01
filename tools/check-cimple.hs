module Main (main) where

import           System.Environment (getArgs)
import qualified Tokstyle.Cimple
import           Tokstyle.Sources   (sources)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> Tokstyle.Cimple.main sources
    _  -> Tokstyle.Cimple.main args
