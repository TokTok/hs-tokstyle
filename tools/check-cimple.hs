module Main (main) where

import qualified Tokstyle.Cimple
import           Tokstyle.Sources (sources)

main :: IO ()
main = Tokstyle.Cimple.main sources
