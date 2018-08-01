module Main (main) where

import qualified Tokstyle.C
import           Tokstyle.Sources (sources)

main :: IO ()
main = Tokstyle.C.main sources
