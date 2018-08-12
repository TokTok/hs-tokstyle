module Tokstyle.Cimple (main) where

import           Text.Groom             (groom)
import           Tokstyle.Cimple.Lexer  (runAlex)
import           Tokstyle.Cimple.Parser (parseCimple)


parseFile :: FilePath -> IO ()
parseFile source = do
    putStrLn $ "Processing " ++ source
    contents <- readFile source
    putStrLn . groom $ runAlex contents parseCimple


main :: [String] -> IO ()
main = mapM_ parseFile
