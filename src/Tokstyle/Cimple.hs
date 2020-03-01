module Tokstyle.Cimple (main) where

import qualified Data.ByteString        as BS
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Text.Groom             (groom)
import           Tokstyle.Cimple.Lexer  (runAlex)
import           Tokstyle.Cimple.Parser (parseCimple)


parseFile :: FilePath -> IO ()
parseFile source = do
    putStrLn $ "Processing " ++ source
    contents <- Text.unpack . Text.decodeUtf8 <$> BS.readFile source
    putStrLn . groom $ runAlex contents parseCimple


main :: [String] -> IO ()
main = mapM_ parseFile
