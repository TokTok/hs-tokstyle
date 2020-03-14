module Main (main) where

import qualified Data.ByteString        as BS
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified GHC.Compact            as Compact
import           System.Environment     (getArgs)
import           Tokstyle.Cimple.Lexer  (runAlex)
import           Tokstyle.Cimple.Parser (parseCimple)
import           Tokstyle.Sources       (sources)


parseFile :: FilePath -> IO ()
parseFile source = do
    putStrLn $ "Processing " ++ source
    contents <- Text.unpack . Text.decodeUtf8 <$> BS.readFile source
    let ast = runAlex contents parseCimple
    size <- Compact.compactSize =<< Compact.compactWithSharing ast
    putStrLn $ "Memory size of AST: " ++ show size ++ " bytes"


main :: IO ()
main = do
  args <- getArgs
  mapM_ parseFile $ case args of
    [] -> sources
    _  -> args
