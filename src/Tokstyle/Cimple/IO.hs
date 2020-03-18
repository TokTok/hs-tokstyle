module Tokstyle.Cimple.IO
    ( parseFile
    ) where

import           Control.Monad.State.Lazy (State, get, put, runState)
import qualified Data.ByteString          as BS
import qualified Data.Compact             as Compact
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Tokstyle.Cimple.AST      (Node (..))
import           Tokstyle.Cimple.Lexer    (Lexeme, runAlex)
import           Tokstyle.Cimple.Parser   (parseCimple)


type CompactState a = State (Map String Text) a

cacheText :: String -> CompactState Text
cacheText s = do
    m <- get
    case Map.lookup s m of
        Nothing -> do
            let text = Text.pack s
            put $ Map.insert s text m
            return text
        Just text ->
            return text


process :: [Node (Lexeme String)] -> IO [Node (Lexeme Text)]
process stringAst = do
    let (textAst, _) = runState (mapM (mapM (mapM cacheText)) stringAst) Map.empty
    Compact.getCompact <$> Compact.compactWithSharing textAst


parseFile :: FilePath -> IO (Either String [Node (Lexeme Text)])
parseFile source = do
    putStrLn $ "Processing " ++ source
    contents <- Text.unpack . Text.decodeUtf8 <$> BS.readFile source

    mapM process $ runAlex contents parseCimple
