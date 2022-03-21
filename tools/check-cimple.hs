{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Arrow               (first)
import           Control.Parallel.Strategies (parMap, rpar)
import           Data.List                   (isPrefixOf, partition)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.IO                as Text
import           Data.Time.Clock             (UTCTime, diffUTCTime,
                                              getCurrentTime)
import           Language.Cimple             (Lexeme, Node)
import           Language.Cimple.IO          (parseFiles)
import           System.Environment          (getArgs)
import           System.IO                   (hPutStrLn, stderr)

import           Tokstyle.Linter             (allWarnings, analyse,
                                              analyseGlobal)


processAst :: [Text] -> (UTCTime, [(FilePath, [Node (Lexeme Text)])]) -> IO ()
processAst ignore (start, tus) = do
    report $ concat $ analyseGlobal ignore tus : parMap rpar (analyse ignore) tus
    end <- getCurrentTime
    hPutStrLn stderr $ "Linting: " <> show (diffUTCTime end start)
  where
    report = \case
        [] -> return ()
        diags -> do
            mapM_ Text.putStrLn diags
            fail "tokstyle violations detected"


parseArgs :: [String] -> ([Text], [FilePath])
parseArgs = first (processFlags . map (drop 2)) . partition ("-W" `isPrefixOf`)
  where
    processFlags :: [String] -> [Text]
    processFlags = foldr processFlag allWarnings . reverse

    processFlag :: String -> [Text] -> [Text]
    processFlag ('n':'o':'-':flag) = filter (/= Text.pack flag)
    processFlag flag               = (Text.pack flag :)


defaultFlags :: [String]
defaultFlags =
    [ "-Wno-callback-names"
    , "-Wno-enum-names"
    ]


main :: IO ()
main = do
    (flags, files) <- parseArgs . (defaultFlags ++) <$> getArgs
    start <- getCurrentTime
    hPutStrLn stderr $ "Parsing " <> show (length files) <> " files..."
    parseFiles files >>= getRight start >>= processAst flags

getRight :: UTCTime -> Either String a -> IO (UTCTime, a)
getRight _ (Left err) = putStrLn err >> fail "aborting after parse error"
getRight start (Right ok) = do
    end <- getCurrentTime
    hPutStrLn stderr $ "Parsing: " <> show (diffUTCTime end start)
    return (end, ok)
