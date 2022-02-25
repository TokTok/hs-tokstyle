{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Arrow               (first)
import           Control.Parallel.Strategies (parMap, rpar)
import           Data.List                   (isPrefixOf, partition)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.IO                as Text
import           Language.Cimple             (Lexeme, Node)
import           Language.Cimple.IO          (parseFiles)
import           System.Environment          (getArgs)

import           Tokstyle.Linter             (allWarnings, analyse,
                                              analyseGlobal)


processAst :: [Text] -> [(FilePath, [Node (Lexeme Text)])] -> IO ()
processAst ignore tus = report $ concat $ (analyseGlobal ignore) tus : parMap rpar (analyse ignore) tus
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
    parseFiles files >>= getRight >>= processAst flags
  where
    getRight (Left err) = putStrLn err >> fail "aborting after parse error"
    getRight (Right ok) = return ok
