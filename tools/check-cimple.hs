{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Parallel.Strategies (parMap, rpar)
import           Data.Text                   (Text)
import qualified Data.Text.IO                as Text
import           Language.Cimple             (Lexeme, Node)
import           Language.Cimple.IO          (parseFiles)
import           System.Environment          (getArgs)

import           Tokstyle.Linter             (analyse, analyseGlobal)


processAst :: [(FilePath, [Node (Lexeme Text)])] -> IO ()
processAst tus = report $ concat $ analyseGlobal tus : parMap rpar analyse tus
  where
    report = \case
        [] -> return ()
        diags -> do
            mapM_ Text.putStrLn diags
            fail "tokstyle violations detected"


main :: IO ()
main =
    getArgs
    >>= parseFiles
    >>= getRight
    >>= processAst
  where
    getRight (Left err) = fail err
    getRight (Right ok) = return ok
