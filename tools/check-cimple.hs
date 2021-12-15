{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Text                (Text)
import qualified Data.Text.IO             as Text
import           Language.Cimple          (Lexeme, Node)
import           Language.Cimple.IO       (parseProgram)
import qualified Language.Cimple.Program  as Program
import           System.Environment       (getArgs)

import           Tokstyle.Cimple.Analysis (analyse)


processAst :: FilePath -> [Node (Lexeme Text)] -> IO ()
processAst file ast = do
    case analyse file ast of
        [] -> return ()
        diags -> do
            mapM_ Text.putStrLn diags
            fail $ "errors found in " <> file


main :: IO ()
main =
    getArgs
    >>= parseProgram
    >>= getRight
    >>= mapM_ (uncurry processAst) . Program.toList
  where
    getRight (Left err) = fail err
    getRight (Right ok) = return ok
