{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.FuncPrototypes (analyse) where

import           Control.Monad.State.Lazy    (State)
import qualified Control.Monad.State.Lazy    as State
import           Data.Text                   (Text)
import           Language.Cimple             (Lexeme, Node (..))
import qualified Language.Cimple.Diagnostics as Diagnostics
import           Language.Cimple.TraverseAst


linter :: FilePath -> AstActions (State [Text]) Text
linter file = defaultActions
    { doNode = \node act ->
        case node of
            FunctionPrototype _ name [] -> do
                warn name "empty parameter list must be written as (void)"
                act

            _ -> act
    }
  where warn = Diagnostics.warn file

analyse :: FilePath -> [Node (Lexeme Text)] -> [Text]
analyse file ast = reverse $ State.execState (traverseAst (linter file) ast) []
