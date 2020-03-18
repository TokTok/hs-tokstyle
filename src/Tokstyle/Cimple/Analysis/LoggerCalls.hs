{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.LoggerCalls (analyse) where

import           Control.Monad.State.Lazy    (State)
import qualified Control.Monad.State.Lazy    as State
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           System.FilePath             (takeFileName)
import           Tokstyle.Cimple.AST         (LiteralType (String), Node (..))
import qualified Tokstyle.Cimple.Diagnostics as Diagnostics
import           Tokstyle.Cimple.Lexer       (Lexeme (..))
import           Tokstyle.Cimple.TraverseAst


linter :: FilePath -> AstActions (State [Text]) Text
linter file = defaultActions
    { doNode = \node act ->
        case node of
            -- Ignore all function calls where the second argument is a string
            -- literal. If it's a logger call, it's a valid one.
            FunctionCall _ (_:LiteralExpr String _:_) -> act
            -- LOGGER_ASSERT has its format as the third parameter.
            FunctionCall (LiteralExpr _ (L _ _ "LOGGER_ASSERT")) (_:_:LiteralExpr String _:_) -> act

            FunctionCall (LiteralExpr _ name@(L _ _ func)) _ | Text.isPrefixOf "LOGGER_" func -> do
                warn name $ "logger call `" <> func <> "' has a non-literal format argument"
                act

            _ -> act
    }
  where warn = Diagnostics.warn file


analyse :: FilePath -> [Node (Lexeme Text)] -> [Text]
-- Ignore logger.h, which contains a bunch of macros that call LOGGER functions
-- with their (literal) arguments. We don't know that they are literals at this
-- point, though.
analyse file _ | takeFileName file == "logger.h" = []
analyse file ast = reverse $ State.execState (traverseAst (linter file) ast) []
