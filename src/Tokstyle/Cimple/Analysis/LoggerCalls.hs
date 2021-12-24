{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.LoggerCalls (analyse) where

import qualified Control.Monad.State.Lazy    as State
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (AstActions, Lexeme (..),
                                              LiteralType (String), Node (..),
                                              defaultActions, doNode,
                                              traverseAst)
import qualified Language.Cimple.Diagnostics as Diagnostics
import           System.FilePath             (takeFileName)


linter :: AstActions [Text]
linter = defaultActions
    { doNode = \file node act ->
        case node of
            -- Ignore all function calls where the second argument is a string
            -- literal. If it's a logger call, it's a valid one.
            FunctionCall _ (_:LiteralExpr String _:_) -> act
            -- LOGGER_ASSERT has its format as the third parameter.
            FunctionCall (LiteralExpr _ (L _ _ "LOGGER_ASSERT")) (_:_:LiteralExpr String _:_) -> act

            FunctionCall (LiteralExpr _ name@(L _ _ func)) _ | Text.isPrefixOf "LOGGER_" func -> do
                Diagnostics.warn file name $ "logger call `" <> func <> "' has a non-literal format argument"
                act

            _ -> act
    }


analyse :: (FilePath, [Node () (Lexeme Text)]) -> [Text]
-- Ignore logger.h, which contains a bunch of macros that call LOGGER functions
-- with their (literal) arguments. We don't know that they are literals at this
-- point, though.
analyse (file, _) | takeFileName file == "logger.h" = []
analyse tu = reverse . flip State.execState [] . traverseAst linter $ tu
