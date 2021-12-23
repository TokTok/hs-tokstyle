{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.LoggerNoEscapes (analyse) where

import           Control.Monad               (when)
import           Control.Monad.State.Lazy    (State)
import qualified Control.Monad.State.Lazy    as State
import           Data.Text                   (Text, isInfixOf)
import qualified Data.Text                   as Text
import           Language.Cimple             (AstActions, Lexeme (..),
                                              LiteralType (String), Node (..),
                                              defaultActions, doNode,
                                              lexemeText, traverseAst)
import qualified Language.Cimple.Diagnostics as Diagnostics


linter :: AstActions [Text]
linter = defaultActions
    { doNode = \file node act -> case node of
            -- LOGGER_ASSERT has its format as the third parameter.
        FunctionCall (LiteralExpr _ (L _ _ "LOGGER_ASSERT")) (_ : _ : LiteralExpr String fmt : _)
            -> do
                checkFormat file fmt
                act

        FunctionCall (LiteralExpr _ (L _ _ func)) (_ : LiteralExpr String fmt : _)
            | Text.isPrefixOf "LOGGER_" func
            -> do
                checkFormat file fmt
                act

        _ -> act
    }


checkFormat :: FilePath -> Lexeme Text -> State [Text] ()
checkFormat file fmt =
    when ("\\" `isInfixOf` text)
        $  Diagnostics.warn file fmt
        $  "logger format "
        <> text
        <> " contains escape sequences (newlines, tabs, or escaped quotes)"
    where text = lexemeText fmt


analyse :: FilePath -> [Node () (Lexeme Text)] -> [Text]
analyse file ast = reverse $ State.execState (traverseAst linter (file, ast)) []
