{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.LoggerNoEscapes (analyse) where

import           Control.Monad               (when)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text, isInfixOf)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), LiteralType (String),
                                              Node, NodeF (..), lexemeText)
import qualified Language.Cimple.Diagnostics as Diagnostics
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act -> case unFix node of
        -- LOGGER_ASSERT has its format as the third parameter.
        FunctionCall (Fix (LiteralExpr _ (L _ _ "LOGGER_ASSERT"))) (_ : _ : Fix (LiteralExpr String fmt) : _)
            -> do
                checkFormat file fmt
                act

        FunctionCall (Fix (LiteralExpr _ (L _ _ func))) (_ : Fix (LiteralExpr String fmt) : _)
            | Text.isPrefixOf "LOGGER_" func
            -> do
                checkFormat file fmt
                act

        _ -> act
    }


checkFormat :: FilePath -> Lexeme Text -> State [Text] ()
checkFormat file fmt =
    when ("\\" `isInfixOf` text) $
        Diagnostics.warn file fmt $
            "logger format "
            <> text
            <> " contains escape sequences (newlines, tabs, or escaped quotes)"
    where text = lexemeText fmt


analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
