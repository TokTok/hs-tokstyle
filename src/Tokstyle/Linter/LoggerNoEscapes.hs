{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.LoggerNoEscapes (descr) where

import           Control.Monad               (when)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
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
    when ("\\" `Text.isInfixOf` text) $
        Diagnostics.warn file fmt $
            "logger format "
            <> text
            <> " contains escape sequences (newlines, tabs, or escaped quotes)"
    where text = lexemeText fmt


analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("logger-no-escapes", Text.unlines
    [ "Checks that no escape sequences are present in the logger format string."
    , ""
    , "**Reason:** newlines, tabs, or double quotes are not permitted in log outputs"
    , "to ensure that each log output is a single line. It's particularly easy to"
    , "accidentally add `\\n` to the end of a log format. This avoids that problem."
    ]))
