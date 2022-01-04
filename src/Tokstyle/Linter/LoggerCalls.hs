{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.LoggerCalls (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (IdentityActions, Lexeme (..),
                                              LiteralType (String), Node,
                                              NodeF (..), defaultActions,
                                              doNode, traverseAst)
import qualified Language.Cimple.Diagnostics as Diagnostics
import           System.FilePath             (takeFileName)


linter :: IdentityActions (State [Text]) Text
linter = defaultActions
    { doNode = \file node act ->
        case unFix node of
            -- Ignore all function calls where the second argument is a string
            -- literal. If it's a logger call, it's a valid one.
            FunctionCall _ (_:Fix (LiteralExpr String _):_) -> act
            -- LOGGER_ASSERT has its format as the third parameter.
            FunctionCall (Fix (LiteralExpr _ (L _ _ "LOGGER_ASSERT"))) (_:_:Fix (LiteralExpr String _):_) -> act

            FunctionCall (Fix (LiteralExpr _ name@(L _ _ func))) _ | Text.isPrefixOf "LOGGER_" func -> do
                Diagnostics.warn file name $ "logger call `" <> func <> "' has a non-literal format argument"
                act

            _ -> act
    }


analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
-- Ignore logger.h, which contains a bunch of macros that call LOGGER functions
-- with their (literal) arguments. We don't know that they are literals at this
-- point, though.
analyse (file, _) | takeFileName file == "logger.h" = []
analyse tu = reverse . flip State.execState [] . traverseAst linter $ tu
