{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.FuncPrototypes (analyse) where

import qualified Control.Monad.State.Lazy    as State
import           Data.Text                   (Text)
import           Language.Cimple             (AstActions, Lexeme, Node (..),
                                              defaultActions, doNode,
                                              traverseAst)
import qualified Language.Cimple.Diagnostics as Diagnostics


linter :: AstActions [Text]
linter = defaultActions
    { doNode = \file node act ->
        case node of
            FunctionPrototype _ name [] -> do
                Diagnostics.warn file name "empty parameter list must be written as (void)"
                act

            _ -> act
    }

analyse :: (FilePath, [Node () (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
