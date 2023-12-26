{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.FuncPrototypes (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme, Node, NodeF (..))
import qualified Language.Cimple.Diagnostics as Diagnostics
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionPrototype _ name [] -> do
                Diagnostics.warn file name "empty parameter list must be written as `(void)`"
                act

            FunctionDefn{} -> return ()
            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("func-prototypes", Text.unlines
    [ "Checks that empty parameter lists in C functions are written as `(void)`."
    , ""
    , "**Reason:** old-style empty parameter lists written as `()` are risky, because"
    , "C interprets them as variadic. GCC warns about this but sometimes misses one."
    ]))
