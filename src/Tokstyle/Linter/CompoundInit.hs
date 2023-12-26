{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.CompoundInit (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)

linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            VarDeclStmt _ (Just (Fix CompoundLiteral{})) ->
                warn file node "don't use compound literals in initialisations; use simple `Type var = {0};`"

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("compound-init", Text.unlines
    [ "Checks that compound literals aren't used in initialisations. E.g.:"
    , ""
    , "```cpp"
    , "Foo foo = (Foo){0};"
    , "```"
    , ""
    , "should be written as:"
    , ""
    , "```cpp"
    , "Foo foo = {0};"
    , "```"
    , ""
    , "**Reason:** compound literals aren't needed in initialisations. Without them,"
    , "the code is clearer."
    ]))
