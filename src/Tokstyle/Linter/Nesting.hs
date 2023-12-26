{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.Nesting (descr) where

import           Control.Monad               (when)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..), foldFix)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)

maxNesting :: Int
maxNesting = 7  -- TODO(iphydf): Reduce.

countNesting :: NodeF (Lexeme Text) Int -> Int
countNesting = \case
    CompoundStmt ns -> 1 + foldr max 0 ns
    ns              -> foldr max 0 ns


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionDefn{} ->
                let nesting = foldFix countNesting node in
                when (nesting > maxNesting) $
                    warn file node $ "function is too deeply nested: "
                        <> Text.pack (show nesting) <> " is deeper than the "
                        <> "maximum allowed of " <> Text.pack (show maxNesting)
                        <> "; consider inversion or extraction"

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("nesting", Text.unlines
    [ "Warns if a function has more than " <> Text.pack (show maxNesting) <> " nesting levels."
    , ""
    , "**Reason:** deep nesting makes functions more difficult to comprehend."
    ]))
