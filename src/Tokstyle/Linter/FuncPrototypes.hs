{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.FuncPrototypes (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (IdentityActions, Lexeme, Node,
                                              NodeF (..), defaultActions,
                                              doNode, traverseAst)
import qualified Language.Cimple.Diagnostics as Diagnostics


linter :: IdentityActions (State [Text]) Text
linter = defaultActions
    { doNode = \file node act ->
        case unFix node of
            FunctionPrototype _ name [] -> do
                Diagnostics.warn file name "empty parameter list must be written as (void)"
                act

            FunctionDefn{} -> return node
            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
