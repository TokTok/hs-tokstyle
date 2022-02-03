{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.CompoundInit (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (Lexeme (..),
                                              Node, NodeF (..))
import           Language.Cimple.TraverseAst             (AstActions,
                                              astActions,
                                              doNode, traverseAst)
import           Language.Cimple.Diagnostics (warn)

linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            VarDeclStmt _ (Just (Fix CompoundExpr{})) -> do
                warn file node "don't use compound literals in initialisations - use simple `Type var = {0};`"

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
