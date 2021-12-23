{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.ForLoops (analyse) where

import qualified Control.Monad.State.Lazy    as State
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (AssignOp (..), AstActions,
                                              Lexeme (..), Node (..),
                                              defaultActions, doNode,
                                              traverseAst)
import qualified Language.Cimple.Diagnostics as Diagnostics


linter :: AstActions [Text]
linter = defaultActions
    { doNode = \file node act ->
        case node of
            ForStmt (VarDecl _ty (Declarator (DeclSpecVar _i) (Just _v))) _ _ _ -> act
            ForStmt (AssignExpr (VarExpr _i) AopEq _v) _ _ _ -> act
            ForStmt i _ _ _ -> do
                warn file node . Text.pack . show $ i
                act

            _ -> act
    }
  where warn file node = Diagnostics.warn file (Diagnostics.at node)

analyse :: FilePath -> [Node () (Lexeme Text)] -> [Text]
analyse file ast = reverse $ State.execState (traverseAst linter (file, ast)) []
