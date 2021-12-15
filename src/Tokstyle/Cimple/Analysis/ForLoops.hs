{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.ForLoops (analyse) where

import           Control.Monad.State.Lazy    (State)
import qualified Control.Monad.State.Lazy    as State
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (AlexPosn (..), AssignOp (..),
                                              Lexeme (..), LexemeClass (..),
                                              Node (..))
import qualified Language.Cimple.Diagnostics as Diagnostics
import           Language.Cimple.TraverseAst (AstActions (..), defaultActions,
                                              traverseAst)


linter :: FilePath -> AstActions (State [Text]) Text
linter file = defaultActions
    { doNode = \node act ->
        case node of
            ForStmt (VarDecl _ty (Declarator (DeclSpecVar _i) (Just _v))) _ _ _ -> do
                act

            ForStmt (AssignExpr (VarExpr _i) AopEq _v) _ _ _ -> do
                act

            ForStmt i _ _ _ -> do
                warn (L (AlexPn 0 0 0) KwFor "for") . Text.pack . show $ i
                act

            _ -> act
    }
  where warn = Diagnostics.warn file

analyse :: FilePath -> [Node (Lexeme Text)] -> [Text]
analyse file ast = reverse $ State.execState (traverseAst (linter file) ast) []
