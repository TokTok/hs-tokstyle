{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.ForLoops (analyse) where

import qualified Control.Monad.State.Lazy    as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (AssignOp (..), AstActions,
                                              Lexeme (..), Node, NodeF (..),
                                              defaultActions, doNode,
                                              traverseAst)
import           Language.Cimple.Diagnostics (warn)


linter :: AstActions [Text]
linter = defaultActions
    { doNode = \file node act ->
        case unFix node of
            ForStmt (Fix (VarDecl _ty (Fix (Declarator (Fix (DeclSpecVar _i)) (Just _v))))) _ _ _ -> act
            ForStmt (Fix (AssignExpr (Fix (VarExpr _i)) AopEq _v)) _ _ _ -> act
            ForStmt i _ _ _ -> do
                warn file node . Text.pack . show $ i
                act

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
