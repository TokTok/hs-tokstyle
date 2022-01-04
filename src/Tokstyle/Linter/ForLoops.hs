{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.ForLoops (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (AssignOp (..), IdentityActions,
                                              Lexeme (..), Node, NodeF (..),
                                              defaultActions, doNode,
                                              traverseAst)
import           Language.Cimple.Diagnostics (warn)


linter :: IdentityActions (State [Text]) Text
linter = defaultActions
    { doNode = \file node act ->
        case unFix node of
            ForStmt (Fix (VarDeclStmt (Fix VarDecl{}) Just{})) _ _ _ -> act
            ForStmt (Fix (AssignExpr (Fix VarExpr{}) AopEq _)) _ _ _ -> act
            ForStmt i _ _ _ -> do
                warn file node $ "ForLoops: " <> Text.pack (show i)
                act

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
