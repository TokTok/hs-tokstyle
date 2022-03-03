{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.Booleans (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (Lexeme, LiteralType (..), Node,
                                              NodeF (..))
import qualified Language.Cimple.Diagnostics as Diagnostics
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)

pattern ReturnBool, OnlyReturnBool :: Node a
-- | `return true` or `return false`.
pattern ReturnBool <- Fix (Return (Just (Fix (LiteralExpr Bool _))))
-- | A compound statement with only a return true/false in it.
pattern OnlyReturnBool <- Fix (CompoundStmt [ReturnBool])

checkStmts :: FilePath -> [Node (Lexeme Text)] -> State [Text] ()
checkStmts _ [] = return ()
checkStmts file [s@(Fix (IfStmt _ OnlyReturnBool Nothing)), ReturnBool] = do
    Diagnostics.warn file s "if-statement followed by boolean return can be simplified to return"
checkStmts file [s@(Fix (IfStmt _ OnlyReturnBool (Just OnlyReturnBool)))] = do
    Diagnostics.warn file s "if/else with return true/false can be simplified to return"
checkStmts file (_:ss) = checkStmts file ss


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            BinaryExpr (Fix (LiteralExpr Bool _)) _ _ -> warnBool file node
            BinaryExpr _ _ (Fix (LiteralExpr Bool _)) -> warnBool file node
            CompoundStmt stmts -> do
                checkStmts file stmts
                act
            _ -> act
    }
  where
      warnBool file node =
          Diagnostics.warn file node
              "boolean constants should not appear in binary expressions (use ! for negation)"

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter