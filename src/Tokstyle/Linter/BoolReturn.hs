{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.BoolReturn (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (Lexeme, LiteralType (..), Node,
                                              NodeF (..))
import qualified Language.Cimple.Diagnostics as Diagnostics
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)

pattern ReturnBool :: Node a
pattern ReturnBool <- Fix (Return (Just (Fix (LiteralExpr Bool _))))

checkStmts :: FilePath -> [Node (Lexeme Text)] -> State [Text] ()
checkStmts _ [] = return ()
checkStmts file [s@(Fix (IfStmt _ (Fix (CompoundStmt [ReturnBool])) Nothing)), ReturnBool] = do
    Diagnostics.warn file s "if-statement followed by boolean return can be simplified to return"
checkStmts file [s@(Fix (IfStmt _ (Fix (CompoundStmt [ReturnBool])) (Just (Fix (CompoundStmt [ReturnBool])))))] = do
    Diagnostics.warn file s "if/else with return true/false can be simplified to return"
checkStmts file (_:ss) = checkStmts file ss


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            CompoundStmt stmts -> do
                checkStmts file stmts
                act
            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
