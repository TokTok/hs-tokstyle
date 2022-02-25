{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.Parens (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)


needsParens :: Node a -> Bool
needsParens n = case unFix n of
    BinaryExpr{}  -> True
    TernaryExpr{} -> True
    CastExpr{}    -> True
    _             -> False


checkArg :: FilePath -> Node (Lexeme Text) -> State [Text] ()
checkArg file arg = case unFix arg of
    ParenExpr{} -> warn file arg "function call argument does not need parentheses"
    _           -> return ()


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            -- Extra parentheses inside macro body is allowed (and sometimes needed).
            PreprocDefineConst{} -> return ()
            PreprocDefineMacro{} -> return ()

            FunctionCall _ args -> do
                mapM_ (checkArg file) args
                act

            VarDeclStmt _ (Just (Fix ParenExpr{})) -> do
                warn file node "variable initialiser does not need parentheses"
                act
            AssignExpr _ _ (Fix ParenExpr{}) -> do
                warn file node "the right hand side of assignments does not need parentheses"
                act
            ParenExpr expr | not $ needsParens expr -> do
                warn file node "expression does not need parentheses"
                act

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
