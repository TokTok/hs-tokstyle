{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.Parens (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Maybe                  (maybeToList)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
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

            IfStmt (Fix (ParenExpr c)) t e ->
                traverseAst linter (file, [c, t] ++ maybeToList e)

            Return (Just (Fix ParenExpr{})) -> do
                warn file node "return expression does not need parentheses"
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

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("parens", Text.unlines
    [ "Suggests removing parentheses where they are not needed:"
    , ""
    , "- in return expressions, e.g. `return(something);` should be `return something;`."
    , "- in initialisers, e.g. `int foo = (something);` should be `int foo = something;`."
    , "- in assignments, e.g. `foo = (something);` should be `foo = something;`."
    , "- in parentheses, e.g. `((something))` should be `(something)`."
    , ""
    , "**Reason:** sometimes extra parentheses add clarity, so we don't forbid all"
    , "redundant parentheses, but in the above cases, they don't add clarity and only"
    , "add more syntax and confusion as to why there are extra parentheses there."
    ]))
