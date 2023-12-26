{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.Booleans (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme, LiteralType (..), Node,
                                              NodeF (..))
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)

pattern ReturnBool, OnlyReturnBool :: Node a
-- | `return true` or `return false`.
pattern ReturnBool <- Fix (Return (Just (Fix (LiteralExpr Bool _))))
-- | A compound statement with only a return true/false in it.
pattern OnlyReturnBool <- Fix (CompoundStmt [ReturnBool])

checkStmts :: FilePath -> [Node (Lexeme Text)] -> State [Text] ()
checkStmts _ [] = return ()
checkStmts file [s@(Fix (IfStmt _ OnlyReturnBool Nothing)), ReturnBool] =
    warn file s "if-statement followed by boolean return can be simplified to return"
checkStmts file [s@(Fix (IfStmt _ OnlyReturnBool (Just OnlyReturnBool)))] =
    warn file s "if/else with return true/false can be simplified to return"
checkStmts file (_:ss) = checkStmts file ss


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            BinaryExpr (Fix (LiteralExpr Bool _)) _ _ -> warn file node message
            BinaryExpr _ _ (Fix (LiteralExpr Bool _)) -> warn file node message
            CompoundStmt stmts -> do
                checkStmts file stmts
                act
            _ -> act
    }
  where
    message = "boolean constants should not appear in binary expressions (use ! for negation)"

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("booleans", Text.unlines
    [ "Checks for if/else statements that return true/false and could be simplified to"
    , "just return. E.g.:"
    , ""
    , "```cpp"
    , "bool foo(void) {"
    , "  if (check_something()) {"
    , "    return false;"
    , "  }"
    , "  return true;"
    , "}"
    , "```"
    , ""
    , "could be simplified to:"
    , ""
    , "```cpp"
    , "bool foo(void) {"
    , "  return !check_something();"
    , "}"
    , "```"
    , ""
    , "Also checks for the use of `true` or `false` in binary expressions. E.g."
    , "`a == true` should be `a` and `a != true` should be `!a`."
    , ""
    , "**Reason:** simpler code is easier to read."
    ]))
