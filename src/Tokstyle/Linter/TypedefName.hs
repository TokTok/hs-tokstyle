{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.TypedefName (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..),
                                              lexemeText)
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            Typedef (Fix (TyStruct sname)) tname | lexemeText sname /= lexemeText tname ->
                warn file sname $ warning "struct" tname sname
            Typedef (Fix (Struct sname _)) tname | lexemeText sname /= lexemeText tname ->
                warn file sname $ warning "struct" tname sname
            Typedef (Fix (Union uname _)) tname | lexemeText uname /= lexemeText tname ->
                warn file uname $ warning "union" tname uname
            EnumDecl ename _ tname | lexemeText ename /= lexemeText tname ->
                warn file ename $ warning "union" tname ename

            FunctionDefn{} -> return ()
            _ -> act
    }
  where
    warning tag tname name =
        "typedef name `" <> lexemeText tname <> "` does not match " <> tag
        <> " name `" <> lexemeText name <> "`"

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("typedef-name", Text.unlines
    [ "Checks that typedef names match the struct/union name. E.g."
    , "`typedef struct Foo_ { ... } Foo;` should instead be"
    , "`typedef struct Foo { ... } Foo;`."
    , ""
    , "**Reason:** there is no good reason for them to be different, and it adds"
    , "confusion and a potential for C++ code to pick the wrong name and later break"
    , "in refactorings."
    ]))
