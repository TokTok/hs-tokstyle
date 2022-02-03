{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.TypedefName where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             ( Lexeme (..),
                                              Node, NodeF (..), lexemeText)
import           Language.Cimple.TraverseAst             (AstActions,
                                              astActions,
                                              doNode, traverseAst)
import           Language.Cimple.Diagnostics (warn)


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            Typedef (Fix (TyStruct sname)) tname | lexemeText sname /= lexemeText tname -> do
                warn file sname $ warning "struct" tname sname
            Typedef (Fix (Struct sname _)) tname | lexemeText sname /= lexemeText tname -> do
                warn file sname $ warning "struct" tname sname
            Typedef (Fix (Union uname _)) tname | lexemeText uname /= lexemeText tname -> do
                warn file uname $ warning "union" tname uname
            EnumDecl ename _ tname | lexemeText ename /= lexemeText tname -> do
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
