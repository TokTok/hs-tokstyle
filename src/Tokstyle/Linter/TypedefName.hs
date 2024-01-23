{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.TypedefName (descr) where

import           Control.Applicative         ((<|>))
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..),
                                              lexemeText)
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)

valid :: Lexeme Text -> Lexeme Text -> Bool
valid (L _ _ tname) (L _ _ sname) =
    sname == tname || fromMaybe False (do
        t <- Text.stripSuffix "_t" tname
        s <- Text.stripSuffix "_s" sname <|> Text.stripSuffix "_u" sname
        return $ t == s)

linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            Typedef (Fix (TyStruct sname)) tname | not $ valid tname sname ->
                warn file sname $ warning "struct" tname sname
            Typedef (Fix (Struct sname _)) tname | not $ valid tname sname ->
                warn file sname $ warning "struct" tname sname
            Typedef (Fix (Union uname _)) tname | not $ valid tname uname ->
                warn file uname $ warning "union" tname uname
            EnumDecl ename _ tname | not $ valid tname ename ->
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
