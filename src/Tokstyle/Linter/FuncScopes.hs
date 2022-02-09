{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.FuncScopes (analyse) where

import           Control.Monad               (when)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..),
                                              Scope (..), lexemeLine,
                                              lexemeText)
import           Language.Cimple.Diagnostics (HasDiagnostics (..), warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)


data Linter = Linter
    { diags :: [Text]
    , decls :: [(Text, (Lexeme Text, Scope))]
    }

empty :: Linter
empty = Linter [] []

instance HasDiagnostics Linter where
    addDiagnostic diag l@Linter{diags} = l{diags = addDiagnostic diag diags}


linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionDecl declScope (Fix (FunctionPrototype _ name _)) ->
                State.modify $ \l@Linter{decls} -> l{decls = (lexemeText name, (name, declScope)) : decls}
            FunctionDefn defnScope (Fix (FunctionPrototype _ name _)) _ -> do
                Linter{decls} <- State.get
                case lookup (lexemeText name) decls of
                    Nothing -> return ()
                    Just (decl, declScope) -> do
                        when (declScope /= defnScope) $ warn file name $
                            warning decl declScope defnScope

            _ -> act
    }
  where
    warning decl declScope defnScope =
        "function definition `" <> lexemeText decl
        <> "` does not agree with its declaration about scope: "
        <> "declaration on line " <> Text.pack (show (lexemeLine decl))
        <> " is " <> scopeKeyword declScope <> " but definition is "
        <> scopeKeyword defnScope

    scopeKeyword Global = "extern"
    scopeKeyword Static = "static"

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . diags . flip State.execState empty . traverseAst linter
