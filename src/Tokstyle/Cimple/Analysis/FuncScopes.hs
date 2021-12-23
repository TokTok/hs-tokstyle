{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.FuncScopes (analyse) where

import           Control.Monad               (foldM, when)
import qualified Control.Monad.State.Lazy    as State
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node (..),
                                              Scope (..), lexemeLine,
                                              lexemeText)
import           Language.Cimple.Diagnostics (warn)


analyse :: FilePath -> [Node a (Lexeme Text)] -> [Text]
analyse file ast = reverse $ snd $ State.runState (foldM go [] ast) []
  where
    go decls (FunctionDecl declScope (FunctionPrototype _ name _) _) =
        return $ (lexemeText name, (name, declScope)) : decls
    go decls (FunctionDefn defnScope (FunctionPrototype _ name _) _) =
        case lookup (lexemeText name) decls of
            Nothing -> return decls
            Just (decl, declScope) -> do
                when (declScope /= defnScope) $ warn file name $
                    warning decl declScope defnScope
                return decls
    go decls _ = return decls

    warning decl declScope defnScope =
        "function definition `" <> lexemeText decl
        <> "' does not agree with its declaration about scope: "
        <> "declaration on line " <> Text.pack (show (lexemeLine decl))
        <> " is " <> scopeKeyword declScope <> " but definition is "
        <> scopeKeyword defnScope

    scopeKeyword Global = "extern"
    scopeKeyword Static = "static"
