{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.GlobalFuncs (analyse) where

import qualified Control.Monad.State.Lazy    as State
import           Data.Text                   (Text)
import           System.FilePath             (takeExtension)
import           Tokstyle.Cimple.AST         (Node (..), Scope (..))
import           Tokstyle.Cimple.Diagnostics (warn)
import           Tokstyle.Cimple.Lexer       (Lexeme (..), lexemeText)


analyse :: FilePath -> [Node (Lexeme Text)] -> [Text]
analyse file _ | takeExtension file /= ".c" = []
analyse file ast = reverse $ snd $ State.runState (mapM go ast) []
  where
    go (FunctionDecl Global (FunctionPrototype _ name _)) =
        warn file name $
            "global function `" <> lexemeText name <> "' declared in .c file"
    go _ = return ()
