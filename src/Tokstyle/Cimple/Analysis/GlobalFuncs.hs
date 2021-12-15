{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.GlobalFuncs (analyse) where

import qualified Control.Monad.State.Lazy    as State
import           Data.Text                   (Text)
import           Language.Cimple             (Lexeme (..), Node (..),
                                              Scope (..), lexemeText)
import           Language.Cimple.Diagnostics (warn)
import           System.FilePath             (takeExtension)


analyse :: FilePath -> [Node (Lexeme Text)] -> [Text]
analyse file _ | takeExtension file /= ".c" = []
analyse file ast = reverse $ State.execState (mapM go ast) []
  where
    go (FunctionDecl Global (FunctionPrototype _ name _) _) =
        warn file name $
            "global function `" <> lexemeText name <> "' declared in .c file"
    go _ = return ()
