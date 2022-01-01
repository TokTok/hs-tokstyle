{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.GlobalFuncs (analyse) where

import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (Lexeme (..), Node, NodeF (..),
                                              Scope (..), lexemeText)
import           Language.Cimple.Diagnostics (warn')
import           System.FilePath             (takeExtension)


analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse (file, _) | takeExtension file /= ".c" = []
analyse (file, ast) = reverse $ State.execState (mapM go ast) []
  where
    go (Fix (FunctionDecl Global (Fix (FunctionPrototype _ name _)))) =
        warn' file name $
            "global function `" <> lexemeText name <> "' declared in .c file"
    go _ = return ()
