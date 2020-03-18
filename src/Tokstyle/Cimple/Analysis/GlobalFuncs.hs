{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.GlobalFuncs (analyse) where

import           Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           System.FilePath          (takeExtension)
import           Tokstyle.Cimple.AST      (Node (..), Scope (..))
import           Tokstyle.Cimple.Lexer    (AlexPosn (..), Lexeme (..),
                                           lexemeText)

type Diagnostics a = State [Text] a

warn :: FilePath -> Lexeme Text -> Text -> Diagnostics ()
warn file (L (AlexPn _ line _) _ _) w = do
    diags <- State.get
    State.put $ diag : diags
  where
    diag =
        Text.pack file <> ":" <> Text.pack (show line) <> ": " <> w


analyse :: FilePath -> [Node (Lexeme Text)] -> [Text]
analyse file _ | takeExtension file /= ".c" = []
analyse file ast = reverse $ snd $ State.runState (mapM go ast) []
  where
    go (FunctionDecl Global (FunctionPrototype _ name _)) =
        warn file name $
            "global function `" <> lexemeText name <> "' defined in .c file"
    go _ = return ()
