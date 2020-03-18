{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Diagnostics (Diagnostics, warn) where

import           Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Tokstyle.Cimple.Lexer    (Lexeme (..), lexemeLine)

type Diagnostics a = State [Text] a

warn :: FilePath -> Lexeme Text -> Text -> Diagnostics ()
warn file l w = do
    diags <- State.get
    State.put $ diag : diags
  where
    diag = Text.pack file <> ":" <> Text.pack (show (lexemeLine l)) <> ": " <> w
