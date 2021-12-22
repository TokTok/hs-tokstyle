{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Cimple.Analysis.DeclaredOnce (analyse) where

import           Control.Monad.State.Lazy    (State)
import qualified Control.Monad.State.Lazy    as State
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import           Language.Cimple             (Lexeme (..), LexemeClass (..),
                                              Node (..))
import           Language.Cimple.Diagnostics (HasDiagnostics (..), warn)
import           Language.Cimple.TraverseAst (AstActions (..), defaultActions,
                                              traverseAst)


data Linter = Linter
    { diags :: [Text]
    , decls :: Map Text (FilePath, Lexeme Text)
    }

empty :: Linter
empty = Linter [] Map.empty

instance HasDiagnostics Linter where
    addDiagnostic diag l@Linter{diags} = l{diags = addDiagnostic diag diags}


linter :: AstActions (State Linter) Text
linter = defaultActions
    { doNode = \file node act ->
        case node of
            FunctionDecl _ (FunctionPrototype _ fn@(L _ IdVar fname) _) _ -> do
                l@Linter{decls} <- State.get
                case Map.lookup fname decls of
                    Nothing -> State.put l{decls = Map.insert fname (file, fn) decls }
                    Just (file', fn') -> do
                        warn file' fn' $ "duplicate declaration of function `" <> fname <> "'"
                        warn file fn $ "function `" <> fname <> "' also declared here"
                act

            _ -> act
    }

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse tus = reverse . diags $ State.execState (traverseAst linter tus) empty
