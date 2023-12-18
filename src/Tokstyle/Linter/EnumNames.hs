{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.EnumNames (analyse) where

import           Control.Monad               (unless)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (HasDiagnostics (..), warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)


data Linter = Linter
    { diags    :: [Text]
    , enumName :: Text
    , prefix   :: Text
    }

empty :: Linter
empty = Linter [] "" ""

instance HasDiagnostics Linter where
    addDiagnostic diag l@Linter{diags} = l{diags = addDiagnostic diag diags}


linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            EnumConsts (Just (L _ _ enumName)) _ -> do
                let prefix = Text.toUpper enumName <> "_"
                State.withState (\s -> s{enumName, prefix}) act

            EnumDecl (L _ _ enumName) _ _ -> do
                let prefix = Text.toUpper enumName <> "_"
                State.withState (\s -> s{enumName, prefix}) act

            Enumerator (L _ _ name) _ -> do
                Linter{enumName, prefix} <- State.get
                unless (Text.isPrefixOf prefix name) $
                    warn file node $
                        "enumerator `" <> name <> "` in enum `" <> enumName
                        <> "` should start with `" <> prefix <> "`"

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . diags . flip State.execState empty . traverseAst linter
