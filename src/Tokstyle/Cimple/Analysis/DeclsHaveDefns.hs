{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Cimple.Analysis.DeclsHaveDefns (analyse) where

import qualified Control.Monad.State.Lazy    as State
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (mapMaybe)
import           Data.Text                   (Text)
import           Language.Cimple             (AstActions, Lexeme (..),
                                              LexemeClass (..), Node (..),
                                              defaultActions, doNode,
                                              traverseAst)
import qualified Language.Cimple.Diagnostics as Diagnostics
import           System.FilePath             (takeFileName)


data DeclDefn = DeclDefn
    { decl :: Maybe (FilePath, Lexeme Text)
    , defn :: Maybe (FilePath, Lexeme Text)
    }


collectPairs :: AstActions (Map Text DeclDefn)
collectPairs = defaultActions
    { doNode = \file node act ->
        case node of
            FunctionDecl _ (FunctionPrototype _ fn@(L _ IdVar fname) _) _ -> do
                State.modify $ \pairs ->
                    case Map.lookup fname pairs of
                        Nothing -> Map.insert fname (DeclDefn{ decl = Just (file, fn), defn = Nothing }) pairs
                        Just dd -> Map.insert fname (dd      { decl = Just (file, fn)                 }) pairs
                act

            FunctionDefn _ (FunctionPrototype _ fn@(L _ IdVar fname) _) _ -> do
                State.modify $ \pairs ->
                    case Map.lookup fname pairs of
                        Nothing -> Map.insert fname (DeclDefn{ decl = Nothing, defn = Just (file, fn) }) pairs
                        Just dd -> Map.insert fname (dd      {                 defn = Just (file, fn) }) pairs
                act

            _ -> act
    }

analyse :: [(FilePath, [Node () (Lexeme Text)])] -> [Text]
analyse =
    map makeDiagnostic
    . mapMaybe lacksDefn
    . Map.elems
    . flip State.execState Map.empty
    . traverseAst collectPairs
    . filter (not . (`elem` ["ccompat.h", "tox.h"]) . takeFileName . fst)
  where
    lacksDefn DeclDefn{decl, defn = Nothing} = decl
    lacksDefn _                              = Nothing

    makeDiagnostic (file, fn@(L _ _ fname)) =
        Diagnostics.sloc file fn <> ": missing definition for `" <> fname <> "'"
