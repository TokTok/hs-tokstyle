{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.DeclsHaveDefns (analyse) where

import           Control.Arrow               ((&&&))
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.List                   (sortOn)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (mapMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (AlexPosn (..), 
                                              Lexeme (..), LexemeClass (..),
                                              Node, NodeF (..), lexemeText)
import           Language.Cimple.TraverseAst             (AstActions,
                                              astActions,
                                              doNode, traverseAst)
import qualified Language.Cimple.Diagnostics as Diagnostics
import           System.FilePath             (takeFileName)
import           Text.EditDistance           (defaultEditCosts,
                                              levenshteinDistance)


maxEditDistance :: Int
maxEditDistance = 5


data DeclDefn = DeclDefn
    { decl :: Maybe (FilePath, Lexeme Text)
    , defn :: Maybe (FilePath, Lexeme Text)
    }

type Env = Map Text DeclDefn

empty :: Env
empty = Map.fromList
    [ ("ev_loop", inFile "ev.h")
    ]
  where
    inFile f = DeclDefn{ decl = Just (f, lexeme), defn = Just (f, lexeme) }
    lexeme = L (AlexPn 0 0 0) Eof ""

addDecl :: FilePath -> Lexeme Text -> State Env ()
addDecl file l@(L _ _ name) =
    State.modify $ \pairs ->
        case Map.lookup name pairs of
            Nothing -> Map.insert name (DeclDefn{ decl = Just (file, l), defn = Nothing }) pairs
            Just dd -> Map.insert name (dd      { decl = Just (file, l)                 }) pairs

addDefn :: FilePath -> Lexeme Text -> State Env ()
addDefn file l@(L _ _ name) =
    State.modify $ \pairs ->
        case Map.lookup name pairs of
            Nothing -> Map.insert name (DeclDefn{ decl = Nothing, defn = Just (file, l) }) pairs
            Just dd -> Map.insert name (dd      {                 defn = Just (file, l) }) pairs


collectPairs :: AstActions (State Env) Text
collectPairs = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionDecl _ (Fix (FunctionPrototype _ fname _)) -> do
                addDecl file fname

            FunctionDefn _ (Fix (FunctionPrototype _ fname _)) _ -> do
                addDefn file fname

            TyStruct sname -> do
                addDecl file sname

            Struct sname _ -> do
                addDefn file sname

            _ -> act
    }

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse =
    (\(a, b) -> concatMap (makeDiagnostic a) b)
    . (mapMaybe defn &&& mapMaybe lacksDefn)
    . Map.elems
    . flip State.execState empty
    . traverseAst collectPairs
    . filter (not . (`elem` ["ccompat.h", "tox.h"]) . takeFileName . fst)
  where
    lacksDefn DeclDefn{decl, defn = Nothing} = decl
    lacksDefn _                              = Nothing

makeDiagnostic :: [(FilePath, Lexeme Text)] -> (FilePath, Lexeme Text) -> [Text]
makeDiagnostic defns (file, fn@(L _ _ name)) =
    Diagnostics.sloc file fn <> ": missing definition for `" <> name <> "`"
    : suggestion
  where
    dists = sortOn fst . map ((levenshteinDistance defaultEditCosts (normalise fn) . normalise . snd) &&& id)
    normalise = Text.unpack . Text.toLower . lexemeText

    suggestion =
        case dists defns of
            (d, (dfile, dn@(L _ _ dname))):_ | d < maxEditDistance ->
                [Diagnostics.sloc dfile dn <> ": did you mean `" <> dname <> "`?"]
            _ -> []
