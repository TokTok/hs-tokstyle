{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Common.EnumLinter
    ( EnumInfo (..)
    , MkFunBody
    , analyseEnums
    , mkLAt
    ) where

import           Control.Monad               (unless)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), LexemeClass (..),
                                              Node, NodeF (..))
import           Language.Cimple.Diagnostics (HasDiagnostics (..), warn)
import           Language.Cimple.Pretty      (ppTranslationUnit, render)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Tokstyle.Common             (semEq)

data EnumInfo = EnumInfo
    { enumName    :: Text
    , enumMembers :: [Node (Lexeme Text)]
    }

type SymbolTable = [(Text, EnumInfo)]

data Linter = Linter
    { diags :: [Text]
    , types :: SymbolTable
    }

instance HasDiagnostics Linter where
    addDiagnostic diag l@Linter{diags} = l{diags = addDiagnostic diag diags}

empty :: Linter
empty = Linter [] []

mkLAt :: Lexeme a -> LexemeClass -> a -> Lexeme a
mkLAt (L p _ _) = L p

collectEnums :: [(FilePath, [Node (Lexeme Text)])] -> State Linter ()
collectEnums = traverseAst actions
  where
    actions :: AstActions (State Linter) Text
    actions = astActions
        { doNode = \file node act ->
            case unFix node of
                EnumDecl (L _ _ ename) enumrs _ -> do
                    l@Linter{types} <- State.get
                    case lookup ename types of
                        Nothing -> State.put l{types = (Text.toLower ename, EnumInfo ename enumrs):types}
                        Just{} -> warn file node $ "duplicate enum: " <> ename

                _ -> act
        }

type MkFunBody = SymbolTable -> Lexeme Text -> EnumInfo -> Maybe (Node (Lexeme Text))

checkEnums :: Text -> MkFunBody -> [(FilePath, [Node (Lexeme Text)])] -> State Linter ()
checkEnums funSuffix mkFunBody = traverseAst actions
  where
    actions :: AstActions (State Linter) Text
    actions = astActions
        { doNode = \file node act ->
            case unFix node of
                FunctionDefn _ (Fix (FunctionPrototype _ (L _ _ fname) (Fix (VarDecl _ varName _):_))) body
                    | funSuffix `Text.isSuffixOf` fname -> do
                    Linter{types} <- State.get
                    case lookup (Text.dropEnd (Text.length funSuffix) fname) types of
                        Nothing -> return ()  -- not every _to_string function is for enums
                        Just e@(EnumInfo ename _) -> do
                            case mkFunBody types varName e of
                                Nothing ->
                                    warn file node $ "invalid enum format for `" <> ename <> "`"
                                Just wanted ->
                                    unless (body `semEq` wanted) $
                                        warn file node $ "enum `" <> funSuffix <> "` function for `" <> ename <> "` should be:\n"
                                            <> render (ppTranslationUnit [wanted])

                _ -> act
        }


analyseEnums :: Text -> MkFunBody -> [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyseEnums funSuffix mkFunBody =
    reverse . diags . flip State.execState empty . (\tus -> collectEnums tus >> checkEnums funSuffix mkFunBody tus) . reverse
