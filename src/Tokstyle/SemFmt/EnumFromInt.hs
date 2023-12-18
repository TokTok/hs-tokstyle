{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.SemFmt.EnumFromInt (analyse) where

import           Control.Applicative         ((<|>))
import           Control.Monad               (unless)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.List.Extra             (firstJust)
import           Data.Maybe                  (mapMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), LiteralType (..),
                                              Node, NodeF (..))
import           Language.Cimple.Diagnostics (HasDiagnostics (..), warn)
import           Language.Cimple.Pretty      (ppTranslationUnit, render)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Tokstyle.Common             (semEq)


data Linter = Linter
    { diags :: [Text]
    , enums :: [(Text, [Node (Lexeme Text)])]
    }

instance HasDiagnostics Linter where
    addDiagnostic diag l@Linter{diags} = l{diags = addDiagnostic diag diags}

empty :: Linter
empty = Linter [] []

funSuffix :: Text
funSuffix = "_from_int"

mkFunBody :: Lexeme Text -> [Node (Lexeme Text)] -> Maybe (Node (Lexeme Text))
mkFunBody varName enumrs = do
    dn <- defaultName
    let defaultCase = Fix (Default (Fix (Return (Just (Fix (LiteralExpr ConstId dn))))))
    return $ Fix (CompoundStmt
        [Fix (SwitchStmt (Fix (VarExpr varName)) (mapMaybe mkCase enumrs ++ [defaultCase]))])
  where
    defaultName =
        firstJust isDefault enumrs <|> firstJust isEnumr enumrs
    isDefault (Fix (Enumerator l@(L _ _ name) _))
        | "_INVALID" `Text.isSuffixOf` name = Just l
    isDefault _ = Nothing
    isEnumr (Fix (Enumerator l _)) = Just l
    isEnumr _                      = Nothing

    mkCase (Fix Comment{}) = Nothing
    mkCase (Fix (Enumerator name _)) = Just $
        -- case $name: return $name;
        Fix (Case (Fix (LiteralExpr ConstId name))
             (Fix (Return (Just (Fix (LiteralExpr ConstId name))))))
    mkCase node = error $ show node


collectEnums :: AstActions (State Linter) Text
collectEnums = astActions
    { doNode = \file node act ->
        case unFix node of
            EnumDecl (L _ _ ename) enumrs _ -> do
                l@Linter{enums} <- State.get
                case lookup ename enums of
                    Nothing -> State.put l{enums = (Text.toLower ename, enumrs):enums}
                    Just{} -> warn file node $ "duplicate enum: " <> ename

            _ -> act
    }

linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionDefn _ (Fix (FunctionPrototype _ (L _ _ fname) [Fix (VarDecl _ varName _)])) body
                | funSuffix `Text.isSuffixOf` fname -> do
                Linter{enums} <- State.get
                case lookup (Text.dropEnd (Text.length funSuffix) fname) enums of
                    Nothing -> warn file node $ "enum not found for function: " <> fname
                    Just enumrs -> do
                        case mkFunBody varName enumrs of
                            Nothing ->
                                warn file node $ "invalid enum format for " <> fname
                            Just wanted ->
                                unless (body `semEq` wanted) $
                                    warn file node $ "enum from_int function should be:\n"
                                        <> render (ppTranslationUnit [wanted])

            _ -> act
    }

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse = reverse . diags . flip State.execState empty . (\tus -> traverseAst collectEnums tus >> traverseAst linter tus) . reverse
