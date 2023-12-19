{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.SemFmt.EnumFromInt (analyse) where

import           Control.Applicative        ((<|>))
import           Data.Fix                   (Fix (..))
import           Data.List.Extra            (firstJust)
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Language.Cimple            (Lexeme (..), LiteralType (..),
                                             Node, NodeF (..))
import           Tokstyle.Common.EnumLinter (MkFunBody, analyseEnums)


funSuffix :: Text
funSuffix = "_from_int"

mkCase :: Node (Lexeme Text) -> Maybe (Node (Lexeme Text))
mkCase (Fix Comment{}) = Nothing
mkCase (Fix (Enumerator name _)) = Just $
    -- case $name: return $name;
    Fix (Case (Fix (LiteralExpr ConstId name))
         (Fix (Return (Just (Fix (LiteralExpr ConstId name))))))
mkCase node = error $ show node

mkFunBody :: MkFunBody
mkFunBody _ varName enumrs = do
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


analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse = analyseEnums funSuffix mkFunBody
