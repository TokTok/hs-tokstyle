{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.SemFmt.EnumFromInt (descr) where

import           Control.Applicative        ((<|>))
import           Data.Fix                   (Fix (..))
import           Data.List.Extra            (firstJust)
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Language.Cimple            (AssignOp (..), Lexeme (..),
                                             LexemeClass (..), LiteralType (..),
                                             Node, NodeF (..), UnaryOp (..))
import           Tokstyle.Common.EnumLinter (MkFunBody, analyseEnums, mkLAt)


funSuffix :: Text
funSuffix = "_from_int"

mkCase :: Node (Lexeme Text) -> Maybe (Node (Lexeme Text))
mkCase (Fix Comment{}) = Nothing
mkCase (Fix (Enumerator name _)) = Just $
    -- case $name: return $name;
    Fix (Case (Fix (LiteralExpr ConstId name))
         (mkAssignOut name (LitTrue, "true")))
mkCase node = error $ show node

mkAssignOut :: Lexeme Text -> (LexemeClass, Text) -> Node (Lexeme Text)
mkAssignOut name (retCls, retStr) =
    let outDeref = Fix (UnaryExpr UopDeref (Fix (VarExpr (mkLAt name IdVar "out")))) in
    Fix $ CompoundStmt
        [ Fix (ExprStmt (Fix (AssignExpr outDeref AopEq (Fix (LiteralExpr ConstId name)))))
        , Fix (Return (Just (Fix (LiteralExpr Bool (mkLAt name retCls retStr)))))
        ]

mkFunBody :: MkFunBody
mkFunBody _ varName enumrs = do
    dn <- defaultName
    let defaultCase = Fix (Default (mkAssignOut dn (LitFalse, "false")))
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

descr :: ([(FilePath, [Node (Lexeme Text)])] -> [Text], (Text, Text))
descr = (analyse, ("enum-from-int", Text.unlines
    [ "Checks that `_from_int` functions for `enum`s are complete."
    , ""
    , "**Reason:** ensures that no enumerators are missed in conversion functions that"
    , "turn `int`s into `enum`s. Type-cast is not permitted, because some values of"
    , "type `int` are not in the enumeration."
    ]))
