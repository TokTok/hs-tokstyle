{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.SemFmt.EnumToString (descr) where

import           Data.Fix                   (Fix (..))
import           Data.Maybe                 (mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Language.Cimple            (Lexeme (..), LexemeClass (..),
                                             LiteralType (..), Node, NodeF (..),
                                             lexemeText)
import           Tokstyle.Common.EnumLinter (EnumInfo (EnumInfo), MkFunBody,
                                             analyseEnums, mkLAt)


funSuffix :: Text
funSuffix = "_to_string"

mkReturnString :: Lexeme Text -> Text -> Node (Lexeme Text)
mkReturnString at str = Fix (Return (Just (Fix (LiteralExpr String (mkLAt at LitString str)))))

mkCase :: Node (Lexeme Text) -> Maybe (Node (Lexeme Text))
mkCase (Fix Comment{}) = Nothing
mkCase (Fix (Enumerator name _)) = Just $
    -- case $name: return "$name";
    Fix (Case (Fix (LiteralExpr ConstId name)) $
         mkReturnString name $ "\"" <> lexemeText name <> "\"")
mkCase node = error $ show node

mkFunBody :: MkFunBody
mkFunBody _ varName (EnumInfo ename enumrs) = do
    return $ Fix (CompoundStmt
        [ Fix (SwitchStmt (Fix (VarExpr varName)) (mapMaybe mkCase enumrs))
        , mkReturnString varName $ "\"<invalid " <> ename <> ">\""
        ])


analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse = analyseEnums funSuffix mkFunBody

descr :: ([(FilePath, [Node (Lexeme Text)])] -> [Text], (Text, Text))
descr = (analyse, ("enum-to-string", Text.unlines
    [ "Checks that `_to_string` functions for `enum`s are complete."
    , ""
    , "**Reason:** we provide `to_string` functions for `enum` but don't want to"
    , "manually maintain them. This linter checks that the function is exactly what"
    , "we want it to be, and the error message will say what the function should look"
    , "like."
    ]))
