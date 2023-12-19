{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.SemFmt.EnumUnpack (analyse) where

import           Data.Fix                   (Fix (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Language.Cimple            (BinaryOp (..), Lexeme (..),
                                             LexemeClass (..), Node, NodeF (..),
                                             UnaryOp (..))
import           Tokstyle.Common.EnumLinter (MkFunBody, analyseEnums, mkLAt)


funSuffix :: Text
funSuffix = "_unpack"

-- {
--     uint32_t u32;
--
--     return bin_unpack_u32(bu, &u32)
--            && ${toLower ename}_from_int(u32, val);
-- }
mkFunBody :: MkFunBody
mkFunBody ename varName _ = return $
    Fix (CompoundStmt
        [Fix
           (VarDeclStmt
              (Fix
                 (VarDecl (Fix (TyStd (mkLAt varName IdStdType "uint32_t")))
                    (mkLAt varName IdVar "u32")
                    []))
              Nothing),
         Fix
           (Return
              (Just
                 (Fix
                    (BinaryExpr
                       (Fix
                          (FunctionCall
                             (Fix (VarExpr (mkLAt varName IdVar "bin_unpack_u32")))
                             [Fix (VarExpr (mkLAt varName IdVar "bu")),
                              Fix
                                (UnaryExpr UopAddress
                                   (Fix (VarExpr (mkLAt varName IdVar "u32"))))]))
                       BopAnd
                       (Fix
                          (FunctionCall
                             (Fix
                                (VarExpr
                                   (mkLAt varName IdVar (Text.toLower ename <> "_from_int"))))
                             [Fix (VarExpr (mkLAt varName IdVar "u32")),
                              Fix (VarExpr (mkLAt varName IdVar "val"))]))))))])


analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse = analyseEnums funSuffix mkFunBody
