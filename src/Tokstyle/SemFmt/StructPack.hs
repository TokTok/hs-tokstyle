{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.SemFmt.StructPack (descr) where

import           Data.Fix                     (Fix (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Language.Cimple              (BinaryOp (..), Lexeme (..),
                                               LexemeClass (..),
                                               LiteralType (..), Node,
                                               NodeF (..), UnaryOp (..))
import           Tokstyle.Common.StructLinter (MkFunBody, analyseStructs, mkLAt)
import           Tokstyle.Common.TypeSystem   (StdType (..), TypeDescr (..),
                                               TypeInfo (..), TypeRef (..))


funSuffix :: Text
funSuffix = "_pack"

{-
return bin_pack_array(bp, 5)
           && bin_pack_u08(bp, foo->some_byte)
           && bin_pack_u16(bp, foo->some_short)
           && some_enum_pack(bp, foo->type)
           && bin_pack_bin(bp, foo->message, foo->message_length)
           && bin_pack_bin(bp, foo->key, 32);
-}
mkFunBody :: MkFunBody
mkFunBody _ varName (StructDescr _ [mem]) = do
    packMems <- mkPackMember varName mem
    return $ Right (Fix (CompoundStmt [Fix (Return (Just packMems))]))
mkFunBody _ varName (StructDescr sname mems) = do
    let packArray = mkPackArray sname (length mems)
    packMems <- foldr (\x y -> Fix (BinaryExpr y BopAnd x)) packArray . reverse <$> mapM (mkPackMember varName) mems
    return $ Right (Fix (CompoundStmt [Fix (Return (Just packMems))]))
mkFunBody _ _ ty = error $ show ty

mkPackArray :: Lexeme Text -> Int -> Node (Lexeme Text)
mkPackArray sname size =
    Fix (FunctionCall (Fix (VarExpr (mkLAt sname IdVar "bin_pack_array")))
        [ Fix (VarExpr (mkLAt sname IdVar "bp"))
        , Fix (LiteralExpr Int (mkLAt sname LitInteger (Text.pack $ show size)))
        ])

builtinPackFunName :: StdType -> Maybe Text
builtinPackFunName BoolTy = Just "bin_pack_bool"
builtinPackFunName U08Ty  = Just "bin_pack_u08"
builtinPackFunName S08Ty  = Just "bin_pack_s08"
builtinPackFunName U16Ty  = Just "bin_pack_u16"
builtinPackFunName S16Ty  = Just "bin_pack_s16"
builtinPackFunName U32Ty  = Just "bin_pack_u32"
builtinPackFunName S32Ty  = Just "bin_pack_s32"
builtinPackFunName U64Ty  = Just "bin_pack_u64"
builtinPackFunName S64Ty  = Just "bin_pack_s64"
builtinPackFunName _      = Nothing

packFunName :: TypeInfo -> Maybe (Either Text (Node (Lexeme Text) -> Node (Lexeme Text), Text))
packFunName (BuiltinType ty) =
    Left <$> builtinPackFunName ty
packFunName (TypeRef EnumRef (L _ _ name)) =
    Just $ Right (id, Text.toLower name <> "_pack")
packFunName (Pointer (TypeRef StructRef (L _ _ name))) =
    Just $ Right (id, Text.toLower name <> "_pack")
packFunName (TypeRef StructRef (L _ _ name)) =
    Just $ Right (Fix . UnaryExpr UopAddress, Text.toLower name <> "_pack")
packFunName (Pointer Const{})    = Nothing
packFunName (TypeRef UnionRef _) = Nothing  -- TODO(iphydf): Union pack.
packFunName x                    = error $ show x

-- bin_pack_bin(bp, var->mem, size)
mkPackBin :: Lexeme Text -> Lexeme Text -> Node (Lexeme Text) -> Node (Lexeme Text)
mkPackBin varName memName size =
    Fix (FunctionCall (Fix (VarExpr (mkLAt memName IdVar "bin_pack_bin")))
        [ Fix (VarExpr (mkLAt memName IdVar "bp"))
        , Fix (PointerAccess (Fix (VarExpr varName)) memName)
        , size
        ])

mkPackMember :: Lexeme Text -> (Lexeme Text, TypeInfo) -> Maybe (Node (Lexeme Text))
mkPackMember varName (memName, Sized (Pointer (BuiltinType U08Ty)) arrSize) = Just $
    mkPackBin varName memName $ Fix (PointerAccess (Fix (VarExpr varName)) arrSize)
mkPackMember varName (memName, Sized (Array (Just (BuiltinType U08Ty)) _) arrSize) = Just $
    mkPackBin varName memName $ Fix (PointerAccess (Fix (VarExpr varName)) arrSize)
mkPackMember varName (memName, Array (Just (BuiltinType U08Ty)) [NameLit arrSize]) = Just $
    mkPackBin varName memName $ Fix (LiteralExpr ConstId arrSize)
mkPackMember varName (memName, Array (Just (BuiltinType U08Ty)) [IntLit arrSize]) = Just $
    mkPackBin varName memName $ Fix (LiteralExpr Int arrSize)
mkPackMember varName (memName, memType) = do
    funName <- packFunName memType
    return $ case funName of
        Left fun ->
            Fix (FunctionCall (Fix (VarExpr (mkLAt memName IdVar fun)))
                [ Fix (VarExpr (mkLAt memName IdVar "bp"))
                , Fix (PointerAccess (Fix (VarExpr varName)) memName)
                ])
        Right (prefix, fun) ->
            Fix (FunctionCall (Fix (VarExpr (mkLAt memName IdVar fun)))
                [ prefix (Fix (PointerAccess (Fix (VarExpr varName)) memName))
                , Fix (VarExpr (mkLAt memName IdVar "bp"))
                ])


analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse = analyseStructs funSuffix mkFunBody

descr :: ([(FilePath, [Node (Lexeme Text)])] -> [Text], (Text, Text))
descr = (analyse, ("struct-pack", Text.unlines
    [ "Checks that `_pack` functions for `struct`s are complete and correct."
    , ""
    , "**Reason:** we provide `pack` functions for `struct` but don't want to"
    , "manually maintain them. This linter checks that the function is exactly what"
    , "we want it to be, and the error message will say what the function should look"
    , "like."
    ]))
