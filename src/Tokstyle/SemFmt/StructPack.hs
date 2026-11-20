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
import           Language.Cimple.Diagnostics  (CimplePos, Diagnostic)
import           Tokstyle.Common.StructLinter (MkFunBody, analyseStructs, mkLAt)
import           Tokstyle.Common.TypeSystem   (StdType (..), TypeDescr (..),
                                               TypeInfo (..), TypeRef (..),
                                               TypeSystem, lookupType)


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
mkFunBody tys varName (StructDescr _ [mem]) = do
    packMems <- mkPackMember tys varName mem
    return $ Right (Fix (CompoundStmt [Fix (Return (Just packMems))]))
mkFunBody tys varName (StructDescr sname mems) = do
    let packArray = mkPackArray sname (length mems)
    packMems <- foldr (\x y -> Fix (BinaryExpr y BopAnd x)) packArray . reverse <$> mapM (mkPackMember tys varName) mems
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

stripWrappers :: TypeInfo -> TypeInfo
stripWrappers (Owner ty)    = stripWrappers ty
stripWrappers (Nonnull ty)  = stripWrappers ty
stripWrappers (Nullable ty) = stripWrappers ty
stripWrappers (Const ty)    = stripWrappers ty
stripWrappers ty            = ty

packFunName :: TypeSystem -> TypeInfo -> Maybe (Either Text (Node (Lexeme Text) -> Node (Lexeme Text), Text))
packFunName _ (BuiltinType ty) =
    Left <$> builtinPackFunName ty
-- A typedef'd integer packs as its integer.
packFunName tys (TypeRef IntRef (L _ _ name)) = case lookupType name tys of
    Just (IntDescr _ ty) -> Left <$> builtinPackFunName ty
    _                    -> Nothing
packFunName _ (TypeRef EnumRef (L _ _ name)) =
    Just $ Right (id, Text.toLower name <> "_pack")
packFunName _ (Pointer (TypeRef StructRef (L _ _ name))) =
    Just $ Right (id, Text.toLower name <> "_pack")
packFunName _ (TypeRef StructRef (L _ _ name)) =
    Just $ Right (Fix . UnaryExpr UopAddress, Text.toLower name <> "_pack")
packFunName _ (Pointer Const{})      = Nothing
packFunName _ (Pointer _)            = Nothing
packFunName _ (TypeRef UnionRef _)   = Nothing  -- TODO(iphydf): Union pack.
packFunName tys (Owner ty)           = packFunName tys ty
packFunName tys (Nonnull ty)         = packFunName tys ty
packFunName tys (Nullable ty)        = packFunName tys ty
packFunName tys (Const ty)           = packFunName tys ty
packFunName _ x                      = error $ show x

-- bin_pack_bin(bp, var->mem, size)
mkPackBinStr :: Text -> Lexeme Text -> Lexeme Text -> Node (Lexeme Text) -> Node (Lexeme Text)
mkPackBinStr fun varName memName size =
    Fix (FunctionCall (Fix (VarExpr (mkLAt memName IdVar fun)))
        [ Fix (VarExpr (mkLAt memName IdVar "bp"))
        , Fix (PointerAccess (Fix (VarExpr varName)) memName)
        , size
        ])

getPackFn :: StdType -> Maybe (Lexeme Text -> Lexeme Text -> Node (Lexeme Text) -> Node (Lexeme Text))
getPackFn U08Ty  = Just (mkPackBinStr "bin_pack_bin")
getPackFn CharTy = Just (mkPackBinStr "bin_pack_str")
getPackFn _      = Nothing

mkPackMember :: TypeSystem -> Lexeme Text -> (Lexeme Text, TypeInfo) -> Maybe (Node (Lexeme Text))
mkPackMember tys varName (memName, ty) = case ty of
    Owner t    -> mkPackMember tys varName (memName, t)
    Nonnull t  -> mkPackMember tys varName (memName, t)
    Nullable t -> mkPackMember tys varName (memName, t)
    Const t    -> mkPackMember tys varName (memName, t)

    Sized t arrSize -> do
        packFn <- case stripWrappers t of
            Pointer (BuiltinType std)        -> getPackFn std
            Array (Just (BuiltinType std)) _ -> getPackFn std
            _                                -> Nothing
        return $ packFn varName memName $ Fix (PointerAccess (Fix (VarExpr varName)) arrSize)

    Array (Just (BuiltinType std)) [NameLit arrSize] -> do
        packFn <- getPackFn std
        return $ packFn varName memName $ Fix (LiteralExpr ConstId arrSize)

    Array (Just (BuiltinType std)) [IntLit arrSize] -> do
        packFn <- getPackFn std
        return $ packFn varName memName $ Fix (LiteralExpr Int arrSize)

    _ -> do
        funName <- packFunName tys ty
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

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse = analyseStructs funSuffix mkFunBody

descr :: ([(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("struct-pack", Text.unlines
    [ "Checks that `_pack` functions for `struct`s are complete and correct."
    , ""
    , "**Reason:** we provide `pack` functions for `struct` but don't want to"
    , "manually maintain them. This linter checks that the function is exactly what"
    , "we want it to be, and the error message will say what the function should look"
    , "like."
    ]))
