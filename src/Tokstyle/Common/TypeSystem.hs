{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Common.TypeSystem where

import           Control.Arrow              (second)
import           Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import           Data.Fix                   (foldFixM)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Language.Cimple            (Lexeme (..), LiteralType (..),
                                             Node, NodeF (..), lexemeText)


data StdType
    = VoidTy
    | BoolTy
    | CharTy
    | U08Ty
    | S08Ty
    | U16Ty
    | S16Ty
    | U32Ty
    | S32Ty
    | U64Ty
    | S64Ty
    | SizeTy
    | F32Ty
    | F64Ty
    deriving (Show)


data TypeRef
    = UnresolvedRef
    | StructRef
    | UnionRef
    | EnumRef
    | IntRef
    | FuncRef
    deriving (Show)


data TypeInfo
    = TypeRef TypeRef (Lexeme Text)

    | Pointer TypeInfo
    | Sized TypeInfo (Lexeme Text)
    | Const TypeInfo
    | BuiltinType StdType
    | ExternalType (Lexeme Text)
    | Array (Maybe TypeInfo) [TypeInfo]

    | Var (Lexeme Text) TypeInfo
    | IntLit (Lexeme Text)
    | NameLit (Lexeme Text)
    | EnumMem (Lexeme Text)
    deriving (Show)


data TypeDescr
    = StructDescr (Lexeme Text) [(Lexeme Text, TypeInfo)]
    | UnionDescr (Lexeme Text) [(Lexeme Text, TypeInfo)]
    | EnumDescr (Lexeme Text) [TypeInfo]
    | IntDescr (Lexeme Text) StdType
    deriving (Show)


type TypeSystem = Map Text TypeDescr


lookupType :: Text -> TypeSystem -> Maybe TypeDescr
lookupType = Map.lookup . Text.toLower

insert :: Lexeme Text -> TypeDescr -> State TypeSystem [TypeInfo]
insert name ty = do
    State.modify $ Map.insert (Text.toLower $ lexemeText name) ty
    return []

foldArray :: Lexeme Text -> [[TypeInfo]] -> TypeInfo -> TypeInfo
foldArray name arrs baseTy = Var name (merge baseTy (concat arrs))
  where
    merge ty (Array Nothing dims:xs) = merge (Array (Just ty) dims) xs
    merge ty []                      = ty
    merge ty xs                      = error (show (ty, xs))


vars :: [[TypeInfo]] -> [(Lexeme Text, TypeInfo)]
vars = joinSizer . map go . concat
  where
    go (Var name ty) = (name, ty)
    go x             = error $ show x

    joinSizer (d@(dn@(L _ _ dname), dty@Array{}):s@(sn@(L _ _ sname), BuiltinType U32Ty):xs)
        | sname `elem` [dname <> "_length", dname <> "_size"] =
            (dn, Sized dty sn) : joinSizer xs
        | otherwise = (d:s:joinSizer xs)
    joinSizer (d@(dn@(L _ _ dname), dty@Pointer{}):s@(sn@(L _ _ sname), BuiltinType U32Ty):xs)
        | sname `elem` [dname <> "_length", dname <> "_size"] =
            (dn, Sized dty sn) : joinSizer xs
        | otherwise = (d:s:joinSizer xs)
    joinSizer (x:xs) = x:joinSizer xs
    joinSizer []     = []


builtin :: Lexeme Text -> TypeInfo
builtin (L _ _              "char")  = BuiltinType CharTy
builtin (L _ _           "uint8_t")  = BuiltinType U08Ty
builtin (L _ _            "int8_t")  = BuiltinType S08Ty
builtin (L _ _          "uint16_t")  = BuiltinType U16Ty
builtin (L _ _           "int16_t")  = BuiltinType S16Ty
builtin (L _ _          "uint32_t")  = BuiltinType U32Ty
builtin (L _ _           "int32_t")  = BuiltinType S32Ty
builtin (L _ _          "uint64_t")  = BuiltinType U64Ty
builtin (L _ _           "int64_t")  = BuiltinType S64Ty
builtin (L _ _            "size_t")  = BuiltinType SizeTy
builtin (L _ _              "void")  = BuiltinType VoidTy
builtin (L _ _              "bool")  = BuiltinType BoolTy
builtin (L _ _             "float")  = BuiltinType F32Ty
builtin (L _ _            "double")  = BuiltinType F64Ty

builtin (L _ _               "int")  = BuiltinType S32Ty
builtin (L _ _      "unsigned int")  = BuiltinType U32Ty
builtin (L _ _          "unsigned")  = BuiltinType U32Ty
builtin (L _ _   "long signed int")  = BuiltinType S64Ty
builtin (L _ _ "long unsigned int")  = BuiltinType U64Ty

builtin n@(L _ _ "OpusEncoder")      = ExternalType n
builtin n@(L _ _ "OpusDecoder")      = ExternalType n
builtin n@(L _ _ "cmp_ctx_t")        = ExternalType n
builtin n@(L _ _ "pthread_mutex_t")  = ExternalType n
builtin n@(L _ _ "pthread_rwlock_t") = ExternalType n
builtin n@(L _ _ "vpx_codec_ctx_t")  = ExternalType n

builtin name                         = TypeRef UnresolvedRef name


collectTypes :: NodeF (Lexeme Text) [TypeInfo] -> State TypeSystem [TypeInfo]
collectTypes node = case node of
    LiteralExpr ConstId name     -> return [NameLit name]
    LiteralExpr Int lit          -> return [IntLit lit]

    DeclSpecArray Nothing        -> return []
    DeclSpecArray (Just arr)     -> return [Array Nothing arr]
    CallbackDecl ty name         -> return [Var name (TypeRef FuncRef ty)]
    VarDecl ty name []           -> return $ map (Var name) ty
    VarDecl ty name arrs         -> return $ map (foldArray name arrs) ty
    MemberDecl l _               -> return l
    Struct dcl mems              -> aggregate StructDescr dcl mems
    Union  dcl mems              -> aggregate UnionDescr  dcl mems

    Enumerator name _            -> return [EnumMem name]
    EnumConsts (Just dcl) mems   -> enum dcl mems
    EnumDecl dcl mems _          -> enum dcl mems
    Typedef [BuiltinType ty] dcl -> int dcl ty

    TyUserDefined name           -> return [TypeRef UnresolvedRef name]
    TyStruct name                -> return [TypeRef StructRef name]
    TyFunc name                  -> return [TypeRef FuncRef name]
    TyPointer ns                 -> return $ map Pointer ns
    TyConst ns                   -> return $ map Const ns

    TyStd name                   -> return [builtin name]

    -- Throw away any type information in top-level decls that aren't
    -- defining types. The ones defining types put them into the TypeSystem
    -- map in the insertion functions below.
    ConstDecl{}                  -> return []
    ConstDefn{}                  -> return []
    StaticAssert{}               -> return []
    FunctionDecl{}               -> return []
    FunctionDefn{}               -> return []
    PreprocDefineMacro{}         -> return []
    EnumConsts Nothing _         -> return []

    -- The rest just collects all the types it sees.
    n                            -> return $ concat n

  where
    aggregate cons dcl mems = insert dcl (cons dcl (vars mems))
    enum dcl mems = insert dcl (EnumDescr dcl (concat mems))
    int dcl ty = insert dcl (IntDescr dcl ty)


collect :: [(FilePath, [Node (Lexeme Text)])] -> TypeSystem
collect = resolve . flip State.execState Map.empty . mapM_ (mapM_ (foldFixM collectTypes) . snd)


resolve :: TypeSystem -> TypeSystem
resolve tys = Map.map go tys
  where
    go (StructDescr dcl mems) = StructDescr dcl (map (second resolveRef) mems)
    go (UnionDescr  dcl mems) = UnionDescr  dcl (map (second resolveRef) mems)
    go ty@EnumDescr{}         = ty
    go ty@IntDescr{}          = ty

    resolveRef ty@(TypeRef UnresolvedRef l@(L _ _ name)) =
        case lookupType name tys of
            Nothing            -> ty
            Just StructDescr{} -> TypeRef StructRef l
            Just UnionDescr{}  -> TypeRef UnionRef l
            Just EnumDescr{}   -> TypeRef EnumRef l
            Just IntDescr{}    -> TypeRef IntRef l
    resolveRef (Const   ty)           = Const   (resolveRef ty)
    resolveRef (Pointer ty)           = Pointer (resolveRef ty)
    resolveRef (Sized ty size)        = Sized (resolveRef ty) size
    resolveRef (Array (Just ty) dims) = Array (Just $ resolveRef ty) dims
    resolveRef ty = ty
