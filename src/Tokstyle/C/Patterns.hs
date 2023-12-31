{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
{- HLINT ignore "Use camelCase" -}
module Tokstyle.C.Patterns where

import           Language.C                    (CConstant (CIntConst),
                                                CDeclaration (CDecl),
                                                CDeclarationSpecifier (CTypeSpec),
                                                CDeclarator (CDeclr),
                                                CDerivedDeclarator (CPtrDeclr),
                                                CExpression (CCast, CConst),
                                                CInteger (CInteger),
                                                CTypeSpecifier (CVoidType))
import           Language.C.Analysis.SemRep    (CompTypeRef (..), IntType (..),
                                                Type (..), TypeDefRef (..),
                                                TypeName (..))
import           Language.C.Analysis.TypeUtils (canonicalType)
import           Language.C.Data.Ident         (Ident (..), SUERef (..))

pattern TY_typedef name         <- TypeDefType (TypeDefRef (Ident name _ _) _ _) _ _
pattern TY_void_ptr             <- PtrType (DirectType TyVoid _ _) _ _
pattern TY_uint8_t_arr          <- ArrayType (TY_typedef "uint8_t") _ _ _
pattern TY_uint8_t_ptr          <- PtrType (TY_typedef "uint8_t") _ _
pattern TY_char_arr             <- ArrayType (DirectType (TyIntegral TyChar) _ _) _ _ _
pattern TY_char_ptr             <- PtrType (DirectType (TyIntegral TyChar) _ _) _ _
pattern TY_struct name          <- DirectType (TyComp (CompTypeRef (NamedRef (Ident name _ _)) _ _)) _ _
pattern TY_struct_ptr name      <- PtrType (TY_struct name) _ _
pattern TY_sockaddr_storage_ptr <- TY_struct_ptr "sockaddr_storage"
pattern TY_sockaddr_ptr         <- TY_struct_ptr "sockaddr"
pattern TY_sockaddr_in_ptr      <- TY_struct_ptr "sockaddr_in"
pattern TY_sockaddr_in6_ptr     <- TY_struct_ptr "sockaddr_in6"
pattern TY_canon_bool           <- (canonicalType -> DirectType (TyIntegral TyBool) _ _)

pattern E_0 <- CConst (CIntConst (CInteger 0 _ _) _)
pattern E_nullptr <- CCast (CDecl [CTypeSpec (CVoidType _)] [(Just (CDeclr _ [CPtrDeclr [] _] _ _ _),_,_)] _) E_0 _

isEnum :: Type -> Bool
isEnum (canonicalType -> DirectType TyEnum{} _ _) = True
isEnum _                                          = False

isIntegral :: Type -> Bool
isIntegral (canonicalType -> DirectType TyIntegral{} _ _) = True
isIntegral _                                              = False

isFloating :: Type -> Bool
isFloating (canonicalType -> DirectType TyFloating{} _ _) = True
isFloating _                                              = False

isNumeric :: Type -> Bool
isNumeric ty = isIntegral ty || isFloating ty
