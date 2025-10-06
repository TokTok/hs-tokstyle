{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.Memcpy (analyse) where

import           Control.Monad                   (unless)
import           Data.Functor.Identity           (Identity)
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemError    (typeMismatch)
import           Language.C.Analysis.SemRep      (CompTypeRef (CompTypeRef),
                                                  GlobalDecls,
                                                  IntType (TyUChar), Type (..),
                                                  TypeName (TyComp, TyIntegral, TyVoid))
import           Language.C.Analysis.TravMonad   (Trav, TravT, recordError)
import           Language.C.Analysis.TypeUtils   (canonicalType)
import           Language.C.Data.Ident           (Ident (..))
import           Language.C.Pretty               (pretty)
import           Language.C.Syntax.AST           (CExpr, CExpression (..),
                                                  annotation)
import           Tokstyle.C.Env                  (Env)
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)

compatibleType :: Type -> Type -> Bool
compatibleType (PtrType    a _ _  ) (PtrType    b _ _  ) = compatibleType a b
compatibleType (ArrayType  a _ _ _) (PtrType    b _ _  ) = compatibleType a b
compatibleType (PtrType    a _ _  ) (ArrayType  b _ _ _) = compatibleType a b
compatibleType (ArrayType  a _ _ _) (ArrayType  b _ _ _) = compatibleType a b
compatibleType (DirectType a _ _  ) (DirectType b _ _  ) = compatibleTypeName a b
compatibleType _ _                                       = False

compatibleTypeName :: TypeName -> TypeName -> Bool
-- `uint8_t*` can can be memcpy'd to and from any integral type.
compatibleTypeName (TyIntegral TyUChar) TyIntegral{} = True
compatibleTypeName TyIntegral{} (TyIntegral TyUChar) = True
-- Integral types can only be memcpy'd to the same integral type.
compatibleTypeName (TyIntegral a) (TyIntegral b) = a == b
-- Structs can only be memcpy'd to the exact same struct.
compatibleTypeName (TyComp (CompTypeRef a _ _)) (TyComp (CompTypeRef b _ _)) = a == b
-- Everything else is disallowed.
compatibleTypeName _ TyComp{} = False
compatibleTypeName TyComp{} _ = False
-- Void pointers are disallowed.
compatibleTypeName TyVoid _ = False
compatibleTypeName _ TyVoid = False
-- Error here for now, to discover more cases.
compatibleTypeName a b = error (show a ++ "\n" ++ show b)

validMemType :: Type -> Bool
validMemType (PtrType   DirectType{} _ _  ) = True
validMemType (ArrayType DirectType{} _ _ _) = True
validMemType _                              = False

checkMemType :: String -> CExpr -> Type -> Trav Env ()
checkMemType fname expr ty =
    unless (validMemType (canonicalType ty)) $
        let annot = (annotation expr, ty) in
        recordError $ typeMismatch
            ("`" <> fname <> "` argument type `" <> show (pretty ty)
            <> "` is not a valid memory type (pointers to arrays are not allowed)")
            annot annot

checkCompatibility :: String -> CExpr -> CExpr -> Trav Env ()
checkCompatibility fname dst src = do
    dstTy <- tExpr [] RValue dst
    srcTy <- tExpr [] RValue src
    checkMemType fname dst dstTy
    checkMemType fname src srcTy
    unless (compatibleType (canonicalType dstTy) (canonicalType srcTy)) $
        recordError $ typeMismatch
            ("`" <> fname <> "` first argument type `" <> show (pretty dstTy)
            <> "` is not compatible with second argument type `"
            <> show (pretty srcTy) <> "`")
            (annotation dst, dstTy) (annotation src, srcTy)

linter :: AstActions (TravT Env Identity)
linter = astActions
    { doExpr = \node act -> case node of
        CCall (CVar (Ident fname _ _) _) [dst, src, _] _ | fname `elem` ["memcpy", "memcmp"] -> do
            checkCompatibility fname dst src
            act

        _ -> act
    }

analyse :: GlobalDecls -> Trav Env ()
analyse = traverseAst linter
