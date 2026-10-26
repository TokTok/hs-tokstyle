{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict     #-}
-- | The types of expressions, and the names the program knows fields and
-- functions by.
module Tokstyle.C.Analysis.Types
    ( FieldKey
    , exprType
    , structTag
    , fieldKey
    , funType
    , funKey
    ) where

import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemRep      (CompTyKind (..),
                                                  CompTypeRef (..), FunType,
                                                  Linkage (..), Storage (..),
                                                  Type (..), TypeName (..),
                                                  declStorage, declType)
import           Language.C.Analysis.TravMonad   (MonadTrav, catchTravError,
                                                  lookupObject)
import           Language.C.Analysis.TypeUtils   (canonicalType)
import           Language.C.Data.Ident           (Ident, SUERef (..),
                                                  identToString)
import           Language.C.Syntax.AST

-- | A field of a struct: the struct's tag and the field's name.
type FieldKey = (String, String)

-- | The tag of a struct type.
structTag :: Type -> Maybe String
structTag ty = case canonicalType ty of
    DirectType (TyComp (CompTypeRef (NamedRef i) StructTag _)) _ _ -> Just (identToString i)
    _                                                              -> Nothing

-- | The field a member access reads or writes.
fieldKey :: MonadTrav m => CExpr -> m (Maybe FieldKey)
fieldKey = \case
    CMember e f isArrow _ -> exprType e >>= \ty -> return $ do
        t <- ty
        tag <- structTag $ if isArrow then pointee t else t
        return (tag, identToString f)
    _ -> return Nothing
  where
    pointee t = case canonicalType t of
        PtrType p _ _ -> p
        other         -> other

-- | Type of a function, or of what a function pointer points to.
-- 'canonicalType' turns function types into function pointers.
funType :: Type -> Maybe FunType
funType ty = case canonicalType ty of
    PtrType (FunctionType ft _) _ _ -> Just ft
    FunctionType ft _               -> Just ft
    _                               -> Nothing

-- | The name the program knows a function by, which for a static function
-- includes its translation unit.
funKey :: MonadTrav m => FilePath -> Ident -> m (Maybe String)
funKey unit i = (lookupObject i `catchTravError` const (return Nothing)) >>= \case
    Just d | Just _ <- funType (declType d) ->
        return . Just $ if isInternal (declStorage d) then unit ++ ":" ++ identToString i else identToString i
    _ -> return Nothing
  where
    isInternal = \case
        FunLinkage InternalLinkage -> True
        Static InternalLinkage _   -> True
        _                          -> False

-- | The type of an expression, if it has one.
exprType :: MonadTrav m => CExpr -> m (Maybe Type)
exprType e = (Just <$> tExpr [] RValue e) `catchTravError` const (return Nothing)
