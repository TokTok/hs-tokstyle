{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.Cast (analyse) where

import           Control.Monad                   (unless, zipWithM_)
import           Data.Functor.Identity           (Identity)
import qualified Data.Map                        as Map
import           Language.C.Analysis.AstAnalysis (ExprSide (..), defaultMD,
                                                  tExpr)
import           Language.C.Analysis.ConstEval   (constEval, intValue)
import           Language.C.Analysis.DefTable    (lookupTag)
import           Language.C.Analysis.SemError    (typeMismatch)
import           Language.C.Analysis.SemRep      (EnumType (..),
                                                  EnumTypeRef (..),
                                                  Enumerator (..), GlobalDecls,
                                                  IntType (..), TagDef (..),
                                                  Type (..), TypeName (..),
                                                  TypeQuals (..), noTypeQuals)
import           Language.C.Analysis.TravMonad   (MonadTrav, Trav, TravT,
                                                  getDefTable, recordError,
                                                  throwTravError)
import           Language.C.Analysis.TypeUtils   (canonicalType, sameType)
import           Language.C.Data.Error           (userErr)
import           Language.C.Data.Ident           (Ident (..))
import           Language.C.Pretty               (pretty)
import           Language.C.Syntax.AST           (CConstant (..), CExpr,
                                                  CExpression (..), annotation)
import           Language.C.Syntax.Constants     (CInteger (..))
import qualified Tokstyle.C.Env                  as Env
import           Tokstyle.C.Env                  (Env)
import           Tokstyle.C.Patterns
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)
import           Tokstyle.C.TravUtils            (getJust)


sameEnum :: MonadTrav m => Type -> Type -> (Ident, CExpr) -> (Ident, CExpr) -> m ()
sameEnum leftTy rightTy (leftId, leftExpr) (rightId, rightExpr) = do
    leftVal  <- getJust failMsg . intValue =<< constEval defaultMD Map.empty leftExpr
    rightVal <- getJust failMsg . intValue =<< constEval defaultMD Map.empty rightExpr
    unless (leftVal == rightVal) $
        throwTravError $ typeMismatch
            ("invalid cast: enumerator value for `"
                <> show (pretty leftId) <> " = " <> show leftVal
                <> "` does not match `"
                <> show (pretty rightId) <> " = " <> show rightVal <> "`")
            (annotation leftExpr, leftTy)
            (annotation rightExpr, rightTy)
  where
    failMsg = "invalid cast: could not determine enumerator values"

checkEnumCast :: MonadTrav m => Type -> Type -> CExpr -> m ()
checkEnumCast castTy exprTy _ = do
    castEnums <- enumerators (canonicalType castTy)
    exprEnums <- enumerators (canonicalType exprTy)
    unless (length castEnums == length exprEnums) $
        throwTravError $ userErr $
            "enum types `" <> show (pretty castTy) <> "` and `"
            <> show (pretty exprTy) <> "` have different a number of enumerators"
    zipWithM_ (sameEnum castTy exprTy) castEnums exprEnums

enumerators :: MonadTrav m => Type -> m [(Ident, CExpr)]
enumerators (DirectType (TyEnum (EnumTypeRef name _)) _ _) = do
    defs <- getDefTable
    case lookupTag name defs of
      Just (Right (EnumDef (EnumType _ enums _ _))) ->
          return $ map (\(Enumerator i e _ _) -> (i, e)) enums
      _ ->
        throwTravError $ userErr $
            "couldn't find enum type `" <> show (pretty name) <> "`"
enumerators ty =
    throwTravError $ userErr $ "invalid enum type `" <> show (pretty ty) <> "`"


unqual :: Type -> Type
unqual (PtrType ty _ a)      = PtrType (unqual ty) noTypeQuals a
unqual (DirectType tn _ a)   = DirectType tn noTypeQuals a
unqual (ArrayType ty sz _ a) = ArrayType (unqual ty) sz noTypeQuals a
unqual ty                    = ty

checkCast :: MonadTrav m => Type -> Type -> CExpr -> m ()
checkCast castTy' exprTy' e
    | isCharOrUint8T castTy' && isCharOrUint8T exprTy' = return ()
    | otherwise = check (canonicalType castTy') (canonicalType exprTy')
  where
    isCharOrUint8T ty = case ty of
        TY_char_ptr    -> True
        TY_char_arr    -> True
        TY_uint8_t_ptr -> True
        TY_uint8_t_arr -> True
        _              -> False

    check castTy exprTy | sameType castTy exprTy = return ()
    -- Casting from T* to const T* is OK. The other way around isn't, but is caught
    -- by clang and other compilers.
    check (PtrType castPointee _ _) (PtrType exprPointee _ _)
        | sameType (unqual castPointee) (unqual exprPointee) = return ()
    -- Casting from T[] to const T* is OK (array decay).
    check (PtrType castPointee _ _) (ArrayType elemTy _ _ _)
        | sameType (unqual castPointee) (unqual elemTy) = return ()
    -- Cast to void: OK.
    check (DirectType TyVoid _ _) _ = return ()
    -- Casting between `void*` and `T*`: OK
    check PtrType{} TY_void_ptr = return ()
    check TY_void_ptr PtrType{} = return ()
    -- Casting literal 0 to `T*`: OK
    check PtrType{} _ | isNullPtr e = return ()
    -- Casting sockaddr_storage to any of the sockaddr_... types: OK
    check TY_sockaddr_ptr     TY_sockaddr_storage_ptr = return ()
    check TY_sockaddr_in_ptr  TY_sockaddr_storage_ptr = return ()
    check TY_sockaddr_in6_ptr TY_sockaddr_storage_ptr = return ()
    -- Casting between numeric types: OK
    check castTy exprTy | isNumeric castTy && isNumeric exprTy = return ()
    -- Casting from enum to int: OK
    check castTy exprTy | isIntegral castTy && isEnum exprTy = return ()
    -- Casting between enums: check whether they have the same enumerators.
    check castTy exprTy | isEnum castTy && isEnum exprTy = checkEnumCast castTy exprTy e
    -- Casting to `Messenger**`: NOT OK, but toxav does this.
    -- TODO(iphydf): Fix this.
    check (PtrType (PtrType (TY_typedef "Messenger") _ _) _ _) _ = return ()
    -- Casting to `void**`: probably not ok, but toxav also does this.
    -- TODO(iphydf): Investigate.
    check (PtrType TY_void_ptr _ _) _ = return ()
    -- Casting from int to enum: actually NOT OK, but we do this a lot, so meh.
    -- TODO(iphydf): Fix these.
    check castTy exprTy | isEnum castTy && isIntegral exprTy = return ()

    -- Any other casts: NOT OK
    check _ _ =
        let annot = (annotation e, castTy') in
        recordError $ typeMismatch ("disallowed cast from " <>
            show (pretty exprTy') <> " to " <> show (pretty castTy')) annot annot

    isNullPtr (CConst (CIntConst (CInteger 0 _ _) _)) = True
    isNullPtr _                                       = False


-- | Some exemptions where weird casts like int* -> char* may happen.
exemptions :: [String]
exemptions = ["call:getsockopt", "call:setsockopt", "call:bs_list_add", "call:bs_list_remove", "call:bs_list_find", "call:random_bytes", "call:randombytes"]


linter :: AstActions (TravT Env Identity)
linter = astActions
    { doExpr = \node act -> case node of
        cast@(CCast _ e _) -> do
            castTy <- tExpr [] RValue cast
            exprTy <- tExpr [] RValue e
            ctx <- Env.getCtx
            unless (head ctx `elem` exemptions) $
                checkCast castTy exprTy e
            act

        CCall (CVar (Ident fname _ _) _) _ _ -> do
            Env.pushCtx $ "call:" <> fname
            act
            Env.popCtx

        _ -> act
    }


analyse :: GlobalDecls -> Trav Env ()
analyse = traverseAst linter
