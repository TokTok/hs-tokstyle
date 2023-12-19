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
                                                  TagDef (..), Type (..),
                                                  TypeName (..))
import           Language.C.Analysis.TravMonad   (MonadTrav, Trav, TravT,
                                                  getDefTable, recordError,
                                                  throwTravError)
import           Language.C.Analysis.TypeUtils   (canonicalType)
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


checkCast :: MonadTrav m => Type -> Type -> CExpr -> m ()
-- Cast to void: OK.
checkCast (DirectType TyVoid _ _) _ _ = return ()
-- Casting between `void*` and `T*`: OK
checkCast PtrType{} TY_void_ptr _ = return ()
checkCast TY_void_ptr PtrType{} _ = return ()
-- Casting between `char*` and `uint8_t*`: OK
checkCast TY_uint8_t_ptr TY_char_ptr _ = return ()
checkCast TY_uint8_t_ptr TY_char_arr _ = return ()
checkCast TY_char_ptr TY_uint8_t_ptr _ = return ()
checkCast TY_char_ptr TY_uint8_t_arr _ = return ()
-- Casting literal 0 to `T*`: OK
checkCast PtrType{} _ (CConst (CIntConst (CInteger 0 _ _) _)) = return ()
-- Casting sockaddr_storage to any of the sockaddr_... types: OK
checkCast TY_sockaddr_ptr     TY_sockaddr_storage_ptr _ = return ()
checkCast TY_sockaddr_in_ptr  TY_sockaddr_storage_ptr _ = return ()
checkCast TY_sockaddr_in6_ptr TY_sockaddr_storage_ptr _ = return ()
-- Casting between numeric types: OK
checkCast castTy exprTy _ | isNumeric castTy && isNumeric exprTy = return ()
-- Casting from enum to int: OK
checkCast castTy exprTy _ | isIntegral castTy && isEnum exprTy = return ()
-- Casting between enums: check whether they have the same enumerators.
checkCast castTy exprTy e | isEnum castTy && isEnum exprTy = checkEnumCast castTy exprTy e
-- Casting to `Messenger**`: NOT OK, but toxav does this.
-- TODO(iphydf): Fix this.
checkCast (PtrType (PtrType (TY_typedef "Messenger") _ _) _ _) _ _ = return ()
-- Casting to `void**`: probably not ok, but toxav also does this.
-- TODO(iphydf): Investigate.
checkCast (PtrType TY_void_ptr _ _) _ _ = return ()
-- Casting from int to enum: actually NOT OK, but we do this a lot, so meh.
-- TODO(iphydf): Fix these.
checkCast castTy exprTy _ | isEnum castTy && isIntegral exprTy = return ()

-- Any other casts: NOT OK
checkCast castTy exprTy e =
    let annot = (annotation e, castTy) in
    recordError $ typeMismatch ("disallowed cast from " <>
        show (pretty exprTy) <> " to " <> show (pretty castTy)) annot annot


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
