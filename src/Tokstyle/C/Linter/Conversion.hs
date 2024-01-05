{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE ViewPatterns      #-}
module Tokstyle.C.Linter.Conversion (analyse) where

import           Data.Functor.Identity           (Identity)
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemError    (typeMismatch)
import           Language.C.Analysis.SemRep      (FunDef (..), FunType (..),
                                                  GlobalDecls, IdentDecl (..),
                                                  IntType (..), Type (..),
                                                  TypeName (..), VarDecl (..),
                                                  mergeTypeQuals, noTypeQuals)
import           Language.C.Analysis.TravMonad   (MonadTrav, Trav, TravT,
                                                  recordError)
import           Language.C.Analysis.TypeUtils   (canonicalType, sameType,
                                                  typeQualsUpd)
import           Language.C.Data.Node            (NodeInfo)
import           Language.C.Pretty               (pretty)
import           Language.C.Syntax.AST           (Annotated, CAssignOp (..),
                                                  CExpr, CExpression (..),
                                                  CStatement (..), annotation)
import qualified Tokstyle.C.Env                  as Env
import           Tokstyle.C.Env                  (Env)
import           Tokstyle.C.Patterns
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)

typeEq :: Type -> Type -> Bool
typeEq a b = sameType (canon a) (canon b)
  where
    canon = removeQuals . canonicalType

removeQuals :: Type -> Type
removeQuals = typeQualsUpd (mergeTypeQuals noTypeQuals)

checkConversion :: (Annotated node, MonadTrav m) => String -> (CExpr, Type) -> (node NodeInfo, Type) -> m ()
checkConversion _ (_, TY_void_ptr) (_, PtrType{}) = return ()
checkConversion _ (_, ArrayType{}) (_, PtrType{}) = return ()
checkConversion _ (_, rTy) (_, lTy)               | typeEq lTy rTy = return ()

-- Allow int to enum conversion to cover ternary operator "?:". Only actual
-- "int" is allowed, not "int32_t" or anything typedef'd. The latter would mean
-- assignment from something that didn't undergo implicit int conversions.
checkConversion _ (_, rTy) (_, lTy) | isEnumConversion (canonicalType lTy) rTy = return ()
  where
    isEnumConversion (DirectType TyEnum{} _ _) (DirectType (TyIntegral TyInt) _ _) = True
    isEnumConversion _ _ = False

checkConversion context (r, removeQuals -> rTy) (l, removeQuals -> lTy) =
    case (show $ pretty rTy, show $ pretty lTy) of
      (rTyName, lTyName) | rTyName == lTyName -> return ()
      ("uint8_t [32]","uint8_t const [32]") -> return ()
      ("char *","const char *")         -> return ()
      ("const int *","const char *")    -> return ()
      (_,"void *")                      -> return ()
      (_,"const void *")                -> return ()

      -- int literals and integer promotions.
      ("int",_)                         -> return ()

      ("uint32_t","int64_t")            -> return ()
      ("enum RTPFlags","uint64_t")      -> return ()

      -- TODO(iphydf): Almost definitely wrong (code should be fixed).
      ("unsigned long long","uint16_t") -> return ()
      ("unsigned int","uint16_t")       -> return ()
      ("uint32_t","uint16_t")           -> return ()
      ("uint8_t","int8_t")              -> return ()

      -- TODO(iphydf): Look into these.
      (_,"uint8_t")                     -> return ()
      (_,"int32_t")                     -> return ()
      (_,"uint32_t")                    -> return ()
      (_,"size_t")                      -> return ()
      (_,"unsigned int")                -> return ()
      (_,"int")                         -> return ()
      (_,"long")                        -> return ()

      -- TODO(iphydf): Remove once the "system" PR is in.
      ("const struct Memory *","Memory const *") -> return ()
      (rTyName, lTyName) ->
          recordError $ typeMismatch
              ("invalid conversion from `" <> rTyName <> "` to `" <>
                  lTyName <> "` in " <> context)
              (annotation l, lTy)
              (annotation r, rTy)

checkAssign :: MonadTrav m => String -> (CExpr, Type) -> (CExpr, Type) -> m ()
checkAssign _ _ (CConst{}, _) = return ()
checkAssign _ _ (CCast{}, _)  = return ()
checkAssign c l r             = checkConversion c r l


linter :: AstActions (TravT Env Identity)
linter = astActions
    { doExpr = \node act -> case node of
        CAssign CAssignOp l r _ -> do
            lTy <- tExpr [] LValue l
            rTy <- tExpr [] RValue r
            checkAssign "assignment" (l, lTy) (r, rTy)
        _ -> act

    , doStat = \node act -> case node of
        CReturn (Just expr) _ -> do
            retTy <- Env.getRetTy
            exprTy <- tExpr [] RValue expr
            checkConversion "return" (expr, exprTy) (expr, retTy)
            act
        _ -> act

    , doIdentDecl = \node act -> case node of
        FunctionDef (FunDef (VarDecl _ _ (FunctionType (FunType ty _ _) _)) _ _) -> do
            Env.setRetTy ty
            act
            Env.unsetRetTy
        _ -> act
    }


analyse :: GlobalDecls -> Trav Env ()
analyse = traverseAst linter
