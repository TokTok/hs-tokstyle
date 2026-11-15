{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.Conversion (descr) where

import           Control.Monad                    (unless)
import           Data.Functor.Identity            (Identity)
import           Data.List                        (isSuffixOf)
import           Data.Text                        (Text)
import           Language.C.Analysis.AstAnalysis  (ExprSide (..), tExpr)
import           Language.C.Analysis.DeclAnalysis (analyseTypeDecl)
import           Language.C.Analysis.SemRep       (FunDef (..), FunType (..),
                                                   GlobalDecls, IdentDecl (..),
                                                   IntType (..), Type (..),
                                                   TypeName (..), VarDecl (..),
                                                   mergeTypeQuals, noTypeQuals)
import           Language.C.Analysis.TravMonad    (MonadTrav, Trav, TravT)
import           Language.C.Analysis.TypeUtils    (canonicalType, sameType,
                                                   typeQualsUpd)
import           Language.C.Data.Position         (posFile, posOf)
import qualified Language.C.Pretty                as C
import           Language.C.Syntax.AST            (CAssignOp (..),
                                                   CDeclaration (..),
                                                   CDeclarator (..), CExpr,
                                                   CExpression (..),
                                                   CInitializer (..),
                                                   CStatement (..), annotation)
import           Prettyprinter                    (pretty, (<+>))
import qualified Tokstyle.C.Env                   as Env
import           Tokstyle.C.Env                   (Env, recordLinterError)
import           Tokstyle.C.Patterns
import           Tokstyle.C.TraverseAst           (AstActions (..), astActions,
                                                   traverseAst)
import           Tokstyle.C.TravUtils             (backticks)

typeEq :: Type -> Type -> Bool
typeEq a b = sameType (canon a) (canon b)
  where
    canon = removeQuals . canonicalType

removeQuals :: Type -> Type
removeQuals = typeQualsUpd (mergeTypeQuals noTypeQuals)

checkConversion :: String -> (CExpr, Type) -> Type -> TravT Env Identity ()
-- Ignore cmp.c, it does a lot of implicit conversions.
-- TODO(iphydf): Maybe it shouldn't? UBSAN also warns about it.
checkConversion _ (r, _) _ | "cmp/cmp.c" `isSuffixOf` posFile (posOf (annotation r)) = return ()

checkConversion context (r, rTy') lTy' =
    unless isAllowed $
        recordLinterError (annotation r) $
            "invalid conversion from" <+> backticks (pretty rTyName) <+> "to" <+>
                backticks (pretty lTyName) <+> "in" <+> pretty context
  where
    rTy = removeQuals rTy'
    lTy = removeQuals lTy'
    rCanon = canonicalType rTy
    lCanon = canonicalType lTy
    rTyName = show $ C.pretty rTy
    lTyName = show $ C.pretty lTy

    isAllowed =
        isNullPtr rTy'
        || typeEq lTy rTy
        || sameType rCanon lCanon
        || show (C.pretty rCanon) == show (C.pretty lCanon)
        || case (rCanon, lCanon) of
            (PtrType rPtd _ _, PtrType lPtd _ _) ->
                typeEq lPtd rPtd || isVoidPtr lCanon
            (ArrayType rPtd _ _ _, PtrType lPtd _ _) ->
                typeEq lPtd rPtd || isVoidPtr lCanon
            _ -> isEnumConversion lCanon rTy' || special

    isNullPtr TY_nullptr = True
    isNullPtr _          = False

    isEnumConversion (DirectType TyEnum{} _ _) (DirectType (TyIntegral TyInt) _ _) = True
    isEnumConversion _ _ = False

    isVoidPtr TY_void_ptr = True
    isVoidPtr _           = False

    special = case (rTyName, lTyName) of
      ("uint8_t [32]","uint8_t const [32]") -> True
      ("const int *","const char *")        -> True
      ("char *","const char *")             -> True

      -- int literals and integer promotions.
      ("int",_) | not (isPtr lTy)           -> True

      ("uint32_t","int64_t")                -> True
      ("enum RTPFlags","uint64_t")          -> True

      -- TODO(iphydf): Almost definitely wrong (code should be fixed).
      ("unsigned long long","uint16_t")     -> True
      ("unsigned int","uint16_t")           -> True
      ("uint32_t","uint16_t")               -> True
      ("uint8_t","int8_t")                  -> True

      -- TODO(iphydf): Look into these.
      (_,"uint8_t")                         -> True
      (_,"int32_t")                         -> True
      (_,"uint32_t")                        -> True
      (_,"size_t")                          -> True
      (_,"unsigned int")                    -> True
      (_,"int")                             -> True
      (_,"long")                            -> True
      _                                     -> False

    isPtr PtrType{}   = True
    isPtr ArrayType{} = True
    isPtr _           = False

checkAssign :: String -> Type -> (CExpr, Type) -> TravT Env Identity ()
checkAssign _ _ (CConst{}, _) = return ()
checkAssign _ _ (CCast{}, _)  = return ()
checkAssign c lTy r           = checkConversion c r lTy


linter :: AstActions (TravT Env Identity)
linter = astActions
    { doExpr = \node act -> case node of
        CAssign CAssignOp l r _ -> do
            lTy <- tExpr [] LValue l
            rTy <- tExpr [] RValue r
            checkAssign "assignment" lTy (r, rTy)
        _ -> act

    , doDecl = \node act -> case node of
        CDecl specs ds ni -> do
            sequence_
                [ do
                    -- The type of the declaration without its name and initialiser.
                    let abstract = CDeclr Nothing derived asm attrs dni
                    lTy <- analyseTypeDecl (CDecl specs [(Just abstract, Nothing, Nothing)] ni)
                    rTy <- tExpr [] RValue r
                    checkAssign "initialiser" lTy (r, rTy)
                | (Just (CDeclr _ derived asm attrs dni), Just (CInitExpr r _), _)
                    <- ds
                ]
            act
        _ -> act

    , doStat = \node act -> case node of
        CReturn (Just expr) _ -> do
            retTy <- Env.getRetTy
            exprTy <- tExpr [] RValue expr
            checkConversion "return" (expr, exprTy) retTy
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


descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("conversion", "Checks for disallowed implicit conversions."))
