{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.BoolConversion (analyse) where

import           Data.Functor.Identity           (Identity)
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemError    (typeMismatch)
import           Language.C.Analysis.SemRep      (GlobalDecls, IntType (..),
                                                  Type (..), TypeDefRef (..),
                                                  TypeName (..))
import           Language.C.Analysis.TravMonad   (MonadTrav, Trav, TravT,
                                                  recordError)
import           Language.C.Data.Ident           (Ident (..))
import           Language.C.Pretty               (pretty)
import           Language.C.Syntax.AST           (CBinaryOp (..), CExpr,
                                                  CExpression (..),
                                                  CUnaryOp (..), annotation)
import           Tokstyle.C.Env                  (Env)
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)

checkBoolConversion :: MonadTrav m => CExpr -> m ()
checkBoolConversion expr = do
    ty <- tExpr [] RValue expr
    case ty of
        DirectType (TyIntegral _) _ _ -> return ()
        TypeDefType (TypeDefRef (Ident "bool" _ _) _ _) _ _ -> return ()
        PtrType _ _ _ ->
            let annot = (annotation expr, ty) in
            recordError $ typeMismatch ("implicit conversion from " <> show (pretty ty) <> " to bool") annot annot
        _ -> return ()


linter :: AstActions (TravT Env Identity)
linter = astActions
    { doExpr = \node act -> case node of
        CCond c _ _ _ -> do
            checkBoolConversion c
            act
        CUnary CNegOp e _ -> do
            checkBoolConversion e
            act
        CBinary CLorOp l r _ -> do
            checkBoolConversion l
            checkBoolConversion r
            act
        CBinary CLndOp l r _ -> do
            checkBoolConversion l
            checkBoolConversion r
            act

        _ -> act
    }


analyse :: GlobalDecls -> Trav Env ()
analyse = traverseAst linter
