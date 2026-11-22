{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.BoolConversion (descr) where

import           Data.Functor.Identity           (Identity)
import           Data.Text                       (Text)
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemRep      (GlobalDecls, Type (..),
                                                  TypeName (..))
import           Language.C.Analysis.TravMonad   (MonadTrav, Trav, TravT)
import qualified Language.C.Pretty               as C
import           Language.C.Syntax.AST           (CBinaryOp (..), CExpr,
                                                  CExpression (..),
                                                  CUnaryOp (..), annotation)
import           Prettyprinter                   (pretty, (<+>))
import           Tokstyle.C.Env                  (Env, recordLinterError)

import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)
import           Tokstyle.C.TravUtils            (backticks)

checkBoolConversion :: CExpr -> TravT Env Identity ()
checkBoolConversion expr = do
    ty <- tExpr [] RValue expr
    case ty of
        DirectType (TyIntegral _) _ _ -> return ()
        PtrType _ _ _ ->
            recordLinterError (annotation expr) $
                "implicit conversion from" <+> backticks (pretty (show (C.pretty ty))) <+> "to bool"
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


descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("bool-conversion", "Checks for implicit conversions to bool."))
