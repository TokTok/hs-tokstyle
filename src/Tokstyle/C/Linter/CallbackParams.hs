{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.CallbackParams (analyse) where

import           Data.Functor.Identity           (Identity)
import           Data.Maybe                      (mapMaybe)
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemError    (invalidAST)
import           Language.C.Analysis.SemRep      (GlobalDecls, ParamDecl (..),
                                                  Type (..), VarDecl (..))
import           Language.C.Analysis.TravMonad   (Trav, TravT, recordError,
                                                  throwTravError)
import           Language.C.Pretty               (pretty)
import           Language.C.Syntax.AST           (CExpr, CExpression (..),
                                                  annotation)
import           Tokstyle.C.Env                  (Env)
import           Tokstyle.C.Patterns
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)


paramNames :: (Int, ParamDecl, ParamDecl) -> Maybe (Int, String, String)
paramNames (i, ParamName a, ParamName b) | a /= b = Just (i, a, b)
paramNames _ = Nothing

funPtrParams :: Type -> [ParamDecl]
funPtrParams (FunPtrParams params) = params
funPtrParams _                     = []

checkParams :: (ParamDecl, CExpr, Type) -> Trav Env ()
checkParams (ParamDecl (VarDecl _ _ cbTy@(FunPtrParams params)) _, expr, ty) = do
    let cbParams = funPtrParams ty
    case mapMaybe paramNames $ zip3 [1..] params cbParams of
        [] -> return ()
        (i, a, b):_ ->
            let annot = annotation expr in
            recordError $ invalidAST annot (
                "parameter " <> show i <> " of " <> show (pretty expr) <> " is named `"
                <> b <> "`, but in callback type `" <> show (pretty cbTy) <> "` it is named `" <> a <> "`")
checkParams _ = return ()


linter :: AstActions (TravT Env Identity)
linter = astActions
    { doExpr = \node act -> case node of
        CCall fun args _ ->
            tExpr [] RValue fun >>= \case
                FunPtrParams params -> do
                    tys <- mapM (tExpr [] RValue) args
                    mapM_ checkParams (zip3 params args tys)
                    act
                x -> throwTravError $ invalidAST (annotation node) $ show x

        _ -> act
    }


analyse :: GlobalDecls -> Trav Env ()
analyse = traverseAst linter
