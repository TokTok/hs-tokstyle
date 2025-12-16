{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Strict            #-}
{-# OPTIONS_GHC -Wwarn #-}
module Tokstyle.C.Linter.VoidCall (analyse) where

import           Data.Functor.Identity           (Identity)
import           Data.List                       (isPrefixOf)
import           Data.Maybe                      (mapMaybe)
import           Language.C                      (Annotated (annotation), CCompoundBlockItem (CBlockDecl),
                                                  CDeclaration (CDecl),
                                                  CDeclarator (CDeclr),
                                                  CDerivedDeclarator (CPtrDeclr),
                                                  CExpression (CCast, CVar),
                                                  CInitializer (CInitExpr),
                                                  CStatement (CCompound), Ident,
                                                  NodeInfo, Pretty (pretty))
import           Language.C.Analysis.AstAnalysis (ExprSide (RValue), tExpr)
import           Language.C.Analysis.SemError    (invalidAST)
import           Language.C.Analysis.SemRep      (FunDef (..), FunType (..),
                                                  GlobalDecls, IdentDecl (..),
                                                  ParamDecl (..), Type (..),
                                                  VarDecl (..), VarName (..))
import           Language.C.Analysis.TravMonad   (MonadCError (recordError),
                                                  Trav, TravT)
import           Language.C.Data.Ident           (Ident (Ident))
import           Tokstyle.C.Env                  (Env (params),
                                                  bracketUserState)
import           Tokstyle.C.Patterns
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)

voidPtrParams :: [ParamDecl] -> [Ident]
voidPtrParams = mapMaybe isVoidPtr
  where
    isVoidPtr (ParamDecl (VarDecl (VarName x _) _ TY_void_ptr) _) = Just x
    isVoidPtr _                                                   = Nothing


pattern VPtrCast :: CExpression a -> Ident -> CExpression a
pattern VPtrCast var ref <- (CCast (CDecl _ [(Just (CDeclr _ [CPtrDeclr [] _] _ [] _),_,_)] _) var@(CVar ref _) _)

pattern VParamCast :: Ident -> CCompoundBlockItem a
pattern VParamCast ref <- CBlockDecl (CDecl _ [(Just (CDeclr _ [CPtrDeclr _ _] _ [] _),Just (CInitExpr (VPtrCast _ ref) _),_)] _)

linter :: AstActions (TravT Env Identity)
linter = astActions
    { doIdentDecl = \node act -> case node of
        FunctionDef (FunDef (VarDecl (VarName fname _) _ (FunctionType (FunType _ ps _) _)) (CCompound _ body _) _)
            | "sys_" `isPrefixOf` idName fname -> return ()
            | otherwise -> checkFunction (voidPtrParams ps) body

        _ -> act

    , doExpr = \node act -> case node of
        VPtrCast e n -> do
            dstTy <- tExpr [] RValue node
            srcTy <- tExpr [] RValue e
            case srcTy of
                TY_void_ptr ->
                    recordError $ invalidAST (annotation node) $
                        "first statement must cast `void *" <> idName n <> "` to `" <> show (pretty dstTy) <> "`"
                _ -> return ()

        _ -> act
    }
  where
    idName (Ident name _ _) = name

    checkFunction :: [Ident] -> [CCompoundBlockItem NodeInfo] -> TravT Env Identity ()
    checkFunction [] _                    = return ()  -- ignore functions without vptr param
    checkFunction _ []                    = return ()  -- ignore empty functions
    checkFunction vptrs (VParamCast _:ss) = checkFunction vptrs ss
    checkFunction vptrs body              = checkCastInit vptrs (traverseAst linter body)

    checkCastInit :: [Ident] -> TravT Env Identity () -> TravT Env Identity ()
    checkCastInit vptrs = bracketUserState (\env -> env{params = vptrs})


analyse :: GlobalDecls -> Trav Env ()
analyse = traverseAst linter
