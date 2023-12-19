{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE ViewPatterns      #-}
module Tokstyle.C.Linter.Sizeof (analyse) where

import           Data.Functor.Identity           (Identity)
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemError    (typeMismatch)
import           Language.C.Analysis.SemRep      (GlobalDecls, Type (..))
import           Language.C.Analysis.TravMonad   (MonadTrav, Trav, TravT,
                                                  recordError)
import           Language.C.Analysis.TypeUtils   (canonicalType)
import           Language.C.Pretty               (pretty)
import           Language.C.Syntax.AST           (CExpr, CExpression (..),
                                                  annotation)
import           Tokstyle.C.Env                  (Env)
import           Tokstyle.C.Patterns
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)


-- | This catches `sizeof(buf)` where `buf` is a pointer instead of an array.
checkSizeof :: MonadTrav m => CExpr -> Type -> m ()
checkSizeof _ (canonicalType -> TY_struct _) = return ()
checkSizeof _ (canonicalType -> TY_struct_ptr "IPPTsPng") = return ()
checkSizeof _ ArrayType{} = return ()
checkSizeof e ty
  | isIntegral ty = return ()
  | otherwise =
      let annot = (annotation e, ty) in
      recordError $ typeMismatch
          ("disallowed sizeof argument of type `" <> show (pretty ty) <>
          "` - did you mean for `" <> show (pretty e) <> "` to be an array?") annot annot


linter :: AstActions (TravT Env Identity)
linter = astActions
    { doExpr = \node act -> case node of
        CSizeofExpr e _ -> do
            ty <- tExpr [] RValue e
            checkSizeof e ty
            act

        _ -> act
    }


analyse :: GlobalDecls -> Trav Env ()
analyse = traverseAst linter
