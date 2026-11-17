{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.Truncation (descr, looksAt) where

import           Control.Monad                   (forM_, when, zipWithM_)
import qualified Data.Map.Strict                 as Map
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemRep      (FunDef, FunType (..),
                                                  GlobalDecls, Linkage (..),
                                                  Storage (..), Type (..),
                                                  TypeName (..), VarName (..),
                                                  declName, declStorage,
                                                  declType)
import           Language.C.Analysis.TravMonad   (Trav, catchTravError,
                                                  getUserState)
import           Language.C.Analysis.TypeUtils   (canonicalType)
import           Language.C.Data.Ident           (identToString)
import           Language.C.Syntax.AST           (CAssignOp (..), annotation)
import           Prettyprinter                   (Doc, pretty, (<+>))
import           Prettyprinter.Render.Terminal   (AnsiStyle)
import           Tokstyle.Analysis.AccessPath    (AccessPath (..))
import           Tokstyle.C.Analysis.Interval    (Facts, IntTy (..), Range (..),
                                                  Term (..), TermKind (..),
                                                  eval, intType, subTermsWith)
import           Tokstyle.C.Analysis.Types       (funType)
import           Tokstyle.C.Env                  (Env (..), recordLinterError)
import           Tokstyle.C.IntervalLinter       (analysedFunctions, exprDoc)
import           Tokstyle.Common                 (isClientHeader)


-- | Reports one of the given parameters converted to a narrower integer type
-- without a cast, if the analysis finds it can be a value the type can't hold.
checkFits :: Set String -> Facts -> Doc AnsiStyle -> IntTy -> Term -> Trav Env ()
checkFits anyValue facts what target t = case (termKind t, termTy t, eval facts t) of
    (TLoad (PathVar p), Just src, Just rs) | p `Set.member` anyValue, tyBits target < tyBits src ->
        case [ r | r <- rs, lo r < lo full || hi r > hi full ] of
            r : _ -> recordLinterError (annotation (termExpr t)) $
                exprDoc (termExpr t) <+> "can be" <+> outside r
                <> ", which doesn't fit in the" <+> pretty (tyBits target) <> "-bit" <+> what
            [] -> return ()
    _ -> return ()
  where
    full = tyRange target
    outside r
        | hi r > hi full = pretty (hi r)
        | otherwise = pretty (lo r)

-- | The integer types of the parameters of the function a call calls.
paramTypes :: Term -> Trav Env [Maybe IntTy]
paramTypes f = (types <$> tExpr [] RValue (termExpr f)) `catchTravError` const (return [])
  where
    types ty = case funType ty of
        Just (FunType _ ps _) -> map (intType . declType) ps
        _                     -> []

checkTerm :: Set String -> Facts -> Term -> Trav Env ()
checkTerm anyValue facts t = case termKind t of
    TAssign _ CAssignOp l r | Just target <- termTy l ->
        checkFits anyValue facts ("variable" <+> exprDoc (termExpr l) <+> "it is stored in") target r
    TCall f args -> do
        tys <- paramTypes f
        zipWithM_ (\i (ty, a) -> forM_ ty $ \target ->
                checkFits anyValue facts ("parameter" <+> pretty (i :: Int) <+> "of" <+> exprDoc (termExpr f)) target a)
            [1 ..] (zip tys args)
    _ -> return ()

-- | The integer parameters of a function. An enum isn't one: it only holds
-- its constants, which the analysis doesn't know.
intParams :: FunDef -> Set String
intParams fd = Set.fromList
    [ identToString i
    | Just (FunType _ ps _) <- [funType (declType fd)]
    , p <- ps
    , VarName i _ <- [declName p]
    , not (isEnum (declType p))
    ]
  where
    isEnum ty = case canonicalType ty of
        DirectType TyEnum{} _ _ -> True
        _                       -> False

-- | The functions the check looks at: those other translation units can call,
-- which the public ones are among.
looksAt :: FunDef -> Bool
looksAt fd = case declStorage fd of
    FunLinkage ExternalLinkage -> True
    _                          -> False

analyse :: GlobalDecls -> Trav Env ()
analyse _ = do
    declared <- declaredIn <$> getUserState
    functions <- analysedFunctions looksAt
    forM_ functions $ \(fd, nodes) ->
        -- A client can pass any value to a function it can call.
        when (maybe False isClientHeader (Map.lookup (name fd) declared)) $
            forM_ nodes $ \(_, facts, terms) ->
                mapM_ (uncurry (checkTerm (intParams fd))) (concatMap (subTermsWith facts) terms)
  where
    name fd = case declName fd of
        VarName i _ -> identToString i
        NoName      -> ""

descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("truncation", Text.unlines
    [ "Checks that an integer parameter of a function a client can call, which can"
    , "be any value, is only stored in a narrower variable or passed as a narrower"
    , "parameter without a cast if the conditions that guard it leave values that"
    , "fit."
    , ""
    , "**Reason:** a `size_t` length passed as a `uint16_t` drops its top bits, so a"
    , "length check after that passes for a length of 65539 as if it were 3."
    ]))
