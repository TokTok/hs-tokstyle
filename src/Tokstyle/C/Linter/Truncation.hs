{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.Truncation (descr, looksAt) where

import           Control.Monad                   (forM_, when)
import           Data.Map.Strict                 (Map)
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
import           Language.C.Syntax.AST           (CAssignOp (..),
                                                  CBinaryOp (..), annotation)
import           Prettyprinter                   (Doc, pretty, (<+>))
import           Prettyprinter.Render.Terminal   (AnsiStyle)
import           Tokstyle.Analysis.AccessPath    (AccessPath (..))
import           Tokstyle.C.Analysis.Interval    (Fact (..), Facts, IntTy (..),
                                                  Program (..), Range (..),
                                                  Term (..), TermKind (..),
                                                  eval, intType, subTerms,
                                                  subTermsWith)
import           Tokstyle.C.Analysis.Types       (funType)
import           Tokstyle.C.Env                  (Env (..), recordLinterError)
import           Tokstyle.C.IntervalLinter       (analysedFunctions, exprDoc)
import           Tokstyle.Common                 (isClientHeader)


-- | What the check knows about the function a term is in.
data Context = Context
    { anyValue :: Set String
    -- ^ The parameters a client passes, which can be any value.
    , compared :: Set AccessPath
    , selects  :: Map String (Set Int)
    -- ^ The parameters each function compares and returns.
    }

-- | Reports a value converted to a narrower integer type without a cast, if
-- the analysis finds it can be one the type can't hold.
checkFits :: (Range -> Range -> Bool) -> Facts -> Doc AnsiStyle -> IntTy -> Term -> Trav Env ()
checkFits outside facts what target t = case (termTy t, eval facts t) of
    (Just src, Just rs) | tyBits target < tyBits src, r : _ <- filter (outside full) rs ->
        recordLinterError (annotation (termExpr t)) $
            exprDoc (termExpr t) <+> "can be" <+> pretty (if hi r > hi full then hi r else lo r)
            <> ", which doesn't fit in the" <+> pretty (tyBits target) <> "-bit" <+> what
    _ -> return ()
  where
    full = tyRange target

cut :: Range -> Range -> Bool
cut full r = lo r < lo full || hi r > hi full

-- | A sum or a product below the type's range has wrapped around, so only the
-- top counts.
tooLarge :: Range -> Range -> Bool
tooLarge full r = hi r > hi full

-- | Whether a term is a sum or a product, which outgrows its operands.
grows :: Term -> Bool
grows t = case termKind t of
    TBin op _ _ -> op `elem` [CAddOp, CMulOp]
    TCond _ m f -> maybe False grows m || grows f
    _           -> False

-- | The integer types of the parameters of the function a call calls.
paramTypes :: Term -> Trav Env [Maybe IntTy]
paramTypes f = (types <$> tExpr [] RValue (termExpr f)) `catchTravError` const (return [])
  where
    types ty = case funType ty of
        Just (FunType _ ps _) -> map (intType . declType) ps
        _                     -> []

checkTerm :: Context -> Facts -> Term -> Trav Env ()
checkTerm cx facts t = case termKind t of
    TAssign path CAssignOp l r | Just target <- termTy l -> do
        let stored = "variable" <+> exprDoc (termExpr l) <+> "it is stored in"
        when (fromClient r) $ checkFits cut facts stored target r
        when (grows r && maybe False (`Set.member` compared cx) path) $
            checkFits tooLarge facts (stored <+> "before it is compared") target r
    TCall f args -> do
        tys <- paramTypes f
        let selected = case termFact t of
                Just (CallFact k) -> Map.findWithDefault Set.empty k (selects cx)
                _                 -> Set.empty
        forM_ (zip3 [0 ..] tys args) $ \(i, ty, a) -> forM_ ty $ \target -> do
            let param = "parameter" <+> pretty (i + 1) <+> "of" <+> exprDoc (termExpr f)
            when (fromClient a) $ checkFits cut facts param target a
            when (grows a && i `Set.member` selected) $
                checkFits tooLarge facts (param <> ", which compares it") target a
    _ -> return ()
  where
    fromClient x = case termKind x of
        TLoad (PathVar p) -> p `Set.member` anyValue cx
        _                 -> False

-- | The variables a function compares, itself or by passing them to a function
-- that compares a parameter and returns it.
comparedIn :: Map String (Set Int) -> [Term] -> Set AccessPath
comparedIn selecting = Set.fromList . concatMap uses
  where
    uses t = case termKind t of
        TBin op a b | op `elem` [CLeOp, CGrOp, CLeqOp, CGeqOp] -> load a ++ load b
        TCall _ args | Just (CallFact k) <- termFact t, Just ns <- Map.lookup k selecting ->
            concat [ load a | (n, a) <- zip [0 ..] args, n `Set.member` ns ]
        _ -> []
    load x = case termKind x of
        TLoad p -> [p]
        TCast a -> load a
        _       -> []

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

looksAt :: FunDef -> Bool
looksAt = const True

analyse :: GlobalDecls -> Trav Env ()
analyse _ = do
    env <- getUserState
    functions <- analysedFunctions looksAt
    forM_ functions $ \(fd, nodes) -> do
        let selecting = progSelects (program env)
            cx = Context
                { anyValue = if client (declaredIn env) fd then intParams fd else Set.empty
                , compared = comparedIn selecting (concatMap subTerms (concat [ ts | (_, _, ts) <- nodes ]))
                , selects = selecting
                }
        forM_ nodes $ \(_, facts, terms) ->
            mapM_ (uncurry (checkTerm cx)) (concatMap (subTermsWith facts) terms)
  where
    -- A client can pass any value to a function it can call: one other
    -- translation units can call, with a prototype in a client header.
    client declared fd = external fd && maybe False isClientHeader (Map.lookup (name fd) declared)
    external fd = case declStorage fd of
        FunLinkage ExternalLinkage -> True
        _                          -> False
    name fd = case declName fd of
        VarName i _ -> identToString i
        NoName      -> ""

descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("truncation", Text.unlines
    [ "Checks that an integer parameter of a function a client can call, which can"
    , "be any value, is only stored in a narrower variable or passed as a narrower"
    , "parameter without a cast if the conditions that guard it leave values that"
    , "fit. Also checks that a sum or a product that doesn't fit in a narrower"
    , "variable isn't stored in it without a cast and compared after, by the function"
    , "itself or by one like `min` that compares a parameter and returns it."
    , ""
    , "**Reason:** a `size_t` length passed as a `uint16_t` drops its top bits, so a"
    , "length check after that passes for a length of 65539 as if it were 3. E.g."
    , "65550 seconds stored in a `uint16_t` and then limited to an hour are 14"
    , "seconds."
    ]))
