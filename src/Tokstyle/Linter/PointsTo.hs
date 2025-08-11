{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.PointsTo (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe, mapMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node,
                                              NodeF (FunctionPrototype, MemberDecl, TyFunc, TyPointer, VarDecl))
import qualified Language.Cimple.Diagnostics as Diagnostics
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Text.Groom                  (groom)

import           Tokstyle.Analysis.CallGraph (buildCallGraph)
import           Tokstyle.Analysis.DataFlow  (cfgOutFacts)
import           Tokstyle.Analysis.PointsTo  (PointsToContext (..),
                                              PointsToState (..),
                                              buildPointsToContext)
import           Tokstyle.Analysis.Types     (AbstractLocation (..),
                                              PointsToMap)


findPointerDecls :: [(FilePath, [Node (Lexeme Text)])] -> Map.Map Text (FilePath, Lexeme Text)
findPointerDecls tus = State.execState (traverseAst collector tus) Map.empty
  where
    collector :: AstActions (State (Map.Map Text (FilePath, Lexeme Text))) Text
    collector = astActions
        { doNode = \file node act -> do
            case unFix node of
                VarDecl (Fix (TyPointer _)) l@(L _ _ name) _ -> State.modify (Map.insert name (file, l))
                MemberDecl (Fix (VarDecl (Fix (TyPointer _)) l@(L _ _ name) _)) _ -> State.modify (Map.insert name (file, l))
                _                                             -> return ()
            act
        }

isFunctionParameter :: Text -> [(FilePath, [Node (Lexeme Text)])] -> Bool
isFunctionParameter name tus = State.execState (traverseAst collector tus) False
  where
    collector :: AstActions (State Bool) Text
    collector = astActions
        { doNode = \_ node act -> do
            case unFix node of
                FunctionPrototype _ _ params ->
                    let paramNames = Set.fromList $ mapMaybe (\case
                            Fix (VarDecl _ (L _ _ pname) _) -> Just pname
                            _                               -> Nothing
                            ) params
                    in if Set.member name paramNames
                       then State.put True
                       else act
                _ -> act
        }

prettyAbstractLocation :: AbstractLocation -> Text
prettyAbstractLocation (VarLocation name) = name
prettyAbstractLocation (FieldLocation base field) = prettyAbstractLocation base <> "." <> field
prettyAbstractLocation (DerefLocation ptr) = "(*" <> prettyAbstractLocation ptr <> ")"
prettyAbstractLocation (GlobalVarLocation name) = name
prettyAbstractLocation (ReturnLocation name) = "ret:" <> name
prettyAbstractLocation (HeapLocation _) = "heap"
prettyAbstractLocation (FunctionLocation name) = "&" <> name

formatPointsToSet :: Set AbstractLocation -> Text
formatPointsToSet set =
    let locations = Set.toList $ Set.map (\case
            FunctionLocation name -> name
            VarLocation name      -> name
            _                     -> ""
            ) set
    in "{" <> Text.intercalate ", " (filter (not . Text.null) locations) <> "}"

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse tus =
    let initialCallGraph = buildCallGraph tus Map.empty
        pointsToCtx = buildPointsToContext tus initialCallGraph Map.empty
        allOutFacts = map ptsMap $ concatMap (map cfgOutFacts . Map.elems) (Map.elems (ptcAnalyzedCfgs pointsToCtx))
        finalPointsToMap = Map.unionsWith Set.union allOutFacts
        ptrDecls = findPointerDecls tus

        warnings = mapMaybe (\(loc, pointsToSet) ->
            let mname = case loc of
                    VarLocation n -> Just n
                    FieldLocation base n -> case base of
                        DerefLocation _ -> Nothing
                        _               -> Just n
                    ReturnLocation n  -> Just n
                    _                 -> Nothing
            in case mname of
                Just name ->
                   if not (Set.null pointsToSet) && not (isFunctionParameter name tus)
                   then let (file, lexeme) = fromMaybe (error $ "impossible: " <> show name) (Map.lookup name ptrDecls)
                        in Just $ Diagnostics.sloc file lexeme <> ": `" <> prettyAbstractLocation loc <> "` points to: " <> formatPointsToSet pointsToSet
                   else Nothing
                Nothing -> Nothing
            ) (Map.toList finalPointsToMap)

    in if null warnings
       then []
       else warnings

descr :: ([(FilePath, [Node (Lexeme Text)])] -> [Text], (Text, Text))
descr = (analyse, ("points-to", Text.unlines
    [ "Reports the set of functions that each function pointer can point to."
    , ""
    , "**Reason:** To statically verify the possible targets of indirect calls."
    ]))
