{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tokstyle.Analysis.CallGraph
    ( buildCallGraph
    , getCallees
    ) where

import           Control.Monad               (forM_)
import           Control.Monad.State.Strict  (State, execState, get, modify)
import           Data.Fix                    (Fix (..))
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Language.Cimple             as C
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Tokstyle.Analysis.Types     (AbstractLocation (..), CallGraph,
                                              CallSite (..), CallType (..),
                                              CalleeMap, FunctionName, NodeId,
                                              PointsToMap, toAbstractLocation)

-- | The state used internally by the builder.
data BuilderState = BuilderState
    { bsGraph           :: CallGraph
    , bsCurrentFunction :: Maybe FunctionName
    , bsFunctionNames   :: Set FunctionName
    , bsPointsToMap     :: PointsToMap
    }

-- | Defines the actions to perform while traversing the AST to build the call graph.
callGraphActions :: AstActions (State BuilderState) Text
callGraphActions = astActions
    { doNode = \_file node act -> case unFix node of
        -- Entering a function definition: update current function name in state
        C.FunctionDefn _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)) _ -> do
            modify $ \st -> st { bsCurrentFunction = Just name }
            act -- Continue traversal into the function body
            modify $ \st -> st { bsCurrentFunction = Nothing } -- Reset after leaving the function

        -- Found a function call
        C.FunctionCall calleeExpr _ -> do
            st <- get
            let nodeId = C.getNodeId node
            case bsCurrentFunction st of
                Just callerName -> do
                    case unFix calleeExpr of
                        C.VarExpr (C.L _ _ name) | Set.member name (bsFunctionNames st) ->
                            -- This is a direct call to a known function.
                            addCall callerName name (CallSite nodeId DirectCall)
                        _ -> do
                            -- This is an indirect call or a call to a function pointer.
                            let calleeLoc = toAbstractLocation calleeExpr
                            let resolvedCallees = fromMaybe Set.empty $ Map.lookup calleeLoc (bsPointsToMap st)
                            if Set.null resolvedCallees
                            then
                                -- If we can't resolve it, we'll just record it as an indirect
                                -- call to a name if we can find one.
                                case unFix calleeExpr of
                                    C.VarExpr (C.L _ _ name) -> addCall callerName name (CallSite nodeId IndirectCall)
                                    _ -> addCall callerName "<indirect>" (CallSite nodeId IndirectCall)
                            else
                                -- We resolved the pointer, so add indirect calls to all
                                -- possible targets.
                                forM_ (Set.toList resolvedCallees) $ \case
                                    FunctionLocation callee -> addCall callerName callee (CallSite nodeId IndirectCall)
                                    VarLocation      callee -> addCall callerName callee (CallSite nodeId IndirectCall)
                                    _ -> return ()
                Nothing -> return ()
            act -- Continue traversal of arguments

        -- For all other nodes, just continue the traversal
        _ -> act
    }

-- | Pre-pass to collect all function names.
collectFunctionNames :: [(FilePath, [C.Node (C.Lexeme Text)])] -> Set FunctionName
collectFunctionNames tus = execState (traverseAst nameCollectorActions tus) Set.empty
  where
    nameCollectorActions = astActions
        { doNode = \_file node act -> case unFix node of
            C.FunctionDefn _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)) _ -> do
                modify (Set.insert name)
                act
            C.FunctionDecl _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)) -> do
                modify (Set.insert name)
                act
            _ -> act
        }

-- | The main function to build the call graph from a list of top-level AST nodes.
buildCallGraph :: [(FilePath, [C.Node (C.Lexeme Text)])] -> PointsToMap -> CallGraph
buildCallGraph tus pointsToMap =
    let functionNames = collectFunctionNames tus
        initialState = BuilderState Map.empty Nothing functionNames pointsToMap
    in bsGraph $ execState (traverseAst callGraphActions tus) initialState

-- | Helper to add a call to the graph.
addCall :: FunctionName -> FunctionName -> CallSite -> State BuilderState ()
addCall caller callee callSite = modify $ \st ->
    let
        graph = bsGraph st
        calleeMap = Map.findWithDefault Map.empty caller graph
        callSiteSet = Map.findWithDefault Set.empty callee calleeMap
        newCallSiteSet = Set.insert callSite callSiteSet
        newCalleeMap = Map.insert callee newCallSiteSet calleeMap
        newGraph = Map.insert caller newCalleeMap graph
    in
        st { bsGraph = newGraph }

-- | Helper function to get all functions called by a given function.
getCallees :: CallGraph -> FunctionName -> CalleeMap
getCallees graph callerName = Map.findWithDefault Map.empty callerName graph
