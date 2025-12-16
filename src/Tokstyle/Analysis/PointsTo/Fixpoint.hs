{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Tokstyle.Analysis.PointsTo.Fixpoint
    ( CallGraph
    , CFG
    , runGlobalFixpoint
    , findEntryPointsAndFuncMap
    , findVarTypes
    ) where

import           Control.Monad                    (foldM, forM, forM_, when)
import           Control.Monad.State.Strict       (State, StateT, execState,
                                                   execStateT, get, gets, lift,
                                                   modify, runState)
import           Data.Fix                         (Fix (..))
import           Data.IntMap.Strict               (IntMap)
import qualified Data.IntMap.Strict               as IntMap
import           Data.IntSet                      (IntSet)
import qualified Data.IntSet                      as IntSet
import           Data.List                        (find, foldl')
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe, listToMaybe,
                                                   mapMaybe)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import qualified Data.Text                        as Text
import           Debug.Trace                      (trace, traceM)
import qualified Language.Cimple                  as C
import           Language.Cimple.TraverseAst      (AstActions (..), astActions,
                                                   traverseAst)
import           Tokstyle.Analysis.DataFlow       (CFGNode (..), DataFlow (..),
                                                   buildCFG, fixpoint, join,
                                                   transfer)
import           Tokstyle.Analysis.PointsTo       (evalExpr, extractRelevantState)
import           Tokstyle.Analysis.PointsTo.Types
import           Tokstyle.Analysis.Scope          (ScopedId (..))
import           Tokstyle.Common.TypeSystem       (TypeDescr (..), lookupType)
import           Tokstyle.Worklist                (Worklist, fromList, pop,
                                                   push, pushList)

debugging :: Bool
debugging = False

dtrace :: String -> a -> a
dtrace msg x = if debugging then trace msg x else x

dtraceM :: Monad m => String -> m ()
dtraceM msg = when debugging (traceM msg)

-- | The call graph of the program.
type CallGraph = Map (ScopedId, RelevantInputState) (Set (ScopedId, RelevantInputState))
type CFG id fact = Map Int (CFGNode id fact)
type CFGCache = Map (ScopedId, RelevantInputState) ([CFG ScopedId PointsToFact], Map ScopedId Int)

data FixpointState = FixpointState
    { fsGlobalEnv         :: GlobalEnv
    , fsCallGraph         :: CallGraph
    , fsReversedCallGraph :: CallGraph
    , fsWorklist          :: Worklist (ScopedId, RelevantInputState, [ScopedId])
    , fsFuncs             :: Map ScopedId [C.Node (C.Lexeme ScopedId)]
    , fsInitialFacts      :: PointsToFact
    , fsCFGCache          :: CFGCache
    , fsVarTypes          :: Map ScopedId (Map ScopedId (C.Node (C.Lexeme ScopedId)))
    , fsParams            :: Map ScopedId [ScopedId]
    , fsMemLocPool        :: MemLocPool
    }

-- | The main driver for the global fixpoint analysis.
runGlobalFixpoint :: PointsToContext ScopedId -> [C.Node (C.Lexeme ScopedId)] -> (GlobalEnv, CallGraph, CFGCache, MemLocPool)
runGlobalFixpoint ctx ast =
    let
        (initialFunctions, funcMap) = findEntryPointsAndFuncMap ast
        initialEnv = GlobalEnv Map.empty
        tmpCtx = ctx { pcFuncs = funcMap, pcGlobalEnv = initialEnv }
        initialPool = MemLocPool 0 Map.empty IntMap.empty IntMap.empty
        (initialGlobalFacts, poolAfterInit) = runState (initializeGlobalFacts tmpCtx ast) initialPool
        tracedInitialFacts = dtrace ("initialGlobalFacts: " ++ show initialGlobalFacts) initialGlobalFacts

        -- Precompute static function properties
        precomputedVarTypes = Map.map (findVarTypes . head) funcMap
        precomputedParams = Map.map (getParams . head) funcMap

        (initialWorklist, poolAfterRIS) = runState (do
            list <- forM initialFunctions $ \funcId -> do
                let params = fromMaybe [] (Map.lookup funcId precomputedParams)
                kvs <- forM params $ \p -> do
                    iloc <- intern (ExternalParamLoc (sidName funcId) (sidName p))
                    return (p, IntSet.singleton (unIMemLoc iloc))
                let initialVarMap = Map.fromList kvs
                ris <- extractRelevantState tracedInitialFacts initialVarMap
                return (funcId, ris, [])
            return $ fromList list
            ) poolAfterInit

        initialState = FixpointState
            { fsGlobalEnv = initialEnv
            , fsCallGraph = Map.empty
            , fsReversedCallGraph = Map.empty
            , fsWorklist = initialWorklist
            , fsFuncs = funcMap
            , fsInitialFacts = tracedInitialFacts
            , fsCFGCache = Map.empty
            , fsVarTypes = precomputedVarTypes
            , fsParams = precomputedParams
            , fsMemLocPool = poolAfterRIS
            }

        finalState = execState (iterateFixpoint ctx) initialState
    in
        (fsGlobalEnv finalState, fsCallGraph finalState, fsCFGCache finalState, fsMemLocPool finalState)

liftPointsToAnalysis :: PointsToAnalysis a -> State FixpointState a
liftPointsToAnalysis ma = do
    pool <- gets fsMemLocPool
    let (result, newPool) = runState ma pool
    modify $ \s -> s { fsMemLocPool = newPool }
    return result

getTypeName :: C.Node (C.Lexeme ScopedId) -> Maybe Text.Text
getTypeName (Fix node) = case node of
    C.TyUserDefined (C.L _ _ sid) -> Just (sidName sid)
    C.TyStruct (C.L _ _ sid)      -> Just (sidName sid)
    C.TyConst t                   -> getTypeName t
    _                             -> Nothing

initializeGlobalFacts :: PointsToContext ScopedId -> [C.Node (C.Lexeme ScopedId)] -> PointsToAnalysis PointsToFact
initializeGlobalFacts ctx ast =
    let
        initialFacts = PointsToFact Map.empty IntMap.empty IntSet.empty

        finder :: C.Node (C.Lexeme ScopedId) -> StateT PointsToFact PointsToAnalysis ()
        finder (Fix (C.ConstDefn _ ty (C.L _ _ sid) initializer)) =
            case unFix initializer of
                C.InitialiserList initializers -> do
                    facts <- get
                    let mTypeName = getTypeName ty
                    case mTypeName of
                        Just typeName ->
                            case lookupType typeName (pcTypeSystem ctx) of
                                Just (StructDescr _ fields) -> do
                                    let fieldNames = map (C.lexemeText . fst) fields
                                    evaledInits <- lift $ mapM (evalExpr facts ctx 0) initializers
                                    globalLoc <- lift $ intern (GlobalVarLoc sid)
                                    fieldLocs <- lift $ mapM (\fName -> intern (FieldLoc (GlobalVarLoc sid) fName)) fieldNames
                                    let newEntries = IntMap.fromList $ zip (map unIMemLoc fieldLocs) evaledInits
                                    modify $ \f -> f { memMap = IntMap.union (memMap f) newEntries }
                                _ -> return ()
                        _ -> return ()
                _ -> do
                    facts <- get
                    initLocs <- lift $ evalExpr facts ctx 0 initializer
                    when (not (IntSet.null initLocs)) $ do
                        globalLoc <- lift $ intern (GlobalVarLoc sid)
                        modify $ \f -> f { memMap = IntMap.insert (unIMemLoc globalLoc) initLocs (memMap f) }
        finder _ = return ()

        actions = astActions { doNode = \_ n act -> finder n >> act }
    in
        execStateT (traverseAst actions ast) initialFacts

mergeSummaries :: [FunctionSummary] -> FunctionSummary
mergeSummaries summaries =
    let allReturnValues = IntSet.unions (map fsReturnValue summaries)
        allParamEffects = Map.unionsWith IntSet.union (map fsParamEffects summaries)
        allMemEffects = IntMap.unionsWith IntSet.union (map fsMemEffects summaries)
    in FunctionSummary allReturnValues allParamEffects allMemEffects

iterateFixpoint :: PointsToContext ScopedId -> State FixpointState ()
iterateFixpoint ctx = do
    worklist <- gets fsWorklist
    case pop worklist of
        Nothing -> return () -- Fixpoint reached
        Just ((funcId, ris, callStack), worklist') -> do
            dtraceM $ "Analyzing func: " ++ show (sidName funcId) ++ " with RIS"
            modify $ \s -> s { fsWorklist = worklist' }

            -- Recursion Check
            when (funcId `elem` callStack) $
                error $ "Recursion detected: " ++ show (map sidName (reverse (funcId : callStack)))

            st <- get
            let currentGlobalEnv = fsGlobalEnv st
            let funcNodes = fromMaybe (error $ "Function AST not found for: " ++ show funcId) (Map.lookup funcId (fsFuncs st))
            let varTypes = fromMaybe Map.empty (Map.lookup funcId (fsVarTypes st))
            let updatedCtx = ctx { pcGlobalEnv = currentGlobalEnv, pcFuncs = fsFuncs st, pcCurrentFunc = funcId, pcVarTypes = varTypes } :: PointsToContext ScopedId

            let params = fromMaybe [] (Map.lookup funcId (fsParams st))
            initialGlobalFacts <- gets fsInitialFacts

            -- Use RIS as initial facts
            let (RelevantInputState initialFactsForFunc) = ris
            dtraceM $ "    initialFactsForFunc: " ++ show initialFactsForFunc

            -- Analyze each definition and collect results
            results <- forM funcNodes $ \funcNode -> do
                cfg <- liftPointsToAnalysis $ buildCFG updatedCtx funcNode initialFactsForFunc
                (finalCfg, newWork) <- liftPointsToAnalysis $ fixpoint updatedCtx funcId cfg

                let exitNodeFacts = cfgOutFacts $ last (Map.elems finalCfg)
                dtraceM $ "  exitNodeFacts: " ++ show exitNodeFacts
                pool <- gets fsMemLocPool
                let paramEffects = Map.fromList $ map (\(paramId, idx) ->
                                let getPointedToLocs facts param =
                                        let paramLocs = fromMaybe IntSet.empty (Map.lookup param (varMap facts))
                                        in IntSet.unions $ map (\ilocInt -> case IntMap.findWithDefault UnknownLoc ilocInt (idToMemLoc pool) of
                                            StackLoc sid -> case Map.lookup sid (varMap facts) of
                                                Just v  -> v
                                                Nothing -> IntMap.findWithDefault IntSet.empty ilocInt (memMap facts)
                                            _            -> IntMap.findWithDefault IntSet.empty ilocInt (memMap facts)
                                        ) (IntSet.toList paramLocs)
                                    finalPointedTo = getPointedToLocs exitNodeFacts paramId
                                in (idx, finalPointedTo)
                        ) (zip params [0..])

                let returns = findReturnStmts funcNode
                let stmtToNodeId = Map.unions $ map (\node -> Map.fromList $ map (, cfgNodeId node) (cfgStmts node)) (Map.elems finalCfg)
                returnLocs <- liftPointsToAnalysis $ foldM (\acc ret ->
                            case Map.lookup ret stmtToNodeId of
                                Nothing ->
                                    case unFix ret of
                                        C.Return (Just _) -> do
                                            iloc <- intern UnknownLoc
                                            return $ IntSet.insert (unIMemLoc iloc) acc
                                        _                 -> return acc
                                Just nodeId ->
                                    let nodeFacts = cfgOutFacts $ fromMaybe (error "CFG node not found") (Map.lookup nodeId finalCfg)
                                    in case unFix ret of
                                        C.Return (Just expr) ->
                                            case unFix expr of
                                                C.LiteralExpr {} -> return acc
                                                _                -> do
                                                    locs <- evalExpr nodeFacts updatedCtx nodeId expr
                                                    return $ IntSet.union locs acc
                                        _ -> return acc
                        ) IntSet.empty returns
                let memEffects = IntMap.differenceWith (\n o -> if n == o then Nothing else Just n) (memMap exitNodeFacts) (memMap initialFactsForFunc)
                dtraceM $ "    memEffects for one def: " ++ show memEffects
                let summary = FunctionSummary returnLocs paramEffects memEffects
                dtraceM $ "    summary for one def: " ++ show summary
                return (summary, newWork, finalCfg)

            let (summaries, workSets, cfgs) = unzip3 results
            let newSummary = mergeSummaries summaries
            let newWork = Set.unions workSets
            dtraceM $ "  merged newSummary: " ++ show newSummary

            modify $ \s -> s { fsCFGCache = Map.insert (funcId, ris) (cfgs, Map.empty) (fsCFGCache s) }

            -- Handle new call targets by adding them to the worklist
            forM_ (Set.toList newWork) $ \(callee, calleeRIS) -> do
                let newCallStack = funcId : callStack
                let newWorkItem = (callee, calleeRIS, newCallStack)
                dtraceM $ "  adding to worklist: " ++ show (sidName callee) ++ " with RIS"
                modify $ \s -> s { fsWorklist = push newWorkItem (fsWorklist s) }
                let callerKey = (funcId, ris)
                let calleeKey = (callee, calleeRIS)
                let newEdge = Map.singleton callerKey (Set.singleton calleeKey)
                let newRevEdge = Map.singleton calleeKey (Set.singleton callerKey)
                modify $ \s -> s { fsCallGraph = Map.unionWith Set.union (fsCallGraph s) newEdge
                                 , fsReversedCallGraph = Map.unionWith Set.union (fsReversedCallGraph s) newRevEdge
                                 }

            let GlobalEnv gEnv = currentGlobalEnv
            let oldSummaryTuple = Map.lookup (funcId, ris) gEnv

            case oldSummaryTuple of
                Nothing -> do -- First time analyzing this function/context
                    dtraceM "  first analysis, storing summary and propagating to callers"
                    let newGlobalEnv = GlobalEnv (Map.insert (funcId, ris) (newSummary, initialFactsForFunc) gEnv)
                    let callers = fromMaybe Set.empty (Map.lookup (funcId, ris) (fsReversedCallGraph st))
                    let callerWorkItems = map (\(c, cRis) -> (c, cRis, [])) (Set.toList callers)
                    modify $ \s -> s { fsGlobalEnv = newGlobalEnv, fsWorklist = pushList callerWorkItems (fsWorklist s) }

                Just (oldSummary, _) -> do -- Re-analyzing
                    if newSummary /= oldSummary
                    then do
                        dtraceM "  summary changed, propagating to callers"
                        let newGlobalEnv = GlobalEnv (Map.insert (funcId, ris) (newSummary, initialFactsForFunc) gEnv)
                        let callers = fromMaybe Set.empty (Map.lookup (funcId, ris) (fsReversedCallGraph st))
                        let callerWorkItems = map (\(c, cRis) -> (c, cRis, [])) (Set.toList callers)
                        modify $ \s -> s { fsGlobalEnv = newGlobalEnv, fsWorklist = pushList callerWorkItems (fsWorklist s) }
                    else do
                        dtraceM "  summary unchanged"

            iterateFixpoint ctx -- Recurse

getParams :: C.Node (C.Lexeme ScopedId) -> [ScopedId]
getParams (Fix (C.FunctionDefn _ (Fix (C.FunctionPrototype _ _ params)) _)) =
    mapMaybe getParamId params
  where
    getParamId (Fix (C.VarDecl _ (C.L _ _ sid) _)) = Just sid
    getParamId _                                   = Nothing
getParams _ = []

findReturnStmts :: C.Node (C.Lexeme ScopedId) -> [C.Node (C.Lexeme ScopedId)]
findReturnStmts funcNode =
    let
        finder (node@(Fix (C.Return _))) = modify (node :)
        finder _                         = return ()
        actions = astActions { doNode = \_ n act -> finder n >> act }
    in
        execState (traverseAst actions funcNode) []

findEntryPointsAndFuncMap :: [C.Node (C.Lexeme ScopedId)] -> ([ScopedId], Map ScopedId [C.Node (C.Lexeme ScopedId)])
findEntryPointsAndFuncMap ast =
    let
        finder = EntryPointFinder Map.empty Set.empty
        finalState = execState (traverseAst entryPointActions ast) finder
        allFuncs = Map.keysSet (epFunctions finalState)
        referencedFuncs = epReferencedIds finalState `Set.intersection` allFuncs
        entryPoints = Set.toList $ allFuncs `Set.difference` referencedFuncs
    in
        (entryPoints, epFunctions finalState)

data EntryPointFinder = EntryPointFinder
    { epFunctions     :: Map ScopedId [C.Node (C.Lexeme ScopedId)]
    , epReferencedIds :: Set ScopedId
    }

entryPointActions :: AstActions (State EntryPointFinder) ScopedId
entryPointActions = astActions
    { doNode = \_ node act -> do
        case unFix node of
            C.FunctionDefn _ (Fix (C.FunctionPrototype _ (C.L _ _ funcId) _)) _ -> do
                modify $ \s -> s { epFunctions = Map.insertWith (++) funcId [node] (epFunctions s) }
                act
            C.VarExpr (C.L _ _ sid) -> do
                modify $ \s -> s { epReferencedIds = Set.insert sid (epReferencedIds s) }
                act
            C.FunctionCall callee _ -> do
                case unFix callee of
                    C.VarExpr (C.L _ _ sid) -> modify $ \s -> s { epReferencedIds = Set.insert sid (epReferencedIds s) }
                    _                       -> return ()
                act
            _ -> act
    }

findVarTypes :: C.Node (C.Lexeme ScopedId) -> Map ScopedId (C.Node (C.Lexeme ScopedId))
findVarTypes funcNode =
    let
        finder (Fix (C.VarDecl ty (C.L _ _ sid) _)) = modify (Map.insert sid ty)
        finder _                                    = return ()
        actions = astActions { doNode = \_ n act -> finder n >> act }
    in
        execState (traverseAst actions funcNode) Map.empty
