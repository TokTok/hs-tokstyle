{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Tokstyle.Analysis.PointsTo
    ( evalExpr
    , extractRelevantState
    ) where

import           Control.Monad                                (foldM, when)
import           Control.Monad.State.Strict
import           Data.Fix                                     (Fix (..))
import           Data.Hashable                                (hash)
import           Data.IntMap.Strict                           (IntMap)
import qualified Data.IntMap.Strict                           as IntMap
import           Data.IntSet                                  (IntSet)
import qualified Data.IntSet                                  as IntSet
import           Data.List                                    (foldl')
import           Data.Map.Strict                              (Map)
import qualified Data.Map.Strict                              as Map
import           Data.Maybe                                   (fromMaybe,
                                                               mapMaybe)
import           Data.Set                                     (Set)
import qualified Data.Set                                     as Set
import           Data.Text                                    (Text, pack)
import           Debug.Trace                                  (trace, traceM)
import           Language.Cimple                              (AlexPosn (..))
import           Language.Cimple                              (AlexPosn (..))
import qualified Language.Cimple                              as C
import           Language.Cimple.TraverseAst                  (AstActions (..),
                                                               astActions,
                                                               traverseAst)
import           Tokstyle.Analysis.DataFlow
import           Tokstyle.Analysis.PointsTo.ExternalSummaries (getExternalSummary,
                                                               locFromPos)
import           Tokstyle.Analysis.PointsTo.Types
import           Tokstyle.Analysis.Scope                      (ScopedId (..))
import           Tokstyle.Analysis.VTable                     (VTableMap)
import           Tokstyle.Common.TypeSystem                   (TypeDescr (..),
                                                               TypeSystem,
                                                               lookupType)

debugging :: Bool
debugging = False

dtrace :: String -> a -> a
dtrace msg x = if debugging then trace msg x else x

dtraceM :: Monad m => String -> m ()
dtraceM msg = when debugging (traceM msg)

-- Helper to find all reachable IMemLocs from a set of root IMemLocs
findReachable :: IntMap IntSet -> IntSet -> IntMap IntSet -> IntSet
findReachable fieldIdx roots mem = go roots roots
  where
    go current visited =
        let
            -- Locations pointed to by current locations
            pointedTo = IntSet.unions $ map (\l -> IntMap.findWithDefault IntSet.empty l mem) (IntSet.toList current)
            -- Fields of current locations
            fields = IntSet.unions $ map (\l -> IntMap.findWithDefault IntSet.empty l fieldIdx) (IntSet.toList current)

            newLocs = pointedTo `IntSet.union` fields
            next = newLocs `IntSet.difference` visited
        in dtrace ("findReachable: current=" ++ show current ++ ", next=" ++ show next) $
           if IntSet.null next
           then visited
           else go next (visited `IntSet.union` next)

-- | Extracts the relevant part of the abstract state for a function call.
extractRelevantState :: PointsToFact -> Map ScopedId IntSet -> PointsToAnalysis RelevantInputState
extractRelevantState facts argMap = do
    pool <- get

    -- Ensure all StackLocs from varMap are in memMap for reachability analysis.
    -- This is needed because evalExpr for &var doesn't add it to memMap.
    let memMapWithStack = Map.foldlWithKey' (\acc sid val ->
            case Map.lookup (StackLoc sid) (memLocToId pool) of
                Just iloc -> IntMap.insertWith (flip const) (unIMemLoc iloc) val acc
                Nothing -> acc
            ) (memMap facts) (varMap facts)

    let
        -- 1. Roots from arguments
        argRoots = IntSet.unions (Map.elems argMap)

        -- 2. Roots from globals. Include ALL GlobalVarLocs from pool as roots.
        globalRoots = IntMap.foldlWithKey' (\acc k v ->
                case v of
                    GlobalVarLoc _ -> IntSet.insert k acc
                    _ -> acc
            ) IntSet.empty (idToMemLoc pool)

        allRoots = argRoots `IntSet.union` globalRoots
        reachableLocs = findReachable (fieldIndex pool) allRoots memMapWithStack

        -- Filter memMap to only reachable locations
        relevantMemMap = IntMap.restrictKeys memMapWithStack reachableLocs

        -- Combine args and globals for the initial varMap of the callee
        isGlobalSid sid = sidScope sid /= C.Local
        relevantVarMap = Map.union argMap (Map.filterWithKey (\sid _ -> isGlobalSid sid) (varMap facts))

    return $ RelevantInputState $ PointsToFact relevantVarMap relevantMemMap (unknownWrites facts)


-- Helper to get the top-level lexeme from a node, if it exists.
getLexeme :: C.Node (C.Lexeme l) -> Maybe (C.Lexeme l)
getLexeme (Fix (C.VarExpr l))         = Just l
getLexeme (Fix (C.MemberAccess _ l))  = Just l
getLexeme (Fix (C.PointerAccess _ l)) = Just l
getLexeme _                           = Nothing

-- A helper function to recursively evaluate the base of a member/pointer access.
evalBaseExpr :: PointsToFact -> PointsToContext ScopedId -> Int -> C.Node (C.Lexeme ScopedId) -> PointsToAnalysis IntSet
evalBaseExpr _ _ _ (Fix (C.VarExpr (C.L _ _ sid))) =
    case sidScope sid of
        C.Local -> do
            iloc <- intern (StackLoc sid)
            return $ IntSet.singleton (unIMemLoc iloc)
        _       -> do
            iloc <- intern (GlobalVarLoc sid)
            return $ IntSet.singleton (unIMemLoc iloc)
evalBaseExpr facts ctx nodeId (Fix (C.PointerAccess baseExpr _)) =
    evalExpr facts ctx nodeId baseExpr
evalBaseExpr facts ctx nodeId (Fix (C.MemberAccess baseExpr (C.L _ _ ScopedId{sidName=fName}))) = do
    baseLocs <- evalBaseExpr facts ctx nodeId baseExpr
    pool <- get
    let baseIMemLocs = map IMemLoc (IntSet.toList baseLocs)
    let baseMemLocs = map (\iloc -> IntMap.findWithDefault UnknownLoc (unIMemLoc iloc) (idToMemLoc pool)) baseIMemLocs
    fieldILocs <- mapM (\base -> intern (FieldLoc base fName)) baseMemLocs
    return $ IntSet.fromList (map unIMemLoc fieldILocs)
evalBaseExpr _ _ _ _ = do
    iloc <- intern UnknownLoc
    return $ IntSet.singleton (unIMemLoc iloc)

-- Evaluates a C expression to determine the set of memory locations it might refer to.
evalExpr :: PointsToFact -> PointsToContext ScopedId -> Int -> C.Node (C.Lexeme ScopedId) -> PointsToAnalysis IntSet
evalExpr _ _ _ (Fix (C.VarExpr (C.L _ _ ScopedId{sidName="nullptr"}))) = do
    iloc <- intern NullLoc
    return $ IntSet.singleton (unIMemLoc iloc)
evalExpr facts _ _ (Fix (C.VarExpr (C.L _ _ sid))) =
    case Map.lookup sid (varMap facts) of
        Just locs -> return locs
        Nothing   ->
            case sidScope sid of
                C.Local -> do
                    iloc <- intern UnknownLoc
                    return $ IntSet.singleton (unIMemLoc iloc)
                _       -> do
                    globalLoc <- intern (GlobalVarLoc sid)
                    return $ IntMap.findWithDefault (IntSet.singleton (unIMemLoc globalLoc)) (unIMemLoc globalLoc) (memMap facts)
evalExpr _ _ _ (Fix (C.UnaryExpr C.UopAddress (Fix (C.VarExpr (C.L _ _ sid))))) =
    case sidScope sid of
        C.Local -> do
            iloc <- intern (StackLoc sid)
            return $ IntSet.singleton (unIMemLoc iloc)
        _       -> do
            iloc <- intern (GlobalVarLoc sid)
            return $ IntSet.singleton (unIMemLoc iloc)
evalExpr _ _ _ (Fix (C.UnaryExpr C.UopAddress (Fix (C.MemberAccess (Fix (C.VarExpr (C.L _ _ structId))) (C.L _ _ ScopedId{sidName=fName}))))) = do
    iloc <- intern (FieldLoc (StackLoc structId) fName)
    return $ IntSet.singleton (unIMemLoc iloc)
evalExpr facts ctx nodeId (Fix (C.UnaryExpr C.UopAddress (Fix (C.PointerAccess baseExpr (C.L _ _ ScopedId{sidName=fName}))))) = do
    baseLocs <- evalExpr facts ctx nodeId baseExpr
    pool <- get
    let baseMemLocs = map (\i -> IntMap.findWithDefault UnknownLoc i (idToMemLoc pool)) (IntSet.toList baseLocs)
    fieldILocs <- mapM (\base -> intern (FieldLoc base fName)) baseMemLocs
    return $ IntSet.fromList (map unIMemLoc fieldILocs)
evalExpr facts ctx nodeId (Fix (C.UnaryExpr C.UopDeref expr)) = do
    locs <- evalExpr facts ctx nodeId expr
    unknownLoc <- intern UnknownLoc
    let unknownResult = if IntSet.member (unIMemLoc unknownLoc) locs
                        then unknownWrites facts `IntSet.union` (IntSet.singleton (unIMemLoc unknownLoc))
                        else IntSet.empty
    pool <- get
    let derefLoc ilocInt = do
            let loc = IntMap.findWithDefault UnknownLoc ilocInt (idToMemLoc pool)
            case loc of
                StackLoc sid            -> return $ Map.findWithDefault IntSet.empty sid (varMap facts)
                epl@ExternalParamLoc {} -> do
                    iloc <- intern epl
                    return $ IntMap.findWithDefault (IntSet.singleton (unIMemLoc unknownLoc)) (unIMemLoc iloc) (memMap facts)
                heapOrFieldOrGlb        -> do
                    iloc <- intern heapOrFieldOrGlb
                    return $ IntMap.findWithDefault IntSet.empty (unIMemLoc iloc) (memMap facts)
    derefedLocs <- mapM derefLoc (IntSet.toList locs)
    return $ foldl' IntSet.union unknownResult derefedLocs
evalExpr facts ctx nodeId (Fix (C.PointerAccess baseExpr (C.L _ _ ScopedId{sidName=fName}))) = do
    baseLocs <- evalExpr facts ctx nodeId baseExpr
    pool <- get
    -- V-Table resolution
    vtableResults <- foldM (\acc ilocInt -> do
        let loc = IntMap.findWithDefault UnknownLoc ilocInt (idToMemLoc pool)
        case loc of
            GlobalVarLoc vtableSid ->
                case Map.lookup vtableSid (pcVTableMap ctx) of
                    Just fields ->
                        case Map.lookup fName fields of
                            Just funcSid -> do
                                funcLoc <- intern (GlobalVarLoc funcSid)
                                return $ IntSet.insert (unIMemLoc funcLoc) acc
                            Nothing      -> return acc
                    Nothing -> return acc
            _ -> return acc
        ) IntSet.empty (IntSet.toList baseLocs)

    -- Regular field access
    regularResults <- foldM (\acc ilocInt -> do
        let base = IntMap.findWithDefault UnknownLoc ilocInt (idToMemLoc pool)
        fieldLoc <- intern (FieldLoc base fName)
        let deref = IntMap.findWithDefault IntSet.empty (unIMemLoc fieldLoc) (memMap facts)
        return $ IntSet.union deref acc
        ) IntSet.empty (IntSet.toList baseLocs)

    return $ IntSet.union vtableResults regularResults
evalExpr facts ctx nodeId (Fix (C.MemberAccess baseExpr (C.L _ _ ScopedId{sidName=fName}))) = do
    baseLocs <- evalBaseExpr facts ctx nodeId baseExpr
    pool <- get
    foldM (\acc ilocInt -> do
        let base = IntMap.findWithDefault UnknownLoc ilocInt (idToMemLoc pool)
        fieldLoc <- intern (FieldLoc base fName)
        let deref = IntMap.findWithDefault IntSet.empty (unIMemLoc fieldLoc) (memMap facts)
        return $ IntSet.union deref acc
        ) IntSet.empty (IntSet.toList baseLocs)
evalExpr facts ctx nodeId (Fix (C.ArrayAccess baseExpr idxExpr)) = do
    baseLocs <- evalBaseExpr facts ctx nodeId baseExpr
    pool <- get
    let fieldName = case unFix idxExpr of
            C.LiteralExpr C.Int (C.L _ _ idxLexeme) -> sidName idxLexeme
            _                                       -> "[]"
    foldM (\acc ilocInt -> do
        let base = IntMap.findWithDefault UnknownLoc ilocInt (idToMemLoc pool)
        fieldLoc <- intern (FieldLoc base fieldName)
        let deref = IntMap.findWithDefault IntSet.empty (unIMemLoc fieldLoc) (memMap facts)
        return $ IntSet.union deref acc
        ) IntSet.empty (IntSet.toList baseLocs)
evalExpr facts ctx nodeId (Fix (C.CastExpr _ expr)) = evalExpr facts ctx nodeId expr
evalExpr facts ctx nodeId (Fix (C.FunctionCall calleeExpr args)) = do
    calleeLocs <- evalExpr facts ctx nodeId calleeExpr
    let pos = case getLexeme calleeExpr of
            Just (C.L p _ _) -> p
            Nothing          -> C.AlexPn 0 0 0

    pool <- get
    unknownLoc <- intern UnknownLoc

    let applySummary ilocInt = do
            let loc = IntMap.findWithDefault UnknownLoc ilocInt (idToMemLoc pool)
            case loc of
                StackLoc sid     -> handleCall sid
                GlobalVarLoc sid -> handleCall sid
                _                -> return $ IntSet.singleton (unIMemLoc unknownLoc) -- Not a function call

        handleCall sid =
            case getExternalSummary sid of
                Just summary -> do
                    (retLocs, _) <- summary (pcFilePath ctx) pos args
                    internedRetLocs <- mapM intern (Set.toList retLocs)
                    return $ IntSet.fromList (map unIMemLoc internedRetLocs)
                Nothing      ->
                    let GlobalEnv gEnv = pcGlobalEnv ctx
                    in do
                        let funcMap = pcFuncs ctx
                        case Map.lookup sid funcMap of
                            Just _ -> do -- Internal function
                                let calleeFuncs = fromMaybe (error $ "Function AST not found for: " ++ show sid) (Map.lookup sid funcMap)
                                let calleeFunc = head calleeFuncs
                                let params = getParams calleeFunc
                                argLocs <- mapM (evalExpr facts ctx nodeId) args
                                let initialVarMap = Map.fromList $ zip params argLocs

                                relevantState <- extractRelevantState facts initialVarMap

                                case Map.lookup (sid, relevantState) gEnv of
                                    Just (summary, _) -> return $ fsReturnValue summary
                                    Nothing           -> return $ IntSet.singleton (unIMemLoc unknownLoc) -- Unsummarized internal call
                            Nothing -> -- Truly external
                                return $ IntSet.singleton (unIMemLoc unknownLoc)

    results <- mapM applySummary (IntSet.toList calleeLocs)
    return $ IntSet.unions results
evalExpr _ _ _ _ = do
    iloc <- intern UnknownLoc
    return $ IntSet.singleton (unIMemLoc iloc) -- Default for unhandled expressions
-- Evaluates the left-hand side of an assignment to a set of memory locations.
evalLhsExpr :: PointsToFact -> PointsToContext ScopedId -> Int -> C.Node (C.Lexeme ScopedId) -> PointsToAnalysis IntSet
evalLhsExpr _ _ _ (Fix (C.VarExpr (C.L _ _ sid))) = do
    iloc <- intern (StackLoc sid)
    return $ IntSet.singleton (unIMemLoc iloc)
evalLhsExpr facts ctx nodeId (Fix (C.UnaryExpr C.UopDeref expr)) = evalExpr facts ctx nodeId expr
evalLhsExpr facts ctx nodeId (Fix (C.MemberAccess baseExpr (C.L _ _ ScopedId{sidName=fName}))) = do
    baseLocs <- evalBaseExpr facts ctx nodeId baseExpr
    pool <- get
    let baseMemLocs = map (\i -> IntMap.findWithDefault UnknownLoc i (idToMemLoc pool)) (IntSet.toList baseLocs)
    fieldILocs <- mapM (\base -> intern (FieldLoc base fName)) baseMemLocs
    return $ IntSet.fromList (map unIMemLoc fieldILocs)
evalLhsExpr facts ctx nodeId (Fix (C.PointerAccess baseExpr (C.L _ _ ScopedId{sidName=fName}))) = do
    baseLocs <- evalExpr facts ctx nodeId baseExpr
    pool <- get
    let baseMemLocs = map (\i -> IntMap.findWithDefault UnknownLoc i (idToMemLoc pool)) (IntSet.toList baseLocs)
    fieldILocs <- mapM (\base -> intern (FieldLoc base fName)) baseMemLocs
    return $ IntSet.fromList (map unIMemLoc fieldILocs)
evalLhsExpr facts ctx nodeId (Fix (C.ArrayAccess baseExpr idxExpr)) = do
    baseLocs <- evalBaseExpr facts ctx nodeId baseExpr
    pool <- get
    let baseMemLocs = map (\i -> IntMap.findWithDefault UnknownLoc i (idToMemLoc pool)) (IntSet.toList baseLocs)
    fieldILocs <- case unFix idxExpr of
        C.LiteralExpr C.Int (C.L _ _ idxLexeme) ->
            mapM (\base -> intern (FieldLoc base (sidName idxLexeme))) baseMemLocs
        _ -> -- Non-constant index, smash the array.
            mapM (\base -> intern (FieldLoc base "[]")) baseMemLocs
    return $ IntSet.fromList (map unIMemLoc fieldILocs)
evalLhsExpr _ _ _ _ = do
    iloc <- intern UnknownLoc
    return $ IntSet.singleton (unIMemLoc iloc)

-- | A helper function to encapsulate the logic for handling a function call within the data flow analysis.
-- It resolves the callee, looks up or generates a summary, applies the summary's effects,
-- and returns the updated facts, any new work for the fixpoint solver, and the return value of the call.
handleFunctionCall :: PointsToContext ScopedId
                   -> ScopedId
                   -> Int
                   -> PointsToFact
                   -> C.Node (C.Lexeme ScopedId)
                   -> [C.Node (C.Lexeme ScopedId)]
                   -> PointsToAnalysis (PointsToFact, Set (ScopedId, RelevantInputState), IntSet)
handleFunctionCall ctx funcId nodeId facts calleeExpr args = do
    calleeLocs <- evalExpr facts ctx nodeId calleeExpr
    let pos = case getLexeme calleeExpr of
            Just (C.L p _ _) -> p
            Nothing          -> C.AlexPn 0 0 0

    pool <- get
    unknownLoc <- intern UnknownLoc

    let processCall ilocInt = do
            let loc = IntMap.findWithDefault UnknownLoc ilocInt (idToMemLoc pool)
            case loc of
                GlobalVarLoc sid -> handleCall sid
                _                -> return ((facts, Set.empty), IntSet.singleton (unIMemLoc unknownLoc))

        handleCall sid =
            if sidName sid `elem` ["memcpy", "memmove"] then do
                let destExpr = args !! 0
                let srcExpr = args !! 1
                destBaseLocs <- evalExpr facts ctx nodeId destExpr
                srcBaseLocs <- evalExpr facts ctx nodeId srcExpr

                -- Helper to replace the base of a memory location.
                let rebase oldBase newBase targetLoc =
                        if targetLoc == oldBase
                        then newBase
                        else case targetLoc of
                            FieldLoc b f -> FieldLoc (rebase oldBase newBase b) f
                            _            -> targetLoc

                -- Helper to get all transitive fields of a base location.
                let getTransitiveFields baseIloc p =
                        let immediate = IntSet.toList $ IntMap.findWithDefault IntSet.empty (unIMemLoc baseIloc) (fieldIndex p)
                        in immediate ++ concatMap (\f -> getTransitiveFields (IMemLoc f) p) immediate

                -- Group sources by destination to handle multiple sources for one dest correctly.
                let destToSources = IntMap.fromListWith (++) [(d, [s]) | d <- IntSet.toList destBaseLocs, s <- IntSet.toList srcBaseLocs]

                finalMemMap <- foldM (\accMem (dIlocInt, srcIlocInts) -> do
                    currentPool <- get
                    let dLoc = IntMap.findWithDefault UnknownLoc dIlocInt (idToMemLoc currentPool)

                    -- Compute all updates for this destination from all its sources.
                    updatesForDest <- foldM (\accUpdates srcIlocInt -> do
                        let sLoc = IntMap.findWithDefault UnknownLoc srcIlocInt (idToMemLoc currentPool)
                        -- Identify all source locations to copy (base + all transitive fields)
                        let allSrcIlocs = srcIlocInt : getTransitiveFields (IMemLoc srcIlocInt) currentPool

                        foldM (\innerUpdates srcFieldInt -> do
                            case IntMap.lookup srcFieldInt (memMap facts) of
                                Nothing -> return innerUpdates
                                Just v -> do
                                    -- We need to re-get pool because intern might have run in previous iteration
                                    p <- get
                                    let srcFieldLoc = IntMap.findWithDefault UnknownLoc srcFieldInt (idToMemLoc p)
                                    let destFieldLoc = rebase sLoc dLoc srcFieldLoc
                                    destFieldIloc <- intern destFieldLoc
                                    return $ IntMap.insertWith IntSet.union (unIMemLoc destFieldIloc) v innerUpdates
                            ) accUpdates allSrcIlocs
                        ) IntMap.empty srcIlocInts

                    -- Apply updates to accMem.
                    -- If we have a single destination base, we can perform a strong update (overwrite).
                    -- Otherwise, we must perform a weak update (merge).
                    if IntSet.size destBaseLocs == 1
                        then return $ IntMap.union updatesForDest accMem -- Strong update: updates win
                        else return $ IntMap.unionWith IntSet.union accMem updatesForDest -- Weak update
                    ) (memMap facts) (IntMap.toList destToSources)

                let newFacts = facts { memMap = finalMemMap }
                memcpyRetLocs <- evalExpr newFacts ctx nodeId destExpr
                return ((newFacts, Set.empty), memcpyRetLocs)

            else if sidName sid == "getaddrinfo" then do
                let resExpr = args !! 3
                resLocs <- evalExpr facts ctx nodeId resExpr
                newHeapLoc <- intern (HeapLoc (locFromPos (pcFilePath ctx) pos))
                let rhsLocs = IntSet.singleton (unIMemLoc newHeapLoc)
                pool' <- get
                let update ilocInt acc =
                        let loc = IntMap.findWithDefault UnknownLoc ilocInt (idToMemLoc pool')
                        in case loc of
                            StackLoc sid' -> acc { varMap = Map.insert sid' rhsLocs (varMap acc) }
                            _             -> acc { memMap = IntMap.insert ilocInt rhsLocs (memMap acc) }
                let newFacts = IntSet.foldr update facts resLocs
                return ((newFacts, Set.empty), IntSet.empty) -- Returns int
            else if sidName sid == "inet_ntop" then do
                inetNtopRetLocs <- evalExpr facts ctx nodeId (args !! 2) -- dst
                return ((facts, Set.empty), inetNtopRetLocs)
            else if sidName sid == "strrchr" then do
                strrchrRetLocs <- evalExpr facts ctx nodeId (args !! 0) -- str
                return ((facts, Set.empty), strrchrRetLocs)
            else
                case getExternalSummary sid of
                    Just summary -> do
                        (summaryRetLocs, _) <- summary (pcFilePath ctx) pos args
                        internedRetLocs <- mapM intern (Set.toList summaryRetLocs)
                        return ((facts, Set.empty), IntSet.fromList (map unIMemLoc internedRetLocs))
                    Nothing -> -- No external summary
                        let funcMap = pcFuncs ctx
                        in case Map.lookup sid funcMap of
                            Just _ -> do -- Internal function, proceed with context-sensitive analysis
                                let GlobalEnv gEnv = pcGlobalEnv ctx
                                let calleeFuncs = fromMaybe (error $ "Function AST not found for: " ++ show sid) (Map.lookup sid funcMap)
                                let calleeFunc = head calleeFuncs -- Assume first def is representative
                                let params = getParams calleeFunc
                                argLocs <- mapM (evalExpr facts ctx nodeId) args
                                let initialVarMap = Map.fromList $ zip params argLocs

                                relevantState <- extractRelevantState facts initialVarMap

                                case Map.lookup (sid, relevantState) gEnv of
                                    Just (summary, _) -> do
                                        (updatedFacts, summaryRetLocs) <- applySummary facts summary
                                        return ((updatedFacts, Set.empty), summaryRetLocs)
                                    Nothing ->
                                        return ((facts, Set.singleton (sid, relevantState)), IntSet.singleton (unIMemLoc unknownLoc))
                            Nothing -> -- Truly external and unsummarized
                                return ((facts, Set.empty), IntSet.singleton (unIMemLoc unknownLoc))

        applySummary :: PointsToFact -> FunctionSummary -> PointsToAnalysis (PointsToFact, IntSet)
        applySummary currentFacts summary = do
            let ret = fsReturnValue summary
            let paramEff = fsParamEffects summary
            let memEff = fsMemEffects summary
            
            dtraceM $ "applySummary: current varMap: " ++ show (varMap currentFacts)
            dtraceM $ "applySummary: memEff: " ++ show memEff

            -- Apply memory effects WEAKLY to ensure soundness.
            let newMemMap = IntMap.unionWith IntSet.union memEff (memMap currentFacts)

            -- Sync varMap from memEff WEAKLY, as StackLocs are in both.
            currentPool <- get
            let newVarMap = IntMap.foldlWithKey' (\acc k v ->
                    case IntMap.lookup k (idToMemLoc currentPool) of
                        Just (StackLoc sid) -> Map.insertWith IntSet.union sid v acc
                        _ -> acc
                    ) (varMap currentFacts) memEff

            dtraceM $ "applySummary: newVarMap: " ++ show newVarMap

            let factsWithMemEffects = currentFacts { memMap = newMemMap, varMap = newVarMap }

            -- Then, apply parameter effects on top of that.
            updatedFacts <- foldM (applyEffect facts) factsWithMemEffects (Map.toList paramEff)
            return (updatedFacts, ret)

        applyEffect :: PointsToFact -> PointsToFact -> (Int, IntSet) -> PointsToAnalysis PointsToFact
        applyEffect initialFacts currentFacts (argIdx, newLocs)
            | IntSet.null newLocs = return currentFacts
            | otherwise = do
                let argExpr = args !! argIdx
                argLocs <- evalExpr initialFacts ctx nodeId argExpr
                pool' <- get
                case unFix argExpr of
                    C.UnaryExpr C.UopAddress (Fix (C.VarExpr (C.L _ _ argSid))) -> do
                        let updatedMemMap = IntSet.foldl' (\acc ilocInt -> IntMap.insert ilocInt newLocs acc) (memMap currentFacts) argLocs
                        return $ currentFacts { memMap = updatedMemMap, varMap = Map.insert argSid newLocs (varMap currentFacts) }
                    _ -> do
                        let update ilocInt acc =
                                let acc' = acc { memMap = IntMap.insert ilocInt newLocs (memMap acc) }
                                    loc = IntMap.findWithDefault UnknownLoc ilocInt (idToMemLoc pool')
                                in case loc of
                                    StackLoc sid -> acc' { varMap = Map.insert sid newLocs (varMap acc') }
                                    _            -> acc'
                        return $ IntSet.foldr update currentFacts argLocs

    results <- mapM processCall (IntSet.toList calleeLocs)
    let unpackedResults = results
    let resultFacts = map (fst . fst) unpackedResults
    let resultEdges = map (snd . fst) unpackedResults
    let resultRetLocs = map snd unpackedResults

    finalFacts <- if null resultFacts then return facts else foldM (join ctx) (head resultFacts) (tail resultFacts)
    let finalEdges = Set.unions resultEdges
    let retLocs = IntSet.unions resultRetLocs
    return (finalFacts, finalEdges, retLocs)

instance DataFlow PointsToAnalysis PointsToContext ScopedId PointsToFact RelevantInputState where
    emptyFacts _ = return $ PointsToFact Map.empty IntMap.empty IntSet.empty

    transfer ctx funcId nodeId facts stmt@(Fix s') = do
        let traceMsg = "transfer: " ++ show (sidName funcId) ++ " " ++ show (fmap (const ()) s')
        (facts', newEdges) <- case s' of
            C.VarDeclStmt (Fix (C.VarDecl _ (C.L _ _ lhsSid) _)) (Just rhs) ->
                let
                    handleTheCall calleeExpr args = do
                        (finalFacts, finalEdges, retLocs) <- handleFunctionCall ctx funcId nodeId facts calleeExpr args
                        return (finalFacts { varMap = Map.insert lhsSid retLocs (varMap finalFacts) }, finalEdges)
                in
                    case unFix rhs of
                        C.CastExpr _ (Fix (C.FunctionCall calleeExpr args)) -> handleTheCall calleeExpr args
                        C.FunctionCall calleeExpr args                  -> handleTheCall calleeExpr args
                        _ -> do
                            locs <- evalExpr facts ctx nodeId rhs
                            return (facts { varMap = Map.insert lhsSid locs (varMap facts) }, Set.empty)

            C.ExprStmt (Fix (C.AssignExpr lhs _ rhs)) ->
                let
                    handleTheCall calleeExpr args = do
                        (factsAfterCall, finalEdges, retLocs) <- handleFunctionCall ctx funcId nodeId facts calleeExpr args
                        lhsLocs <- evalLhsExpr factsAfterCall ctx nodeId lhs
                        -- Need to properly check for UnknownLoc.
                        unknownLoc <- intern UnknownLoc
                        let newUnknowns' = if IntSet.member (unIMemLoc unknownLoc) lhsLocs
                                           then unknownWrites factsAfterCall `IntSet.union` retLocs
                                           else unknownWrites factsAfterCall

                        pool <- get
                        let update ilocInt acc =
                                let acc' = acc { memMap = IntMap.insert ilocInt retLocs (memMap acc) }
                                in case IntMap.findWithDefault UnknownLoc ilocInt (idToMemLoc pool) of
                                    StackLoc sid -> acc' { varMap = Map.insert sid retLocs (varMap acc') }
                                    _            -> acc'
                        return (IntSet.foldr update (factsAfterCall { unknownWrites = newUnknowns' }) lhsLocs, finalEdges)
                in
                    case unFix rhs of
                        C.CastExpr _ (Fix (C.FunctionCall calleeExpr args)) -> handleTheCall calleeExpr args
                        C.FunctionCall calleeExpr args -> handleTheCall calleeExpr args
                        _ -> do
                            lhsLocs <- evalLhsExpr facts ctx nodeId lhs
                            rhsLocs <- evalExpr facts ctx nodeId rhs
                            unknownLoc <- intern UnknownLoc
                            let newUnknowns = if IntSet.member (unIMemLoc unknownLoc) lhsLocs
                                        then unknownWrites facts `IntSet.union` rhsLocs
                                        else unknownWrites facts

                            pool <- get
                            -- Check if this assignment is to a union field.
                            let isUnionAssignment = any (\ilocInt ->
                                    case IntMap.findWithDefault UnknownLoc ilocInt (idToMemLoc pool) of
                                        FieldLoc (StackLoc sid) _ ->
                                            case Map.lookup sid (pcVarTypes ctx) of
                                                Just tyNode -> case getTypeName tyNode of
                                                    Just typeName -> case lookupType typeName (pcTypeSystem ctx) of
                                                        Just (UnionDescr _ _) -> True
                                                        _                     -> False
                                                    _ -> False
                                                _ -> False
                                        _ -> False
                                    ) (IntSet.toList lhsLocs)

                            updatedFacts <-
                                if isUnionAssignment then
                                    -- Handle union assignment atomically.
                                    let
                                        -- We only need one FieldLoc to find the base and all sibling fields.
                                        oneFieldLocInt = if IntSet.null lhsLocs then error "LHS of union assignment is empty" else IntSet.findMin lhsLocs
                                        oneFieldLoc = IntMap.findWithDefault UnknownLoc oneFieldLocInt (idToMemLoc pool)
                                    in do
                                        allFieldsToUpdate <- case oneFieldLoc of
                                            FieldLoc baseLoc@(StackLoc sid) _ ->
                                                case Map.lookup sid (pcVarTypes ctx) of
                                                    Just tyNode -> case getTypeName tyNode of
                                                        Just typeName -> case lookupType typeName (pcTypeSystem ctx) of
                                                            Just (UnionDescr _ fields) -> do
                                                                let allFieldNames = map (C.lexemeText . fst) fields
                                                                mapM (\fName -> intern (FieldLoc baseLoc fName)) allFieldNames
                                                            _ -> return [IMemLoc oneFieldLocInt]
                                                        _ -> return [IMemLoc oneFieldLocInt]
                                                    _ -> return [IMemLoc oneFieldLocInt]
                                            _ -> return [IMemLoc oneFieldLocInt]

                                        let updatedMemMap = foldl' (\m iloc -> IntMap.insert (unIMemLoc iloc) rhsLocs m) (memMap facts) allFieldsToUpdate
                                        return $ facts { memMap = updatedMemMap, unknownWrites = newUnknowns }
                                else
                                    -- Use the original fold for non-union assignments.
                                    let
                                        update ilocInt acc =
                                            let acc' = acc { memMap = IntMap.insert ilocInt rhsLocs (memMap acc) }
                                            in case IntMap.findWithDefault UnknownLoc ilocInt (idToMemLoc pool) of
                                                StackLoc sid -> acc' { varMap = Map.insert sid rhsLocs (varMap acc') }
                                                _            -> acc'
                                    in
                                        return $ IntSet.foldr update (facts { unknownWrites = newUnknowns }) lhsLocs
                            return (updatedFacts, Set.empty)

            C.ExprStmt (Fix (C.FunctionCall calleeExpr args)) -> do
                (finalFacts, finalEdges, _) <- handleFunctionCall ctx funcId nodeId facts calleeExpr args
                return (finalFacts, finalEdges)

            C.Return (Just expr) -> do
                let
                    -- The return value itself is handled by the fixpoint driver by inspecting
                    -- the exit node's facts. Here, we just need to ensure that if the return
                    -- expression contains a function call, we generate the necessary work for it.
                    _ = evalExpr facts ctx nodeId expr
                case unFix expr of
                    C.FunctionCall calleeExpr args -> do
                        (finalFacts, finalEdges, _) <- handleFunctionCall ctx funcId nodeId facts calleeExpr args
                        return (finalFacts, finalEdges)
                    C.CastExpr _ (Fix (C.FunctionCall calleeExpr args)) -> do
                        (finalFacts, finalEdges, _) <- handleFunctionCall ctx funcId nodeId facts calleeExpr args
                        return (finalFacts, finalEdges)
                    _ -> return (facts, Set.empty)

            _ -> return (facts, Set.empty)
        return (dtrace (traceMsg ++ "\n  facts': " ++ show facts') facts', newEdges)

    join _ (PointsToFact vm1 mm1 uw1) (PointsToFact vm2 mm2 uw2) =
        return $ PointsToFact (Map.unionWith IntSet.union vm1 vm2)
                     (IntMap.unionWith IntSet.union mm1 mm2)
                     (IntSet.union uw1 uw2)

getParams :: C.Node (C.Lexeme ScopedId) -> [ScopedId]
getParams (Fix (C.FunctionDefn _ (Fix (C.FunctionPrototype _ _ params)) _)) =
    mapMaybe getParamId params
  where
    getParamId (Fix (C.VarDecl _ (C.L _ _ sid) _)) = Just sid
    getParamId _                                   = Nothing
getParams _ = []

getTypeName :: C.Node (C.Lexeme ScopedId) -> Maybe Text
getTypeName (Fix node) = case node of
    C.TyUserDefined (C.L _ _ sid) -> Just (sidName sid)
    C.TyStruct (C.L _ _ sid)      -> Just (sidName sid)
    C.TyUnion (C.L _ _ sid)       -> Just (sidName sid)
    C.TyConst t                   -> getTypeName t
    _                             -> Nothing
