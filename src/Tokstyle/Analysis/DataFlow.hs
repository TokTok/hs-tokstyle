{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

-- | This module provides a generic framework for forward data flow analysis
-- on C code, represented by the 'Language.Cimple.Ast'. It includes tools
-- for building a control flow graph (CFG) from a function definition and
-- a fixpoint solver to compute data flow facts.
--
-- The core components are:
--
-- * 'CFG': A control flow graph representation, where nodes contain basic
--   blocks of statements.
-- * 'DataFlow': A type class that defines the specific analysis to be
--   performed (e.g., reaching definitions, liveness analysis).
-- * 'buildCFG': A function to construct a 'CFG' from a 'C.FunctionDefn'.
-- * 'fixpoint': A generic solver that iteratively computes data flow facts
--   until a stable state (fixpoint) is reached.
--
-- To use this module, you need to:
--
-- 1. Define a data type for your data flow facts.
-- 2. Create an instance of the 'DataFlow' type class for your data type,
--    implementing 'emptyFacts', 'transfer', and 'join'.
-- 3. Build the CFG for a function using 'buildCFG'.
-- 4. Run the 'fixpoint' solver on the generated CFG.
-- 5. Extract and use the computed 'cfgInFacts' and 'cfgOutFacts' from the
--    resulting CFG.
module Tokstyle.Analysis.DataFlow
    ( CFGNode (..)
    , CFG
    , DataFlow (..)
    , fixpoint
    , buildCFG
    ) where

import           Control.Monad              (foldM, forM, forM_)
import           Control.Monad.State.Strict (State, get, modify, put, runState)
import           Data.Fix                   (Fix (Fix, unFix))
import           Data.Foldable              (foldl')
import           Data.Kind                  (Type)
import           Data.List                  (find)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe, mapMaybe)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Debug.Trace                (trace)
import           Language.Cimple            (NodeF (..))
import qualified Language.Cimple            as C
import           Language.Cimple.Pretty     (showNodePlain)
import           Prettyprinter              (Pretty (..))
import           Text.Groom                 (groom)
import           Tokstyle.Analysis.Types    (lookupOrError)
import           Tokstyle.Worklist

debugging :: Bool
debugging = False

dtrace :: String -> a -> a
dtrace msg x = if debugging then trace msg x else x

-- | A node in the control flow graph. Each node represents a basic block
-- of statements.
data CFGNode l a = CFGNode
    { cfgNodeId   :: Int -- ^ A unique identifier for the node.
    , cfgPreds    :: [Int] -- ^ A list of predecessor node IDs.
    , cfgSuccs    :: [Int] -- ^ A list of successor node IDs.
    , cfgStmts    :: [C.Node (C.Lexeme l)] -- ^ The statements in this basic block.
    , cfgInFacts  :: a -- ^ The data flow facts at the entry of this node.
    , cfgOutFacts :: a -- ^ The data flow facts at the exit of this node.
    }
    deriving (Show, Eq)

-- | The Control Flow Graph is a map from node IDs to 'CFGNode's.
type CFG l a = Map Int (CFGNode l a)

-- | A type class for data flow analysis. Users of this framework must
-- provide an instance of this class for their specific analysis.
class (Eq a, Show a) => DataFlow (c :: Type -> Type) l a where
    -- | The facts for an empty basic block.
    emptyFacts :: c l -> a
    -- | The transfer function defines how a single statement affects the
    -- data flow facts. It takes the facts before the statement and
    -- returns the facts after the statement, plus any new work discovered.
    transfer :: c l -> l -> a -> C.Node (C.Lexeme l) -> (a, Set (l, [Int]))
    -- | The join operator combines facts from multiple predecessor nodes.
    -- This is used at control flow merge points (e.g., after an if-statement
    -- or at the start of a loop).
    join :: c l -> a -> a -> a

-- | A generic fixpoint solver for forward data flow analysis. This function
-- iteratively applies the transfer function to each node in the CFG until
-- the data flow facts no longer change. It uses a worklist algorithm for
-- efficiency, and returns the final CFG along with any new work discovered.
fixpoint :: forall c l a. (DataFlow c l a, Show l, Ord l) => c l -> l -> CFG l a -> (CFG l a, Set (l, [Int]))
fixpoint ctx funcName cfg =
    let
        worklist = fromList (Map.keys cfg)
    in
        go worklist cfg Set.empty
    where
        go :: Worklist Int -> CFG l a -> Set (l, [Int]) -> (CFG l a, Set (l, [Int]))
        go worklist cfg' accumulatedWork
            | Just (currentId, worklist') <- pop worklist =
                let
                    node = lookupOrError "fixpoint" cfg' currentId
                    predNodes = mapMaybe (`Map.lookup` cfg') (cfgPreds node)
                    inFacts' = if null predNodes
                                  then cfgInFacts node
                                  else foldl1 (join ctx) (map cfgOutFacts predNodes)

                    (outFacts', blockWork) =
                        foldl'
                            (\(accFacts, accWork) stmt ->
                                let (newFacts, newWork) = transfer ctx funcName (dtrace ("fixpoint fold: accFacts=" <> show accFacts) accFacts) stmt
                                in (newFacts, Set.union accWork newWork))
                            (inFacts', Set.empty)
                            (cfgStmts node)

                    outFactsChanged = outFacts' /= cfgOutFacts node
                    cfg'' = dtrace (unlines [ "fixpoint (" <> show funcName <> ", node " <> show currentId <> "):"
                                            , "  inFacts': " <> groom inFacts'
                                            , "  outFacts': " <> groom outFacts'
                                            , "  old outFacts: " <> groom (cfgOutFacts node)
                                            , "  outFactsChanged: " <> show outFactsChanged
                                            ]) $ Map.insert currentId (node { cfgInFacts = inFacts', cfgOutFacts = outFacts' }) cfg'
                    worklist'' = if outFactsChanged
                        then pushList (cfgSuccs node) worklist'
                        else worklist'
                    accumulatedWork' = Set.union accumulatedWork blockWork
                in
                    go worklist'' cfg'' accumulatedWork'
            | otherwise = (cfg', accumulatedWork)

data BuilderState c l a = BuilderState
    { bsCtx        :: c l
    , bsStmts      :: [C.Node (C.Lexeme l)]
    , bsCfg        :: CFG l a
    , bsLabels     :: Map l Int
    , bsNextNodeId :: Int
    , bsExitNodeId :: Int
    , bsBreaks     :: [Int]
    , bsContinues  :: [Int]
    }

-- | Build a control flow graph for a function definition. This is the main
-- entry point for constructing a CFG from a Cimple AST.
buildCFG :: forall c l a. (DataFlow c l a, Pretty l, Ord l, Show l) => c l -> C.Node (C.Lexeme l) -> a -> CFG l a
buildCFG ctx (Fix (C.FunctionDefn _ (Fix (C.FunctionPrototype _ (C.L _ _ funcName) _)) body)) facts =
    buildCFG' ctx funcName body facts
buildCFG _ _ _ = Map.empty

buildCFG' :: forall c l a. (DataFlow c l a, Pretty l, Ord l, Show l) => c l -> l -> C.Node (C.Lexeme l) -> a -> CFG l a
buildCFG' ctx funcName (Fix (C.CompoundStmt stmts)) facts =
    let
        (labelMap, maxNodeId) = buildLabelMap stmts 1
        exitNodeId = maxNodeId + 2
        exitNode = CFGNode exitNodeId [] [] [] (emptyFacts ctx) (emptyFacts ctx)
        labelNodes = Map.fromList $ map (\(_, nodeId) -> (nodeId, CFGNode nodeId [] [] [] (emptyFacts ctx) (emptyFacts ctx))) $ Map.toList labelMap
        (outFacts, _) = foldl'
            (\(accFacts, _) stmt ->
                let (newFacts, _) = transfer ctx funcName accFacts stmt
                in (newFacts, Set.empty))
            (facts, Set.empty)
            []
        initialCfg = Map.insert exitNodeId exitNode $ Map.union labelNodes $ Map.singleton 0 (CFGNode 0 [] [] [] facts outFacts)
        initialState = BuilderState
            { bsCtx = ctx
            , bsStmts = []
            , bsCfg = initialCfg
            , bsLabels = labelMap
            , bsNextNodeId = exitNodeId + 1
            , bsExitNodeId = exitNodeId
            , bsBreaks = []
            , bsContinues = []
            }
        (lastNodeId, finalState) = runState (buildStmts stmts 0) initialState
        cfg = bsCfg finalState

        -- Connect the last node to the exit node if it's a fallthrough.
        lastNode = lookupOrError "buildCFG" cfg lastNodeId
        intermediateCfg = if null (cfgSuccs lastNode) && (cfgNodeId lastNode == 0 || not (null (cfgPreds lastNode))) && cfgNodeId lastNode /= bsExitNodeId finalState then
            Map.adjust (\n -> n { cfgSuccs = [bsExitNodeId finalState] }) lastNodeId $
            Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [lastNodeId] }) (bsExitNodeId finalState) cfg
        else
            cfg

        -- Prune unreachable nodes
        reachable = go (Set.singleton 0) [0]
          where
            go visited [] = visited
            go visited (curr:rest) =
                let
                    node = lookupOrError "buildCFG" intermediateCfg curr
                    newSuccs = filter (`Set.notMember` visited) (cfgSuccs node)
                in
                    go (Set.union visited (Set.fromList newSuccs)) (rest ++ newSuccs)

        finalCfg = Map.filterWithKey (\k _ -> k `Set.member` reachable) intermediateCfg
    in
        dtrace ("\n--- CFG for " <> show funcName <> " ---\n" <> show (fmap (\n -> (cfgNodeId n, cfgPreds n, cfgSuccs n, map showNodePlain (cfgStmts n))) finalCfg)) finalCfg
buildCFG' _ _ _ _ = Map.empty



getCompoundStmts :: C.Node (C.Lexeme l) -> [C.Node (C.Lexeme l)]
getCompoundStmts (Fix (C.CompoundStmt stmts)) = stmts
getCompoundStmts stmt                         = [stmt]

buildLabelMap :: Ord t => [C.Node (C.Lexeme t)] -> Int -> (Map t Int, Int)
buildLabelMap stmts startId =
    foldl' go (Map.empty, startId) stmts
    where
        go (acc, nodeId) (Fix (C.Label (C.L _ _ label) _)) = (Map.insert label nodeId acc, nodeId + 1)
        go (acc, nodeId) (Fix (C.IfStmt _ thenB mElseB)) =
            let (acc', nextId') = go (acc, nodeId + 1) thenB
                (acc'', nextId'') = case mElseB of
                    Just elseB -> go (acc', nextId' + 1) elseB
                    Nothing    -> (acc', nextId')
            in (acc'', nextId'' + 1)
        go (acc, nodeId) (Fix (C.WhileStmt _ body)) =
            let (acc', nextId') = go (acc, nodeId + 1) body
            in (acc', nextId' + 1)
        go (acc, nodeId) (Fix (C.ForStmt _ _ _ body)) =
            let (acc', nextId') = go (acc, nodeId + 1) body
            in (acc', nextId' + 1)
        go (acc, nodeId) (Fix (C.DoWhileStmt body _)) =
            let (acc', nextId') = go (acc, nodeId + 1) body
            in (acc', nextId' + 1)
        go (acc, nodeId) (Fix (C.SwitchStmt _ body)) =
            let (acc', nextId') = foldl' (\(a, n) s -> go (a, n) s) (acc, nodeId + 1) body
            in (acc', nextId' + length body + 1)
        go (acc, nodeId) (Fix (C.CompoundStmt stmts')) =
            foldl' go (acc, nodeId) stmts'
        go (acc, nodeId) _ = (acc, nodeId)

buildStmts :: (DataFlow c l a, Pretty l, Ord l, Show l) => [C.Node (C.Lexeme l)] -> Int -> State (BuilderState c l a) Int
buildStmts stmts currNodeId = foldM buildStmt currNodeId stmts

newDisconnectedNode :: (DataFlow c l a) => State (BuilderState c l a) Int
newDisconnectedNode = do
    st <- get
    let newNodeId = bsNextNodeId st
    let newNode = CFGNode newNodeId [] [] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
    put $ st { bsCfg = Map.insert newNodeId newNode (bsCfg st), bsNextNodeId = newNodeId + 1 }
    return newNodeId

buildStmt :: forall c l a. (DataFlow c l a, Pretty l, Ord l, Show l) => Int -> C.Node (C.Lexeme l) -> State (BuilderState c l a) Int
buildStmt currNodeId stmt@(Fix s') = dtrace ("buildStmt processing: " <> T.unpack (showNodePlain stmt)) $ case s' of
    C.CompoundStmt stmts' -> buildStmts stmts' currNodeId
    C.Label (C.L _ _ label) innerStmt -> do
        st <- get
        let labelNodeId = fromMaybe (error $ "Label not found: " ++ show label) (Map.lookup label (bsLabels st))
        let currentNode = lookupOrError "buildStmt Label" (bsCfg st) currNodeId
        if (not (null (cfgPreds currentNode)) || currNodeId == 0) && null (cfgSuccs currentNode) then do
            let cfg' = Map.adjust (\n -> n { cfgSuccs = cfgSuccs n ++ [labelNodeId] }) currNodeId (bsCfg st)
            let cfg'' = Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [currNodeId] }) labelNodeId cfg'
            put $ st { bsCfg = cfg'' }
        else
            return ()
        buildStmt labelNodeId innerStmt
    C.Goto (C.L _ _ label) -> do
        st <- get
        let labelNodeId = fromMaybe (error $ "Label not found: " ++ show label) (Map.lookup label (bsLabels st))
        let updatedCfg = Map.adjust (\n -> n { cfgSuccs = [labelNodeId] }) currNodeId (bsCfg st)
        let cfgWithPred = Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [currNodeId] }) labelNodeId updatedCfg
        put $ st { bsCfg = cfgWithPred }
        newDisconnectedNode
    C.IfStmt cond thenB mElseB -> do
        modify $ \st -> st { bsCfg = Map.adjust (\n -> n { cfgStmts = cfgStmts n ++ [cond] }) currNodeId (bsCfg st) }
        st <- get
        let thenNodeId = bsNextNodeId st
        case mElseB of
            Just elseB -> do
                let elseNodeId = thenNodeId + 1
                let mergeNodeId = elseNodeId + 1
                let thenNode = CFGNode thenNodeId [currNodeId] [] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
                let elseNode = CFGNode elseNodeId [currNodeId] [] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
                let mergeNode = CFGNode mergeNodeId [] [] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
                let updatedCfg = Map.insert thenNodeId thenNode $ Map.insert elseNodeId elseNode $ Map.insert mergeNodeId mergeNode (bsCfg st)
                let cfgWithSuccs = Map.adjust (\n -> n { cfgSuccs = [thenNodeId, elseNodeId] }) currNodeId updatedCfg
                put $ st { bsCfg = cfgWithSuccs, bsNextNodeId = mergeNodeId + 1 }
                lastThenNodeId <- buildStmts (getCompoundStmts thenB) thenNodeId
                lastElseNodeId <- buildStmts (getCompoundStmts elseB) elseNodeId
                st' <- get
                let lastThenNode = lookupOrError "buildStmt IfStmt" (bsCfg st') lastThenNodeId
                let lastElseNode = lookupOrError "buildStmt IfStmt" (bsCfg st') lastElseNodeId
                let cfgWithThen = if null (cfgSuccs lastThenNode)
                                  then Map.adjust (\n -> n { cfgSuccs = [mergeNodeId] }) lastThenNodeId (bsCfg st')
                                  else bsCfg st'
                let cfgWithElse = if null (cfgSuccs lastElseNode)
                                  then Map.adjust (\n -> n { cfgSuccs = [mergeNodeId] }) lastElseNodeId cfgWithThen
                                  else cfgWithThen
                let predNodes = (if null (cfgSuccs lastThenNode) then [lastThenNodeId] else []) ++
                                (if null (cfgSuccs lastElseNode) then [lastElseNodeId] else [])
                let finalCfg = Map.adjust (\n -> n { cfgPreds = predNodes }) mergeNodeId cfgWithElse
                put $ st' { bsCfg = finalCfg }
                return mergeNodeId
            Nothing -> do
                let mergeNodeId = thenNodeId + 1
                let thenNode = CFGNode thenNodeId [currNodeId] [] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
                let mergeNode = CFGNode mergeNodeId [currNodeId] [] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
                let updatedCfg = Map.insert thenNodeId thenNode $ Map.insert mergeNodeId mergeNode (bsCfg st)
                let cfgWithSuccs = Map.adjust (\n -> n { cfgSuccs = [thenNodeId, mergeNodeId] }) currNodeId updatedCfg
                put $ st { bsCfg = cfgWithSuccs, bsNextNodeId = mergeNodeId + 1 }
                lastThenNodeId <- buildStmts (getCompoundStmts thenB) thenNodeId
                st' <- get
                let lastThenNode = lookupOrError "buildStmt IfStmt" (bsCfg st') lastThenNodeId
                let finalCfg = if null (cfgSuccs lastThenNode)
                               then Map.adjust (\n -> n { cfgSuccs = [mergeNodeId] }) lastThenNodeId $ Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [lastThenNodeId] }) mergeNodeId (bsCfg st')
                               else bsCfg st'
                put $ st' { bsCfg = finalCfg }
                return mergeNodeId
    C.PreprocIfdef _ thenStmts (Fix (C.PreprocElse elseStmts)) -> do
        st <- get
        let thenNodeId = bsNextNodeId st
        let elseNodeId = thenNodeId + 1
        let mergeNodeId = elseNodeId + 1
        let thenNode = CFGNode thenNodeId [currNodeId] [] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
        let elseNode = CFGNode elseNodeId [currNodeId] [] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
        let mergeNode = CFGNode mergeNodeId [] [] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
        let updatedCfg = Map.insert thenNodeId thenNode $ Map.insert elseNodeId elseNode $ Map.insert mergeNodeId mergeNode (bsCfg st)
        let cfgWithSuccs = Map.adjust (\n -> n { cfgSuccs = [thenNodeId, elseNodeId] }) currNodeId updatedCfg
        put $ st { bsCfg = cfgWithSuccs, bsNextNodeId = mergeNodeId + 1 }
        lastThenNodeId <- buildStmts thenStmts thenNodeId
        lastElseNodeId <- buildStmts elseStmts elseNodeId
        st' <- get
        let lastThenNode = lookupOrError "buildStmt PreprocIfdef" (bsCfg st') lastThenNodeId
        let lastElseNode = lookupOrError "buildStmt PreprocIfdef" (bsCfg st') lastElseNodeId
        let cfgWithThen = if null (cfgSuccs lastThenNode)
                          then Map.adjust (\n -> n { cfgSuccs = [mergeNodeId] }) lastThenNodeId (bsCfg st')
                          else bsCfg st'
        let cfgWithElse = if null (cfgSuccs lastElseNode)
                          then Map.adjust (\n -> n { cfgSuccs = [mergeNodeId] }) lastElseNodeId cfgWithThen
                          else cfgWithThen
        let predNodes = (if null (cfgSuccs lastThenNode) then [lastThenNodeId] else []) ++
                        (if null (cfgSuccs lastElseNode) then [lastElseNodeId] else [])
        let finalCfg = Map.adjust (\n -> n { cfgPreds = predNodes }) mergeNodeId cfgWithElse
        put $ st' { bsCfg = finalCfg }
        return mergeNodeId
    C.WhileStmt cond body -> do
        st <- get
        let condNodeId = bsNextNodeId st
        let bodyNodeId = condNodeId + 1
        let loopExitNodeId = bodyNodeId + 1

        let condNode = CFGNode condNodeId [] [bodyNodeId, loopExitNodeId] [cond] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
        let bodyNode = CFGNode bodyNodeId [condNodeId] [] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
        let loopExitNode = CFGNode loopExitNodeId [condNodeId] [] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))

        let updatedCfg = Map.insert condNodeId condNode $ Map.insert bodyNodeId bodyNode $ Map.insert loopExitNodeId loopExitNode (bsCfg st)
        let cfgWithSuccs = Map.adjust (\n -> n { cfgSuccs = cfgSuccs n ++ [condNodeId] }) currNodeId updatedCfg
        let cfgWithPreds = Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [currNodeId] }) condNodeId cfgWithSuccs

        put $ st { bsCfg = cfgWithPreds, bsNextNodeId = loopExitNodeId + 1, bsBreaks = loopExitNodeId : bsBreaks st, bsContinues = condNodeId : bsContinues st }

        lastBodyNodeId <- buildStmts (getCompoundStmts body) bodyNodeId

        st' <- get

        let lastBodyNode = lookupOrError "buildStmt WhileStmt" (bsCfg st') lastBodyNodeId
        let finalCfg = if null (cfgSuccs lastBodyNode) then
                         Map.adjust (\n -> n { cfgSuccs = [condNodeId] }) lastBodyNodeId $
                         Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [lastBodyNodeId] }) condNodeId (bsCfg st')
                       else
                         bsCfg st'
        put $ st' { bsCfg = finalCfg, bsBreaks = bsBreaks st, bsContinues = bsContinues st }
        return loopExitNodeId
    C.ForStmt init' cond inc body -> do
        initNodeId <- buildStmt currNodeId init'
        st <- get
        let condNodeId = bsNextNodeId st
        let bodyNodeId = condNodeId + 1
        let incNodeId = bodyNodeId + 1
        let exitNodeId' = incNodeId + 1

        let condNode = CFGNode condNodeId [] [bodyNodeId, exitNodeId'] [cond] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
        let bodyNode = CFGNode bodyNodeId [condNodeId] [incNodeId] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
        let incNode = CFGNode incNodeId [bodyNodeId] [condNodeId] [inc] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
        let exitNode' = CFGNode exitNodeId' [condNodeId] [] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))

        let updatedCfg = Map.insert condNodeId condNode $
                         Map.insert bodyNodeId bodyNode $
                         Map.insert incNodeId incNode $
                         Map.insert exitNodeId' exitNode' (bsCfg st)

        let cfgWithSuccs = Map.adjust (\n -> n { cfgSuccs = cfgSuccs n ++ [condNodeId] }) initNodeId updatedCfg
        let cfgWithPreds = Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [initNodeId, incNodeId] }) condNodeId cfgWithSuccs

        put $ st { bsCfg = cfgWithPreds, bsNextNodeId = exitNodeId' + 1, bsBreaks = exitNodeId' : bsBreaks st, bsContinues = incNodeId : bsContinues st }

        lastBodyNodeId <- buildStmts (getCompoundStmts body) bodyNodeId

        st' <- get
        let lastBodyNode = lookupOrError "buildStmt ForStmt" (bsCfg st') lastBodyNodeId
        let finalCfg = if null (cfgSuccs lastBodyNode) then
                         Map.adjust (\n -> n { cfgSuccs = [incNodeId] }) lastBodyNodeId $
                         Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [lastBodyNodeId] }) incNodeId (bsCfg st')
                       else
                         bsCfg st'

        put $ st' { bsCfg = finalCfg, bsBreaks = bsBreaks st, bsContinues = bsContinues st }
        return exitNodeId'
    C.DoWhileStmt body cond -> do
        st <- get
        let bodyNodeId = bsNextNodeId st
        let condNodeId = bodyNodeId + 1
        let exitNodeId' = condNodeId + 1

        let bodyNode = CFGNode bodyNodeId [] [condNodeId] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
        let condNode = CFGNode condNodeId [bodyNodeId] [bodyNodeId, exitNodeId'] [cond] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
        let exitNode = CFGNode exitNodeId' [condNodeId] [] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))

        let updatedCfg = Map.insert bodyNodeId bodyNode $ Map.insert condNodeId condNode $ Map.insert exitNodeId' exitNode (bsCfg st)
        let cfgWithSuccs = Map.adjust (\n -> n { cfgSuccs = [bodyNodeId] }) currNodeId updatedCfg
        let cfgWithPreds = Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [currNodeId, condNodeId] }) bodyNodeId cfgWithSuccs
        put $ st { bsCfg = cfgWithPreds, bsNextNodeId = exitNodeId' + 1, bsBreaks = exitNodeId' : bsBreaks st, bsContinues = condNodeId : bsContinues st }

        lastBodyNodeId <- buildStmts (getCompoundStmts body) bodyNodeId

        st' <- get
        let lastBodyNode = lookupOrError "buildStmt DoWhileStmt" (bsCfg st') lastBodyNodeId
        let finalCfg = if null (cfgSuccs lastBodyNode) then
                         Map.adjust (\n -> n { cfgSuccs = [condNodeId] }) lastBodyNodeId $
                         Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [lastBodyNodeId] }) condNodeId (bsCfg st')
                       else
                         bsCfg st'

        put $ st' { bsCfg = finalCfg, bsBreaks = bsBreaks st, bsContinues = bsContinues st }
        return exitNodeId'
    C.SwitchStmt cond body -> do
        st <- get
        let switchExitNodeId = bsNextNodeId st
        let switchExitNode = CFGNode switchExitNodeId [] [] [] (emptyFacts (bsCtx st)) (emptyFacts (bsCtx st))
        let cfg' = Map.insert switchExitNodeId switchExitNode (bsCfg st)
        put $ st { bsCfg = cfg', bsNextNodeId = switchExitNodeId + 1, bsBreaks = switchExitNodeId : bsBreaks st }

        let flattenCases stmts = concatMap (\case
                (Fix (C.Case caseCond (Fix (C.CompoundStmt bodyStmts)))) -> [(Just caseCond, bodyStmts)]
                (Fix (C.Case _ stmt')) -> flattenCases [stmt']
                (Fix (C.Default (Fix (C.CompoundStmt bodyStmts)))) -> [(Nothing, bodyStmts)]
                (Fix (C.Default stmt')) -> flattenCases [stmt']
                _ -> []) stmts

        let caseBlocks = flattenCases body

        (caseNodeIds, stmts') <- fmap unzip $ forM caseBlocks $ \(_, stmts) -> do
            st_b <- get
            let caseId = bsNextNodeId st_b
            let node = CFGNode caseId [] [] [] (emptyFacts (bsCtx st_b)) (emptyFacts (bsCtx st_b))
            put $ st_b { bsCfg = Map.insert caseId node (bsCfg st_b), bsNextNodeId = bsNextNodeId st_b + 1 }
            return (caseId, stmts)

        -- The switch node is a predecessor to all cases.
        st_c <- get
        let cfg_c' = Map.adjust (\n -> n { cfgSuccs = cfgSuccs n ++ caseNodeIds, cfgStmts = cfgStmts n ++ [cond] }) currNodeId (bsCfg st_c)
        let cfg_c'' = foldl' (\c i -> Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [currNodeId] }) i c) cfg_c' caseNodeIds
        put $ st_c { bsCfg = cfg_c'' }

        -- Process each case.
        let cases = zip caseNodeIds stmts'
        let casesWithFallthrough = zip cases (tail (map (Just . fst) cases) ++ [Nothing])
        unbrokenEndNodes <- fmap concat $ forM casesWithFallthrough $ \((caseNodeId, caseStmts), mNextCaseId) -> do
            endNodeId <- buildStmts caseStmts caseNodeId
            st_after <- get
            let endNode = lookupOrError "buildStmt SwitchStmt" (bsCfg st_after) endNodeId

            if null (cfgSuccs endNode) then
                case mNextCaseId of
                    Just nextId -> do
                        st_f <- get
                        let cfg_f' = Map.adjust (\n -> n { cfgSuccs = [nextId] }) endNodeId (bsCfg st_f)
                        let cfg_f'' = Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [endNodeId] }) nextId cfg_f'
                        put $ st_f { bsCfg = cfg_f'' }
                        return []
                    Nothing -> return [endNodeId]
            else return []

        -- Connect unbroken ends to the exit node.
        st_d <- get
        let cfg_d' = foldl' (\c p -> Map.adjust (\n -> n { cfgSuccs = cfgSuccs n ++ [switchExitNodeId] }) p c) (bsCfg st_d) unbrokenEndNodes
        let cfg_d'' = Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ unbrokenEndNodes }) switchExitNodeId cfg_d'

        -- Also connect switch to exit for default case not being present
        let hasDefault = any (\case (Nothing, _) -> True; _ -> False) caseBlocks
        let cfg_d''' = if hasDefault
                       then cfg_d''
                       else Map.adjust (\n -> n { cfgSuccs = cfgSuccs n ++ [switchExitNodeId] }) currNodeId cfg_d''
        let cfg_d'''' = if hasDefault
                        then cfg_d'''
                        else Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [currNodeId] }) switchExitNodeId cfg_d'''

        put $ st_d { bsCfg = cfg_d'''', bsBreaks = bsBreaks st, bsContinues = bsContinues st }
        return switchExitNodeId
    C.Return _ -> do
        st <- get
        let cfgWithStmt = Map.adjust (\n -> n { cfgStmts = cfgStmts n ++ [stmt] }) currNodeId (bsCfg st)
        let updatedCfg = Map.adjust (\n -> n { cfgSuccs = [bsExitNodeId st] }) currNodeId cfgWithStmt
        let cfgWithPred = Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [currNodeId] }) (bsExitNodeId st) updatedCfg
        put $ st { bsCfg = cfgWithPred }
        newDisconnectedNode
    C.Break -> do
        st <- get
        let target = head (bsBreaks st)
        let cfgWithStmt = Map.adjust (\n -> n { cfgStmts = cfgStmts n ++ [stmt] }) currNodeId (bsCfg st)
        let updatedCfg = Map.adjust (\n -> n { cfgSuccs = [target] }) currNodeId cfgWithStmt
        let cfgWithPred = Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [currNodeId] }) target updatedCfg
        put $ st { bsCfg = cfgWithPred }
        newDisconnectedNode
    C.Continue -> do
        st <- get
        let target = head (bsContinues st)
        let cfgWithStmt = Map.adjust (\n -> n { cfgStmts = cfgStmts n ++ [stmt] }) currNodeId (bsCfg st)
        let updatedCfg = Map.adjust (\n -> n { cfgSuccs = [target] }) currNodeId cfgWithStmt
        let cfgWithPred = Map.adjust (\n -> n { cfgPreds = cfgPreds n ++ [currNodeId] }) target updatedCfg
        put $ st { bsCfg = cfgWithPred }
        newDisconnectedNode
    C.PreprocDefineMacro {} -> do
        st <- get
        let updatedCfg = Map.adjust (\n -> n { cfgStmts = cfgStmts n ++ [stmt] }) currNodeId (bsCfg st)
        put $ st { bsCfg = updatedCfg }
        return currNodeId
    C.PreprocUndef {} -> do
        st <- get
        let updatedCfg = Map.adjust (\n -> n { cfgStmts = cfgStmts n ++ [stmt] }) currNodeId (bsCfg st)
        put $ st { bsCfg = updatedCfg }
        return currNodeId
    C.PreprocScopedDefine def stmts' undef -> do
        currNodeId' <- buildStmt currNodeId def
        currNodeId'' <- buildStmts stmts' currNodeId'
        buildStmt currNodeId'' undef
    _ -> do
        st <- get
        let updatedCfg = Map.adjust (\n -> n { cfgStmts = cfgStmts n ++ [stmt] }) currNodeId (bsCfg st)
        put $ st { bsCfg = updatedCfg }
        return currNodeId
