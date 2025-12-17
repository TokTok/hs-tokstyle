{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}

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

import           Control.Monad           (foldM)
import           Data.Fix                (Fix (Fix, unFix))
import           Data.Foldable           (foldl')
import           Data.Kind               (Type)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (mapMaybe)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.String             (IsString)
import           Debug.Trace             (trace)
import           Language.Cimple         (NodeF (..))
import qualified Language.Cimple         as C
import           Language.Cimple.Pretty  (showNodePlain)
import           Prettyprinter           (Pretty (..))
import           Text.Groom              (groom)
import qualified Tokstyle.Analysis.CFG   as CFGBuilder
import           Tokstyle.Analysis.Types (lookupOrError)
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
class (Eq a, Show a, Monad m, Ord callCtx) => DataFlow m (c :: Type -> Type) l a callCtx | a -> l, a -> callCtx where
    -- | The facts for an empty basic block.
    emptyFacts :: c l -> m a
    -- | The transfer function defines how a single statement affects the
    -- data flow facts. It takes the facts before the statement and
    -- returns the facts after the statement, plus any new work discovered.
    transfer :: c l -> l -> Int -> a -> C.Node (C.Lexeme l) -> m (a, Set (l, callCtx))
    -- | The join operator combines facts from multiple predecessor nodes.
    -- This is used at control flow merge points (e.g., after an if-statement
    -- or at the start of a loop).
    join :: c l -> a -> a -> m a

-- | A generic fixpoint solver for forward data flow analysis. This function
-- iteratively applies the transfer function to each node in the CFG until
-- the data flow facts no longer change. It uses a worklist algorithm for
-- efficiency, and returns the final CFG along with any new work discovered.
fixpoint :: forall m c l a callCtx. (DataFlow m c l a callCtx, Show l, Ord l) => c l -> l -> CFG l a -> m (CFG l a, Set (l, callCtx))
fixpoint ctx funcName (cfg :: CFG l a) =
    let
        worklist = fromList (Map.keys cfg)
    in
        go worklist cfg Set.empty
    where
        go :: Worklist Int -> CFG l a -> Set (l, callCtx) -> m (CFG l a, Set (l, callCtx))
        go worklist cfg' accumulatedWork
            | Just (currentId, worklist') <- pop worklist = do
                let node = lookupOrError "fixpoint" cfg' currentId
                let predNodes = mapMaybe (`Map.lookup` cfg') (cfgPreds node)

                inFacts' <- if null predNodes
                                then return $ cfgInFacts node
                                else foldM (join ctx) (cfgOutFacts (head predNodes)) (map cfgOutFacts (tail predNodes))

                (outFacts', blockWork) <-
                    foldM
                        (\(accFacts, accWork) stmt -> do
                            (newFacts, newWork) <- transfer ctx funcName (cfgNodeId node) (dtrace ("fixpoint fold: accFacts=" <> show accFacts) accFacts) stmt
                            return (newFacts, Set.union accWork newWork))
                        (inFacts', Set.empty)
                        (cfgStmts node)

                let outFactsChanged = outFacts' /= cfgOutFacts node
                let cfg'' = dtrace (unlines [ "fixpoint (" <> show funcName <> ", node " <> show currentId <> "):"
                                            , "  inFacts': " <> groom inFacts'
                                            , "  outFacts': " <> groom outFacts'
                                            , "  old outFacts: " <> groom (cfgOutFacts node)
                                            , "  outFactsChanged: " <> show outFactsChanged
                                            ]) $ Map.insert currentId (node { cfgInFacts = inFacts', cfgOutFacts = outFacts' }) cfg'
                let worklist'' = if outFactsChanged
                        then pushList (cfgSuccs node) worklist'
                        else worklist'
                let accumulatedWork' = Set.union accumulatedWork blockWork
                go worklist'' cfg'' accumulatedWork'
            | otherwise = return (cfg', accumulatedWork)

-- | Build a control flow graph for a function definition. This is the main
-- entry point for constructing a CFG from a Cimple AST.
buildCFG :: forall m c l a callCtx. (DataFlow m c l a callCtx, Pretty l, Ord l, Show l, IsString l) => c l -> C.Node (C.Lexeme l) -> a -> m (CFG l a)
buildCFG ctx cNode@(Fix (C.FunctionDefn _ (Fix (C.FunctionPrototype _ (C.L _ _ funcName) _)) _)) initialFacts = do
    let structuralCFG = CFGBuilder.buildCFG cNode

    let addFacts :: Int -> CFGBuilder.CFGNode l -> m (CFGNode l a)
        addFacts nodeId structuralNode = do
            facts <- if nodeId == 0 then return initialFacts else emptyFacts ctx
            return $ CFGNode
                    { cfgNodeId   = CFGBuilder.cfgNodeId structuralNode
                    , cfgPreds    = CFGBuilder.cfgPreds structuralNode
                    , cfgSuccs    = CFGBuilder.cfgSuccs structuralNode
                    , cfgStmts    = CFGBuilder.cfgStmts structuralNode
                    , cfgInFacts  = facts
                    , cfgOutFacts = facts
                    }

    dfaCFG <- Map.traverseWithKey addFacts structuralCFG
    return $ dtrace ("\n--- CFG for " <> show funcName <> " ---\n" <> show (fmap (\n -> (cfgNodeId n, cfgPreds n, cfgSuccs n, map showNodePlain (cfgStmts n))) dfaCFG)) dfaCFG
buildCFG _ _ _ = return Map.empty
