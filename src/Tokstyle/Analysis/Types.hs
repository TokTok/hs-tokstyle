{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Analysis.Types
    ( CallGraph
    , CallSite(..)
    , CallType(..)
    , FunctionName
    , CalleeMap
    , PointsToMap
    , PointsToSummary
    , PointsToSummaryData(..)
    , getCallers
    , AbstractLocation(..)
    , toAbstractLocation
    , NodeId
    , Context
    , lookupOrError
    ) where

import           Data.Fix        (Fix (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           GHC.Stack       (HasCallStack)
import qualified Language.Cimple as C

-- | Represents a location where a value can be stored. This allows the
-- analysis to distinguish between different variables and fields.
data AbstractLocation
    = VarLocation Text          -- ^ A local variable or parameter name.
    | GlobalVarLocation Text    -- ^ A global or static variable.
    | FieldLocation AbstractLocation Text -- ^ A struct/union field, e.g., msg.data
    | DerefLocation AbstractLocation    -- ^ The memory pointed to by a pointer, e.g., *p
    | ReturnLocation Text       -- ^ The return value of a function.
    | HeapLocation Int          -- ^ An abstract location on the heap.
    | FunctionLocation Text     -- ^ The address of a function.
    deriving (Eq, Ord, Show)

-- | A unique identifier for a C AST node.
type NodeId = Int

-- | The call-string context, limited to depth k.
type Context = [NodeId]

-- | A function name is just Text.
type FunctionName = Text

-- | Describes how a function is called.
data CallType = DirectCall | IndirectCall
    deriving (Eq, Ord, Show)

-- | A new, richer representation of a call site.
data CallSite = CallSite
    { csNodeId   :: NodeId   -- The unique ID of the call expression node.
    , csCallType :: CallType -- Direct or Indirect
    } deriving (Eq, Ord, Show)

-- | A map from a callee's name to the set of ways it's called.
type CalleeMap = Map FunctionName (Set CallSite)

-- | The CallGraph is a map from a caller function to its CalleeMap.
type CallGraph = Map FunctionName CalleeMap

-- | The PointsToMap is the data flow fact. It maps a pointer's abstract
-- location to the set of abstract locations it can point to.
type PointsToMap = Map AbstractLocation (Set AbstractLocation)

-- | The summary for a function's points-to analysis in a specific context.
data PointsToSummaryData = PointsToSummaryData
    { returnPointsTo :: Set AbstractLocation
    , outputPointsTo :: Map AbstractLocation (Set AbstractLocation)
    } deriving (Eq, Ord, Show)

-- | The full, context-sensitive points-to summary for a function.
type PointsToSummary = Map Context PointsToSummaryData

-- | Helper function to get all functions that call a given function.
getCallers :: CallGraph -> FunctionName -> Map FunctionName (Set CallSite)
getCallers graph calleeName =
    Map.foldlWithKey' (\acc caller calleeMap ->
        case Map.lookup calleeName calleeMap of
            Just directions -> Map.insert caller directions acc
            Nothing         -> acc
    ) Map.empty graph

-- | Helper to convert an LHS expression AST node to an AbstractLocation.
toAbstractLocation :: HasCallStack => C.Node (C.Lexeme Text) -> AbstractLocation
toAbstractLocation (Fix node) = case node of
    C.VarExpr (C.L _ _ name) -> VarLocation name
    C.MemberAccess struct (C.L _ _ field) ->
        FieldLocation (toAbstractLocation struct) field
    C.PointerAccess ptr (C.L _ _ field) ->
        FieldLocation (DerefLocation (toAbstractLocation ptr)) field
    C.UnaryExpr C.UopDeref ptr ->
        DerefLocation (toAbstractLocation ptr)
    C.UnaryExpr C.UopAddress inner ->
        toAbstractLocation inner
    C.CastExpr _ inner ->
        toAbstractLocation inner
    C.ArrayAccess array (Fix (C.LiteralExpr C.Int (C.L _ _ index))) ->
        FieldLocation (toAbstractLocation array) (Text.pack ("_index_" ++ Text.unpack index))
    C.ArrayAccess array _ -> toAbstractLocation array -- Fallback for non-constant index
    C.VarDecl _ (C.L _ _ name) _ -> VarLocation name
    C.LiteralExpr C.ConstId (C.L _ _ name) -> VarLocation name
    _ -> error $ "toAbstractLocation: Unhandled LHS node: " ++ show node

-- | A safer version of 'Map.!'.
lookupOrError :: (Ord k, Show k) => String -> Map k a -> k -> a
lookupOrError context m k = fromMaybe (error $ context ++ ": Key not found in map: " ++ show k) (Map.lookup k m)
