{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Tokstyle.Analysis.PointsTo.Types
    ( MemLoc (..)
    , IMemLoc (..)
    , MemLocPool (..)
    , Context
    , FunctionSummary(..)
    , GlobalEnv(..)
    , PointsToFact(..)
    , RelevantInputState(..)
    , PointsToContext(..)
    , PointsToAnalysis
    , intern
    ) where

import           Control.Monad.State.Strict (State, get, put)
import           Data.Hashable              (Hashable)
import           Data.IntMap.Strict         (IntMap)
import qualified Data.IntMap.Strict         as IntMap
import           Data.IntSet                (IntSet)
import qualified Data.IntSet                as IntSet
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import qualified Language.Cimple            as C
import           Tokstyle.Analysis.Scope    (ScopedId)
import           Tokstyle.Analysis.VTable   (VTableMap)
import           Tokstyle.Common.TypeSystem (TypeSystem)

data MemLoc
  = StackLoc { locId :: ScopedId }
  | HeapLoc { allocSite :: Text } -- "filepath:line:col"
  | GlobalVarLoc { locId :: ScopedId }
  | ExternalParamLoc { funcName :: Text, paramName :: Text }
  | FieldLoc { baseLoc :: MemLoc, fieldName :: Text }
  | NullLoc
  | UnknownLoc
  deriving (Eq, Ord, Show, Generic)

instance Hashable MemLoc

-- | Interned Memory Location (just an Int for efficiency).
newtype IMemLoc = IMemLoc { unIMemLoc :: Int }
    deriving (Eq, Ord, Show, Hashable)

-- | A pool for interning MemLocs.
data MemLocPool = MemLocPool
    { nextMemLocId :: !Int
    , memLocToId   :: !(Map MemLoc IMemLoc)
    , idToMemLoc   :: !(IntMap MemLoc)
    , fieldIndex   :: !(IntMap IntSet) -- ^ Maps a base IMemLoc to its immediate fields' IMemLocs
    } deriving (Show)

type PointsToAnalysis = State MemLocPool

intern :: MemLoc -> PointsToAnalysis IMemLoc
intern loc = do
    pool <- get
    case Map.lookup loc (memLocToId pool) of
        Just iloc -> return iloc
        Nothing -> do
            -- If it's a FieldLoc, ensure base is interned first and get its ID.
            maybeBaseIloc <- case loc of
                FieldLoc base _ -> Just <$> intern base
                _               -> return Nothing

            -- Re-get pool as it might have changed during intern base
            pool' <- get
            let nextId = nextMemLocId pool'
            let iloc = IMemLoc nextId

            let newFieldIndex = case maybeBaseIloc of
                    Just baseIloc -> IntMap.insertWith IntSet.union (unIMemLoc baseIloc) (IntSet.singleton nextId) (fieldIndex pool')
                    Nothing       -> fieldIndex pool'

            put $ pool'
                { nextMemLocId = nextId + 1
                , memLocToId   = Map.insert loc iloc (memLocToId pool')
                , idToMemLoc   = IntMap.insert nextId loc (idToMemLoc pool')
                , fieldIndex   = newFieldIndex
                }
            return iloc

-- The call stack context.
type Context = [ScopedId]

data FunctionSummary = FunctionSummary
    { fsReturnValue  :: !IntSet -- ^ What the function can return (Set IMemLoc).
    , fsParamEffects :: !(Map Int IntSet) -- ^ Map from arg index to its final points-to set (Set IMemLoc).
    , fsMemEffects   :: !(IntMap IntSet) -- ^ Memory side effects (IMemLoc -> Set IMemLoc).
    } deriving (Show, Eq)

data PointsToFact = PointsToFact
    { varMap        :: !(Map ScopedId IntSet) -- ^ ScopedId -> Set IMemLoc
    , memMap        :: !(IntMap IntSet)       -- ^ IMemLoc -> Set IMemLoc
    , unknownWrites :: !IntSet                -- ^ Set IMemLoc
    } deriving (Show, Eq, Ord)

-- | Represents the subset of PointsToFact that is relevant for a function call.
-- This is used as the context key for memoization.
newtype RelevantInputState = RelevantInputState PointsToFact
    deriving (Show, Eq, Ord)

newtype GlobalEnv = GlobalEnv (Map (ScopedId, RelevantInputState) (FunctionSummary, PointsToFact))
    deriving (Show, Eq)

data PointsToContext l = PointsToContext
    { pcFilePath    :: FilePath
    , pcTypeSystem  :: TypeSystem
    , pcVTableMap   :: VTableMap
    , pcGlobalEnv   :: GlobalEnv
    , pcFuncs       :: Map ScopedId [C.Node (C.Lexeme ScopedId)]
    , pcCurrentFunc :: ScopedId
    , pcVarTypes    :: Map ScopedId (C.Node (C.Lexeme ScopedId))
    }
