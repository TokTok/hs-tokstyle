{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Analysis.SecurityRank.Lattice
    ( SecurityRank(..)
    , mergeRank
    , TaintState
    ) where

import           Data.Map.Strict         (Map)
import           Data.Text               (Text)
import           Tokstyle.Analysis.Types (AbstractLocation)

-- | Represents the security level of data.
-- The Ord instance defines the lattice structure, where the minimum element
-- is the most restrictive/tainted.
-- Bottom < Safe < Rank 0 < Rank 1 < ...
data SecurityRank
    = Bottom   -- ^ Uninitialized or unreachable code path.
    | Safe     -- ^ A value known to be safe (e.g., a literal).
    | Rank Int -- ^ A tainted value with a specific integer rank.
    deriving (Eq, Show)

-- | The ordering defines the lattice. A lower value is "more tainted".
instance Ord SecurityRank where
    compare Bottom      Bottom   = EQ
    compare Bottom      _        = LT
    compare _           Bottom   = GT
    compare Safe        Safe     = EQ
    compare Safe        _        = GT
    compare _           Safe     = LT
    compare (Rank a)    (Rank b) = compare a b

-- | The merge operation for the lattice. When control paths join,
-- we take the most restrictive (i.e., minimum) rank.
mergeRank :: SecurityRank -> SecurityRank -> SecurityRank
mergeRank = min

-- | The TaintState is the fact that flows through the analysis. It maps every
-- abstract location currently in scope to its security rank.
type TaintState = Map AbstractLocation SecurityRank
