module Tokstyle.Analysis.SecurityRank.Types where

import           Data.Map.Strict                        (Map)
import qualified Data.Map.Strict                        as Map
import           Tokstyle.Analysis.SecurityRank.Lattice (SecurityRank)
import           Tokstyle.Analysis.Types                (AbstractLocation,
                                                         Context)

-- | The summary for a function's security rank analysis in a specific context.
data SecurityRankSummaryData = SecurityRankSummaryData
    { -- | Maps an output location (return value or dereferenced pointer param)
      -- to the rank it will be tainted with.
      srsOutputRanks  :: Map AbstractLocation SecurityRank
      -- | Maps an input parameter's index to the rank it's expected to have (as a sink).
    , srsSinks        :: Map Int SecurityRank
    , srsdDiagnostics :: [String]
    } deriving (Show, Ord)

-- | The full, context-sensitive security rank summary for a function.
type SecurityRankSummary = Map Context SecurityRankSummaryData

-- Custom Eq instance to ignore diagnostics for fixpoint termination.
instance Eq SecurityRankSummaryData where
    a == b = srsOutputRanks a == srsOutputRanks b &&
             srsSinks a == srsSinks b

-- | An empty security rank summary data.
emptySecurityRankSummaryData :: SecurityRankSummaryData
emptySecurityRankSummaryData = SecurityRankSummaryData Map.empty Map.empty []
