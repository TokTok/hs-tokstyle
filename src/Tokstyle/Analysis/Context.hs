module Tokstyle.Analysis.Context
    ( pushContext
    , kLimit
    ) where

import           Tokstyle.Analysis.Types (Context, NodeId)

-- | Adds a new call site to a context, respecting the k-limit.
-- Prepends the new node ID and then takes the first k elements.
pushContext :: Int -> NodeId -> Context -> Context
pushContext k nodeId context = take k (nodeId : context)

kLimit :: Int
kLimit = 2
