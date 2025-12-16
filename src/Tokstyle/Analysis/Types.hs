{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Analysis.Types
    ( FunctionName
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

-- | A unique identifier for a C AST node.
type NodeId = Int

-- | The call-string context, limited to depth k.
type Context = [NodeId]

-- | A function name is just Text.
type FunctionName = Text

-- | A safer version of 'Map.!'.
lookupOrError :: (Ord k, Show k) => String -> Map k a -> k -> a
lookupOrError context m k = fromMaybe (error $ context ++ ": Key not found in map: " ++ show k) (Map.lookup k m)
