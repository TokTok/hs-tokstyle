{-# LANGUAGE Strict #-}
-- | What the analysis knows about functions whose bodies it doesn't see.
module Tokstyle.C.Analysis.Known
    ( Reach (..)
    , libraryReaches
    , allocators
    , noReturn
    , harmless
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


-- | How far a function can reach into the memory one of its pointer
-- parameters points to: another parameter times a coefficient, if any, plus
-- a constant.
data Reach = Reach
    { reachParam  :: Int
    , reachLength :: Maybe (Int, Integer)
    , reachConst  :: Integer
    }
    deriving (Eq, Show)

-- | How far the library functions the program calls reach into the memory
-- their pointer arguments point to.
libraryReaches :: Map String [Reach]
libraryReaches = Map.fromList
    [ ("memcpy", [upTo 0 2, upTo 1 2])
    , ("memmove", [upTo 0 2, upTo 1 2])
    , ("memcmp", [upTo 0 2, upTo 1 2])
    , ("memset", [upTo 0 2])
    , ("snprintf", [upTo 0 1])
    , ("sodium_memzero", [upTo 0 1])
    , ("sodium_memcmp", [upTo 0 2, upTo 1 2])
    , ("randombytes_buf", [upTo 0 1])
    , ("crypto_hash_sha256", [Reach 0 Nothing 32, upTo 1 2])
    , ("crypto_hash_sha512", [Reach 0 Nothing 64, upTo 1 2])
    , ("crypto_auth", [Reach 0 Nothing 32, upTo 1 2])
    , ("crypto_auth_verify", [upTo 1 2])
    , ("crypto_sign_detached", [Reach 0 Nothing 64, upTo 2 3])
    , ("crypto_sign_verify_detached", [upTo 1 2])
    , ("crypto_box_afternm", [upTo 0 2, upTo 1 2])
    , ("crypto_box_open_afternm", [upTo 0 2, upTo 1 2])
    ]
  where
    upTo p n = Reach p (Just (n, 1)) 0

-- | The functions that allocate memory: how many arguments they take, and
-- which of them multiply to the size in bytes.
allocators :: Map String (Int, [Int])
allocators = Map.fromList
    [ ("alloca", (1, [0]))
    , ("malloc", (1, [0]))
    , ("calloc", (2, [0, 1]))
    , ("realloc", (2, [1]))
    , ("mem_balloc", (2, [1]))
    , ("mem_alloc", (2, [1]))
    , ("mem_valloc", (3, [1, 2]))
    , ("mem_brealloc", (3, [2]))
    , ("mem_vrealloc", (4, [2, 3]))
    ]

noReturn :: [String]
noReturn = ["__assert_fail", "abort", "exit", "_Exit", "logger_abort"]

-- | External functions that don't change the objects their pointers point to
-- in ways the analysis needs to see.
harmless :: [String]
harmless = ["free", "memset", "memcpy", "memmove", "crypto_memzero", "sodium_memzero"]
