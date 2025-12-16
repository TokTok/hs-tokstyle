{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Analysis.PointsTo.ExternalSummaries
    ( getExternalSummary
    , locFromPos
    ) where

import           Data.Fix                         (Fix (..))
import           Data.IntSet                      (IntSet)
import qualified Data.IntSet                      as IntSet
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Text                        (Text, pack)
import           Debug.Trace                      (traceShow)
import           Language.Cimple                  (AlexPosn (..), NodeF (..))
import qualified Language.Cimple                  as C
import           Tokstyle.Analysis.PointsTo.Types
import           Tokstyle.Analysis.Scope          (ScopedId (..))

-- A summary of an external function's behavior.
-- It takes the arguments and the current state, and returns the new state
-- and the set of memory locations the function's return value can point to.
type ExternalSummary = FilePath -> C.AlexPosn -> [C.Node (C.Lexeme ScopedId)] -> PointsToAnalysis (Set MemLoc, Bool)

-- Map from function names to their summaries.
summaries :: Map Text ExternalSummary
summaries = Map.fromList
    [ ("malloc", summaryMalloc)
    , ("calloc", summaryMalloc) -- Treat calloc like malloc for now
    , ("realloc", summaryRealloc)
    , ("free", summaryFree)
    -- keep-sorted start
    , ("GetAdaptersInfo", summaryNoOp)
    , ("WSAGetLastError", summaryNoOp)
    , ("assert", summaryNoOp)
    , ("bind", summaryNoOp)
    , ("closesocket", summaryNoOp)
    , ("close", summaryNoOp)
    , ("connect", summaryNoOp)
    , ("crypto_box_beforenm", summaryNoOp)
    , ("crypto_hash_sha256", summaryNoOp)
    , ("crypto_memzero", summaryNoOp)
    , ("crypto_pwhash_scryptsalsa208sha256", summaryNoOp)
    , ("crypto_sign_detached", summaryNoOp)
    , ("crypto_sign_ed25519_pk_to_curve25519", summaryNoOp)
    , ("crypto_sign_ed25519_sk_to_curve25519", summaryNoOp)
    , ("epoll_create", summaryNoOp)
    , ("epoll_wait", summaryNoOp)
    , ("fcntl", summaryNoOp)
    , ("getaddrinfo", summaryGetaddrinfo)
    , ("getsockopt", summaryNoOp)
    , ("htonl", summaryNoOp)
    , ("htons", summaryNoOp)
    , ("inet_ntop", summaryReturnsPointer)
    , ("inet_pton", summaryNoOp)
    , ("ioctlsocket", summaryNoOp)
    , ("listen", summaryNoOp)
    , ("memcpy", summaryMemcpy)
    , ("mem_is_heap", summaryNoOp)
    , ("memmove", summaryMemcpy)
    , ("ntohl", summaryNoOp)
    , ("ntohs", summaryNoOp)
    , ("opus_decode", summaryNoOp)
    , ("opus_decoder_create", summaryNoOp)
    , ("opus_decoder_get_nb_samples", summaryNoOp)
    , ("opus_encode", summaryNoOp)
    , ("opus_encoder_create", summaryNoOp)
    , ("opus_encoder_ctl", summaryNoOp)
    , ("opus_packet_get_nb_channels", summaryNoOp)
    , ("printf", summaryNoOp)
    , ("random_bytes", summaryNoOp)
    , ("randombytes_random", summaryNoOp)
    , ("randombytes_uniform", summaryNoOp)
    , ("recvfrom", summaryNoOp)
    , ("recv", summaryNoOp)
    , ("send", summaryNoOp)
    , ("sendto", summaryNoOp)
    , ("setsockopt", summaryNoOp)
    , ("snprintf", summaryNoOp)
    , ("strcpy", summaryStrcpy)
    , ("strerror_r", summaryNoOp)
    , ("strlen", summaryNoOp)
    , ("strrchr", summaryReturnsPointer)
    , ("time", summaryNoOp)
    , ("vpx_codec_control", summaryNoOp)
    , ("vpx_codec_dec_init", summaryNoOp)
    , ("vpx_codec_decode", summaryNoOp)
    , ("vpx_codec_enc_config_default", summaryNoOp)
    , ("vpx_codec_enc_config_set", summaryNoOp)
    , ("vpx_codec_enc_init", summaryNoOp)
    , ("vpx_codec_encode", summaryNoOp)
    , ("vpx_codec_get_cx_data", summaryNoOp)
    , ("vpx_codec_get_frame", summaryNoOp)
    , ("vpx_codec_vp8_cx", summaryNoOp)
    , ("vpx_codec_vp8_dx", summaryNoOp)
    -- keep-sorted end
    ]

-- The main function to get a summary for an external call.
getExternalSummary :: ScopedId -> Maybe ExternalSummary
getExternalSummary sid = Map.lookup (sidName sid) summaries

-- Summaries Implementation

summaryMalloc :: ExternalSummary
summaryMalloc file pos _ = return (Set.fromList [HeapLoc (locFromPos file pos)], False)

summaryRealloc :: ExternalSummary
summaryRealloc file pos _ = return (Set.fromList [HeapLoc (locFromPos file pos)], True)

summaryFree :: ExternalSummary
summaryFree _ _ _ = return (Set.empty, True)

-- For now, we assume strcpy, memcpy, etc., don't transfer pointers.
-- This is a simplifying assumption as per the design doc.
summaryStrcpy :: ExternalSummary
summaryStrcpy _ _ _ = return (Set.empty, True)

summaryMemcpy :: ExternalSummary
summaryMemcpy _ _ _ = return (Set.empty, True)

summaryNoOp :: ExternalSummary
summaryNoOp _ _ _ = return (Set.empty, False)

summaryGetaddrinfo :: ExternalSummary
summaryGetaddrinfo _ _ _ = return (Set.empty, True) -- returns int, but modifies pointer arg

summaryReturnsPointer :: ExternalSummary
summaryReturnsPointer _ _ _ = return (Set.singleton UnknownLoc, False)


-- Helpers

locFromPos :: FilePath -> C.AlexPosn -> Text
locFromPos file (C.AlexPn _ line col) = pack $ file ++ ":" ++ show line ++ ":" ++ show col
