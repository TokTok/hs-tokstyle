{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.SizeofTargetSpec (spec) where

import           Data.Text             (Text)
import           Test.Hspec            (Spec, it)

import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["sizeof-target"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["sizeof-target"]


decls :: [Text]
decls =
    [ "typedef unsigned long size_t;"
    , "typedef unsigned char uint8_t;"
    , "typedef struct Random Random;"
    , "void crypto_memzero(void *data, size_t length);"
    , "void *memcpy(void *dest, const void *src, size_t n);"
    , "void random_bytes(const Random *rng, uint8_t *bytes, size_t length);"
    , "int printf(const char *fmt, ...);"
    , "typedef struct Conn { uint8_t public_key[32]; uint8_t secret_key[32]; } Conn;"
    ]


spec :: Spec
spec = do
    it "warns when the sizeof is of another buffer of the same size" $ do
        shouldWarn' (decls ++
            [ "void reset(Conn *conn) {"
            , "  uint8_t sk[32];"
            , "  crypto_memzero(conn->public_key, sizeof(sk));"
            , "}"
            ])
            [[ "error: `sizeof(sk)` measures `sk`, not `conn->public_key` [-Wsizeof-target]"
             , "   --> test.c:11:43"
             , "    |"
             , "  11|   crypto_memzero(conn->public_key, sizeof(sk));"
             , "    |                                           ^^"
             ]]

    it "says it's not the size when the sizes differ" $ do
        shouldWarn' (decls ++
            [ "void reset(Conn *conn) {"
            , "  uint8_t sk[64];"
            , "  crypto_memzero(conn->public_key, sizeof(sk));"
            , "}"
            ])
            [[ "error: `sizeof(sk)` is not the size of `conn->public_key` [-Wsizeof-target]"
             , "   --> test.c:11:43"
             , "    |"
             , "  11|   crypto_memzero(conn->public_key, sizeof(sk));"
             , "    |                                           ^^"
             ]]

    it "warns when a memcpy size is of neither buffer" $ do
        shouldWarn' (decls ++
            [ "void copy(Conn *a, const Conn *b) {"
            , "  uint8_t tmp[32];"
            , "  memcpy(a->public_key, b->public_key, sizeof(tmp));"
            , "}"
            ])
            [[ "error: `sizeof(tmp)` measures `tmp`, not `a->public_key` or `b->public_key` [-Wsizeof-target]"
             , "   --> test.c:11:47"
             , "    |"
             , "  11|   memcpy(a->public_key, b->public_key, sizeof(tmp));"
             , "    |                                               ^^^"
             ]]

    it "accepts the size of either buffer, through casts, addresses and pointers" $ do
        shouldAccept' (decls ++
            [ "void ok(Conn *conn, const Random *rng, Conn *out) {"
            , "  uint8_t sk[32];"
            , "  unsigned int n;"
            , "  crypto_memzero(conn->secret_key, sizeof(conn->secret_key));"
            , "  memcpy(out->public_key, sk, sizeof(sk));"
            , "  random_bytes(rng, (uint8_t *)&n, sizeof(n));"
            , "  crypto_memzero(out, sizeof(*out));"
            , "}"
            ])

    it "ignores sizes that are not a plain sizeof, and variadic arguments" $ do
        shouldAccept' (decls ++
            [ "void other(Conn *conn) {"
            , "  uint8_t sk[32];"
            , "  crypto_memzero(conn->public_key, sizeof(uint8_t) * 32);"
            , "  crypto_memzero(conn->public_key, 32);"
            , "  printf(\"%zu\", sizeof(sk));"
            , "}"
            ])
