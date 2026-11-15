{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.MemzeroSpec (spec) where
import           Data.Text             (Text)
import           Test.Hspec            (Spec, it)

import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["memzero"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["memzero"]


prelude :: [Text]
prelude =
    [ "void crypto_memzero(void *data, unsigned int length);"
    , "void crypto_memunlock(void *data, unsigned int length);"
    , "void hash(unsigned char *out, const unsigned char *in);"
    , "int derive(unsigned char *out, const unsigned char *in);"
    ]


spec :: Spec
spec = do
    it "warns about a return that skips the wipe" $ do
        shouldWarn' (prelude ++
            [ "int f(const unsigned char *pw, unsigned char *out) {"
            , "  unsigned char key[32];"
            , "  hash(key, pw);"
            , "  if (derive(out, key) != 0) {"
            , "    return -1;"
            , "  }"
            , "  crypto_memzero(key, sizeof(key));"
            , "  return 0;"
            , "}"
            ])
            [[ "error: `key` is wiped on other paths, but not before the function returns here [-Wmemzero]"
             , "   --> test.c:9:5"
             , "    |"
             , "   9|     return -1;"
             , "    |     ^^^^^^^^^^"
             , "  11|   crypto_memzero(key, sizeof(key));"
             , "    |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
             , "    |   |"
             , "    |   wiped here"
             ]]

    it "accepts wipes on every path that wrote the local" $ do
        shouldAccept' (prelude ++
            [ "int f(int c, const unsigned char *pw, unsigned char *out) {"
            , "  unsigned char key[32];"
            , "  if (c) {"
            , "    return -1;"
            , "  }"
            , "  hash(key, pw);"
            , "  int ret = 0;"
            , "  if (derive(out, key) != 0) {"
            , "    ret = -1;"
            , "    goto out;"
            , "  }"
            , "out:"
            , "  crypto_memzero(key, sizeof(key));"
            , "  return ret;"
            , "}"
            ])

    it "accepts a return after a failed call that would have written it" $ do
        shouldAccept' (prelude ++
            [ "int f(const unsigned char *pw, unsigned char *out) {"
            , "  unsigned char key[32];"
            , "  if (derive(key, pw) != 0) {"
            , "    return -1;"
            , "  }"
            , "  hash(out, key);"
            , "  crypto_memzero(key, sizeof(key));"
            , "  return 0;"
            , "}"
            ])

    it "accepts unlocking after the wipe" $ do
        shouldAccept' (prelude ++
            [ "void f(const unsigned char *pw, unsigned char *out) {"
            , "  unsigned char key[32];"
            , "  hash(key, pw);"
            , "  hash(out, key);"
            , "  crypto_memzero(key, sizeof(key));"
            , "  crypto_memunlock(key, sizeof(key));"
            , "}"
            ])

    it "warns about a secret that another caller of its function wipes" $ do
        shouldWarn' (secretPrelude ++
            [ "void wiping(const Random *rng, unsigned char *out) {"
            , "  unsigned char pk[32];"
            , "  unsigned char sk[32];"
            , "  new_keypair(rng, pk, sk);"
            , "  hash(out, sk);"
            , "  crypto_memzero(sk, sizeof(sk));"
            , "}"
            , "void leaking(const Random *rng, unsigned char *out) {"
            , "  unsigned char pk[32];"
            , "  unsigned char sk[32];"
            , "  new_keypair(rng, pk, sk);"
            , "  hash(out, sk);"
            , "}"
            ])
            [[ "error: `sk` holds a secret, but isn't wiped before the function returns here [-Wmemzero]"
             , "   --> test.c:18:3"
             , "    |"
             , "  17|   new_keypair(rng, pk, sk);"
             , "    |   ^^^^^^^^^^^^^^^^^^^^^^^^"
             , "    |   |"
             , "    |   other callers of `new_keypair` wipe what it writes here"
             , "  18|   hash(out, sk);"
             , "    |   ^^^^^^^^^^^^^^"
             ]]

    it "accepts a secret that nothing reads" $ do
        shouldAccept' (secretPrelude ++
            [ "void wiping(const Random *rng, unsigned char *out) {"
            , "  unsigned char pk[32];"
            , "  unsigned char sk[32];"
            , "  new_keypair(rng, pk, sk);"
            , "  hash(out, sk);"
            , "  crypto_memzero(sk, sizeof(sk));"
            , "}"
            , "void public_only(const Random *rng, unsigned char *out) {"
            , "  unsigned char pk[32];"
            , "  unsigned char unused_sk[32];"
            , "  new_keypair(rng, pk, unused_sk);"
            , "  hash(out, pk);"
            , "}"
            ])

    it "accepts what a function writes from data it reads" $ do
        shouldAccept' (prelude ++
            [ "typedef unsigned char uint8_t;"
            , "void encrypt(const uint8_t key[32], const uint8_t *plain, uint8_t *out);"
            , "void wiping(const uint8_t *k, const uint8_t *pw, uint8_t *out) {"
            , "  uint8_t sealed[32];"
            , "  encrypt(k, pw, sealed);"
            , "  hash(out, sealed);"
            , "  crypto_memzero(sealed, sizeof(sealed));"
            , "}"
            , "void sealing(const uint8_t *k, uint8_t *out) {"
            , "  uint8_t plain[32];"
            , "  hash(plain, k);"
            , "  uint8_t sealed[32];"
            , "  encrypt(k, plain, sealed);"
            , "  hash(out, sealed);"
            , "}"
            ])

    it "accepts what a function reads from a stream" $ do
        shouldAccept' (prelude ++
            [ "typedef struct Stream Stream;"
            , "int read_bytes(Stream *s, unsigned char *out, unsigned int length);"
            , "void loading_key(Stream *s, unsigned char *out) {"
            , "  unsigned char key[32];"
            , "  read_bytes(s, key, sizeof(key));"
            , "  hash(out, key);"
            , "  crypto_memzero(key, sizeof(key));"
            , "}"
            , "void loading_name(Stream *s, unsigned char *out) {"
            , "  unsigned char name[32];"
            , "  read_bytes(s, name, sizeof(name));"
            , "  hash(out, name);"
            , "}"
            ])

    it "accepts what a function writes where a caller returns it" $ do
        shouldAccept' (secretPrelude ++
            [ "void fill(const Random *rng, unsigned char *bytes);"
            , "void seeded(const Random *rng, unsigned char *out) {"
            , "  unsigned char seed[32];"
            , "  fill(rng, seed);"
            , "  hash(out, seed);"
            , "  crypto_memzero(seed, sizeof(seed));"
            , "}"
            , "unsigned char random_u08(const Random *rng) {"
            , "  unsigned char n;"
            , "  fill(rng, &n);"
            , "  return n;"
            , "}"
            , "void nonce(const Random *rng, unsigned char *out) {"
            , "  unsigned char bytes[24];"
            , "  fill(rng, bytes);"
            , "  hash(out, bytes);"
            , "}"
            ])
  where
    secretPrelude = prelude ++
        [ "typedef struct Random Random;"
        , "int new_keypair(const Random *rng, unsigned char *pk, unsigned char *sk);"
        ]
