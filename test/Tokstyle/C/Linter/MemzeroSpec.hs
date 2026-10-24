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
