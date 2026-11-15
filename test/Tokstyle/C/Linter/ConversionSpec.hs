{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.ConversionSpec (spec) where
import           Data.Text             (Text)
import           Test.Hspec            (Spec, describe, it)

import qualified Data.Text             as Text
import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["conversion"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["conversion"]


spec :: Spec
spec = describe "Conversion linter" $ do
    describe "invalid conversions" $ do
        it "warns on returning incompatible pointer types" $ do
            shouldWarn'
                [ "char *func(int *p) {"
                , "  return p;"
                , "}"
                ]
                [[ "error: invalid conversion from `int *` to `char *` in return [-Wconversion]"
                 , "   --> test.c:2:10"
                 , "    |"
                 , "   2|   return p;"
                 , "    |          ^"
                 ]]

        it "warns on initialising a narrower integer" $ do
            shouldWarn'
                [ "typedef unsigned long long uint64_t;"
                , "typedef unsigned short uint16_t;"
                , "void func(uint64_t id) {"
                , "  const uint16_t start = id;"
                , "}"
                ]
                [[ "error: invalid conversion from `uint64_t` to `uint16_t` in initialiser [-Wconversion]"
                 , "   --> test.c:4:26"
                 , "    |"
                 , "   4|   const uint16_t start = id;"
                 , "    |                          ^^"
                 ]]

    describe "valid conversions" $ do
        it "allows casting to void*" $ do
            shouldAccept'
                [ "void *func(int *p) {"
                , "  return p;"
                , "}"
                ]

        it "allows integer promotions" $ do
            shouldAccept'
                [ "long func(int i) {"
                , "  return i;"
                , "}"
                ]

        it "allows Memory const * to const struct Memory * assignment" $ do
            shouldAccept'
                [ "typedef struct Memory Memory;"
                , "void func() {"
                , "  const struct Memory *a;"
                , "  Memory const *b;"
                , "  a = b;"
                , "}"
                ]

        it "allows const struct Memory * to Memory const * assignment" $ do
            shouldAccept'
                [ "typedef struct Memory Memory;"
                , "void func() {"
                , "  const struct Memory *a;"
                , "  Memory const *b;"
                , "  b = a;"
                , "}"
                ]

        it "allows int* to const int* assignment" $ do
            shouldAccept'
                [ "void func() {"
                , "  int *a = 0;"
                , "  const int *b;"
                , "  b = a;"
                , "}"
                ]

        it "allows nullptr assignment" $ do
            shouldAccept'
                [ "typedef void *nullptr_t;"
                , "extern nullptr_t nullptr;"
                , "void func() {"
                , "  int *a;"
                , "  a = nullptr;"
                , "}"
                ]

        it "warns on int to pointer assignment" $ do
            shouldWarn'
                [ "void func() {"
                , "  int a = 0;"
                , "  char *b;"
                , "  b = a;"
                , "}"
                ]
                [[ "error: invalid conversion from `int` to `char *` in assignment [-Wconversion]"
                 , "   --> test.c:4:7"
                 , "    |"
                 , "   4|   b = a;"
                 , "    |       ^"
                 ]]
