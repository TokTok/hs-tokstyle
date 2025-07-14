{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.ConversionSpec (spec) where

import           Test.Hspec            (Spec, describe, it, shouldBe)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings, analyse)
import           Tokstyle.C.LinterSpec (mustParse)


spec :: Spec
spec = describe "Conversion linter" $ do
    describe "invalid conversions" $ do
        it "warns on returning incompatible pointer types" $ do
            ast <- mustParse
                [ "char *func(int *p) {"
                , "  return p;"
                , "}"
                ]
            analyse allWarnings ast
                `shouldBe`
                [ Text.unlines
                    [ "test.c:2: (column 10) [ERROR]  >>> Type mismatch"
                    , "  invalid conversion from `int *` to `char *` in return"
                    ]
                ]

    describe "valid conversions" $ do
        it "allows casting to void*" $ do
            ast <- mustParse
                [ "void *func(int *p) {"
                , "  return p;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows integer promotions" $ do
            ast <- mustParse
                [ "long func(int i) {"
                , "  return i;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []
