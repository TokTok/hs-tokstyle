{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.BooleanReturnSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (allWarnings, analyse)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should give diagnostics on functions with only 2 const returns" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  return 1;"
            , "  return 0;"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:1: function `a` only ever returns two values `0` and `1`; it can return `bool` [-Wboolean-return]"
            ]

    it "should show negative return values in the diagnostic" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  return -1;"
            , "  return 0;"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:1: function `a` only ever returns two values `-1` and `0`; it can return `bool` [-Wboolean-return]"
            ]

    it "should not give diagnostics on functions with non-const returns" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  return 1;"
            , "  return 0;"
            , "  return foo();"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            []

    it "should not give diagnostics on functions with more than 2 const returns" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  return 1;"
            , "  return 0;"
            , "  return -1;"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            []
