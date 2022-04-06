{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.ParensSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (allWarnings, analyse)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "warns about parentheses around return expressions" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  return (1 + 2);"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: return expression does not need parentheses [-Wparens]"
            ]

    it "does not warn about parens in if conditions" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  if ((true)) { return 3; }"
            , "}"
            ]
        analyse allWarnings ("test.c", ast) `shouldBe` []
