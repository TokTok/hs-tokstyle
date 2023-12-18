{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.ParensSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (allWarnings, analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "warns about parentheses around return expressions" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  return (1 + 2);"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: return expression does not need parentheses [-Wparens]"
            ]

    it "does not warn about parens in if conditions" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  if ((true)) { return 3; }"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast) `shouldBe` []
