{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CommentsSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (allWarnings, analyse)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "rejects gendered pronouns in plain comments" $ do
        ast <- mustParse
            [ "/* This is the peer himself. */"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:1: inappropriately gendered pronoun: himself [-Wcomments]"
            ]
    it "rejects gendered pronouns in doxygen comments" $ do
        ast <- mustParse
            [ "/** @brief Checks peer goodness."
            , " *"
            , " * Checks a peer to see if she is good."
            , " */"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:3: inappropriately gendered pronoun: she [-Wcomments]"
            ]
    it "accepts gendered pronouns in code" $ do
        ast <- mustParse
            [ "void f(int ge, int he) { return; }"
            ]
        analyse allWarnings ("test.c", ast) `shouldBe` []
