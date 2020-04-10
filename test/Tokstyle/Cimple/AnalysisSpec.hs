{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.AnalysisSpec where

import           Test.Hspec               (Spec, describe, it, shouldBe)

import           Language.Cimple.IO       (parseText)
import           Tokstyle.Cimple.Analysis (analyse)


spec :: Spec
spec =
    describe "analyse" $ do
        it "should parse a simple function" $ do
            Right ast <- parseText "int a(void) { return 3; }"
            analyse "test.c" ast `shouldBe` []

        it "should give diagnostics on extern decls in .c files" $ do
            Right ast <- parseText "int a(void);"
            analyse "test.c" ast
                `shouldBe` ["test.c:1: global function `a' declared in .c file"]

        it "should not give diagnostics on extern decls in .h files" $ do
            Right ast <- parseText "int a(void);"
            analyse "test.h" ast `shouldBe` []
