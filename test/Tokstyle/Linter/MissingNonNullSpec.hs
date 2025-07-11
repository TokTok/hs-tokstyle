{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.MissingNonNullSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "does not warn for correctly annotated function with non-null per-parameter attribute" $ do
        ast <- mustParse
            [ "void func_a(non_null() int *b);"
            ]
        analyseLocal ["missing-non-null"] ("test.c", ast) `shouldBe` []

    it "does not warn for correctly annotated function with nullable per-parameter attribute" $ do
        ast <- mustParse
            [ "void func_b(nullable() int *c);"
            ]
        analyseLocal ["missing-non-null"] ("test.c", ast) `shouldBe` []

    it "does not warn for correctly annotated static function with per-parameter attributes" $ do
        ast <- mustParse
            [ "static void func_c(non_null() int *b, nullable() int *c);"
            ]
        analyseLocal ["missing-non-null"] ("test.c", ast) `shouldBe` []

    it "warns for global function with pointer args and no annotation" $ do
        ast <- mustParse
            [ "void func_d(int *b);"
            ]
        analyseLocal ["missing-non-null"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: global function has no non_null or nullable annotation [-Wmissing-non-null]"
            ]

    it "warns for static function with pointer args and no annotation" $ do
        ast <- mustParse
            [ "static void func_e(int *b);"
            ]
        analyseLocal ["missing-non-null"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: static function must have nullability annotation [-Wmissing-non-null]"
            ]
