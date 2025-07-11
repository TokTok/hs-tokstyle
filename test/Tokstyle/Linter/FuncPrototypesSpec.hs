{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.FuncPrototypesSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on (void) parameter list" $ do
        ast <- mustParse
            [ "void foo(void);"
            ]
        analyseLocal ["func-prototypes"] ("test.h", ast)
            `shouldBe`
            []

    it "should give diagnostics on () parameter list in declaration" $ do
        ast <- mustParse
            [ "void foo();"
            ]
        analyseLocal ["func-prototypes"] ("test.h", ast)
            `shouldBe`
            [ "test.h:1: empty parameter list must be written as `(void)` [-Wfunc-prototypes]"
            ]

    it "should not give diagnostics on () parameter list in definition" $ do
        ast <- mustParse
            [ "int foo() { return 0; }"
            ]
        analyseLocal ["func-prototypes"] ("test.c", ast)
            `shouldBe`
            []

    it "should not give diagnostics on functions with parameters" $ do
        ast <- mustParse
            [ "void foo(int a, int b);"
            ]
        analyseLocal ["func-prototypes"] ("test.h", ast)
            `shouldBe`
            []
