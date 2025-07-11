{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.DeclaredOnceSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseGlobal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on valid declarations" $ do
        ast1 <- mustParse
            [ "int foo(void);"
            ]
        ast2 <- mustParse
            [ "int foo(void) { return 0; }"
            ]
        analyseGlobal ["declared-once"] [("test1.h", ast1), ("test2.c", ast2)]
            `shouldBe`
            []

    it "should give diagnostics on duplicate declaration in the same file" $ do
        ast <- mustParse
            [ "void foo(void);"
            , "void foo(void);"
            ]
        analyseGlobal ["declared-once"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:1: duplicate declaration of function `foo` [-Wdeclared-once]"
            , "test.c:2: function `foo` also declared here [-Wdeclared-once]"
            ]

    it "should give diagnostics on duplicate declaration in different files" $ do
        ast1 <- mustParse
            [ "void foo(void);"
            ]
        ast2 <- mustParse
            [ "void foo(void);"
            ]
        analyseGlobal ["declared-once"] [("test1.h", ast1), ("test2.h", ast2)]
            `shouldBe`
            [ "test1.h:1: duplicate declaration of function `foo` [-Wdeclared-once]"
            , "test2.h:1: function `foo` also declared here [-Wdeclared-once]"
            ]

    it "should not give diagnostics for multiple definitions" $ do
        ast1 <- mustParse
            [ "int foo(void) { return 0; }"
            ]
        ast2 <- mustParse
            [ "int foo(void) { return 0; }"
            ]
        analyseGlobal ["declared-once"] [("test1.c", ast1), ("test2.c", ast2)]
            `shouldBe`
            []
