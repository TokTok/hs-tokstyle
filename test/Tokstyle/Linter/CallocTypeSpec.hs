{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CallocTypeSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (allWarnings, analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "detects when mem_alloc() is used with built-in types" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  uint8_t *a = (uint8_t *)mem_alloc(mem, sizeof(uint8_t));"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            ["test.c:2: `mem_alloc` should not be used for `\ESC[0;32muint8_t\ESC[0m*`; use `mem_balloc` instead [-Wcalloc-type]"]

    it "detects when mem_valloc() result is cast to the wrong type" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  uint8_t *a = (uint8_t *)mem_valloc(mem, 1);"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: invalid `mem_valloc` invocation: 2 arguments after `mem` expected [-Wcalloc-args]"
            , "test.c:2: the result of `mem_valloc` must be cast to its member type [-Wcalloc-type]"
            ]

    it "detects when mem_valloc() result is not cast to any type" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  void *a = mem_valloc(mem, 2, sizeof(int));"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            ["test.c:2: the result of `mem_valloc` must be cast to its member type [-Wcalloc-type]"]

    it "should not give diagnostics mem_alloc() used correctly" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  Foo *a = (Foo *)mem_alloc(mem, sizeof(Foo));"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe` []
