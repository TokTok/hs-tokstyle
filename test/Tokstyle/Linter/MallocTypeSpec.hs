{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.MallocTypeSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on valid malloc calls" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  uint8_t *buf = (uint8_t *)malloc(10);"
            , "}"
            ]
        analyseLocal ["malloc-type"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on invalid malloc calls" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  My_Struct *s = (My_Struct *)malloc(sizeof(My_Struct));"
            , "}"
            ]
        analyseLocal ["malloc-type"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: `malloc` should be used for builtin types only (e.g. `uint8_t *` or `int16_t *`); use `mem_alloc` instead [-Wmalloc-type]"
            ]

    it "should not give diagnostics on valid mem_balloc calls" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  uint8_t *buf = (uint8_t *)mem_balloc(m, 10);"
            , "}"
            ]
        analyseLocal ["malloc-type"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on invalid mem_balloc calls" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  My_Struct *s = (My_Struct *)mem_balloc(m, sizeof(My_Struct));"
            , "}"
            ]
        analyseLocal ["malloc-type"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: `mem_balloc` should be used for builtin types only (e.g. `uint8_t *` or `int16_t *`); use `mem_alloc` instead [-Wmalloc-type]"
            ]

    it "should give diagnostics on uncasted malloc" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  void *buf = malloc(10);"
            , "}"
            ]
        analyseLocal ["malloc-type"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: the result of `malloc` must be cast; plain `void *` is not supported [-Wmalloc-type]"
            ]
