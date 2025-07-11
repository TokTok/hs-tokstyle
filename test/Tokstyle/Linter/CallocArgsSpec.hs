{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CallocArgsSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on valid calloc" $ do
        ast <- mustParse
            [ "void f(int n) {"
            , "  int *p = calloc(n, sizeof(int));"
            , "}"
            ]
        analyseLocal ["calloc-args"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on calloc with invalid nmemb" $ do
        ast <- mustParse
            [ "void f(int n) {"
            , "  int *p = calloc(sizeof(int), sizeof(int));"
            , "}"
            ]
        analyseLocal ["calloc-args"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: `sizeof` should not appear in the `nmemb` argument to `calloc` [-Wcalloc-args]"
            ]

    it "should give diagnostics on calloc with invalid size" $ do
        ast <- mustParse
            [ "void f(int n) {"
            , "  int *p = calloc(n, 16);"
            , "}"
            ]
        analyseLocal ["calloc-args"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: `size` argument in call to `calloc` must be a sizeof expression [-Wcalloc-args]"
            ]

    it "should not give diagnostics on valid realloc" $ do
        ast <- mustParse
            [ "void f(int n, void *ptr) {"
            , "  int *p = realloc(ptr, n * sizeof(int));"
            , "}"
            ]
        analyseLocal ["calloc-args"] ("test.c", ast)
            `shouldBe`
            []

    it "should not give diagnostics on valid mem_alloc" $ do
        ast <- mustParse
            [ "void f(void *mem) {"
            , "  int *p = mem_alloc(mem, sizeof(int));"
            , "}"
            ]
        analyseLocal ["calloc-args"] ("test.c", ast)
            `shouldBe`
            []

    it "should not give diagnostics on valid mem_valloc" $ do
        ast <- mustParse
            [ "void f(void *mem, int n) {"
            , "  int *p = mem_valloc(mem, n, sizeof(int));"
            , "}"
            ]
        analyseLocal ["calloc-args"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on wrong number of arguments" $ do
        ast <- mustParse
            [ "void f(void* p, void* mem) {"
            , "  p = calloc(1);"
            , "  p = realloc(p, 1, 2);"
            , "  p = mem_alloc(mem, 1, 2);"
            , "  p = mem_valloc(mem, 1);"
            , "}"
            ]
        analyseLocal ["calloc-args"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: invalid `calloc` invocation: 2 arguments expected [-Wcalloc-args]"
            , "test.c:3: invalid `realloc` invocation: 2 arguments expected [-Wcalloc-args]"
            , "test.c:4: invalid `mem_alloc` invocation: 1 argument after `mem` expected [-Wcalloc-args]"
            , "test.c:5: invalid `mem_valloc` invocation: 2 arguments after `mem` expected [-Wcalloc-args]"
            ]
