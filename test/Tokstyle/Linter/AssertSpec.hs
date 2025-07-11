{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.AssertSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on pure expressions in assert" $ do
        ast <- mustParse
            [ "void a(int b) { assert(b == 1); }"
            ]
        analyseLocal ["assert"] ("test.c", ast)
            `shouldBe`
            []

    it "should not give diagnostics on exempt functions in assert" $ do
        ast <- mustParse
            [ "void a(void *x, void *y, int z) { assert(memcmp(x, y, z) == 0); }"
            ]
        analyseLocal ["assert"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on non-pure functions in assert" $ do
        ast <- mustParse
            [ "void a(int b) { assert(some_function(b) == 1); }"
            ]
        analyseLocal ["assert"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: non-pure function `some_function` cannot be called inside `assert()` [-Wassert]"
            ]

    it "should not give diagnostics on complex pure expressions in assert" $ do
        ast <- mustParse
            [ "typedef struct Foo { int c[1]; } Foo;"
            , "void a(int b, Foo *b_ptr) { assert((b > 0) && (b < 10) || (b_ptr->c[0] == 42)); }"
            ]
        analyseLocal ["assert"] ("test.c", ast)
            `shouldBe`
            []

    it "should not give diagnostics on functions with no arguments" $ do
        ast <- mustParse
            [ "void a(int b) { assert(is_ready()); }"
            ]
        analyseLocal ["assert"] ("test.c", ast)
            `shouldBe`
            []
