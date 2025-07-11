{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.NonNullSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "warns about nullability attribute on non-pointer parameter" $ do
        ast <- mustParse
            [ "nullable(1) void func_b(int b);"
            ]
        analyseLocal ["non-null"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: parameter 1 (`\ESC[0;32mint\ESC[0m b`) does not have a pointer type; nullability has no effect [-Wnon-null]"
            , "test.c:1: function declaration has no pointer-type parameters, nullability has no effect [-Wnon-null]"
            ]

    it "does not warn for correctly annotated function" $ do
        ast <- mustParse
            [ "non_null(1) nullable(2) void func_c(int *b, int *c);"
            ]
        analyseLocal ["non-null"] ("test.c", ast) `shouldBe` []

    it "does not warn for correctly annotated static function with a separate declaration" $ do
        ast <- mustParse
            [ "non_null() static int func_c(int *b, int *c);"
            , "non_null() static int func_c(int *b, int *c) { return 0; }"
            ]
        analyseLocal ["non-null"] ("test.c", ast) `shouldBe` []

    it "warns when destructor is not nullable" $ do
        ast <- mustParse
            [ "non_null() void some_kill(int *b);"
            ]
        analyseLocal ["non-null"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: destructor function `some_kill` must accept nullable arguments [-Wnon-null]"
            ]

    it "warns when nullability is on definition of global function" $ do
        ast <- mustParse
            [ "non_null() int func_d(int *b) { return 0; }"
            ]
        analyseLocal ["non-null"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: global function must only have nullability attribute on its declaration, not on its definition [-Wnon-null]"
            ]

    it "warns when nullability is on definition of static function but not declaration" $ do
        ast <- mustParse
            [ "static void func_e(int *b);"
            , "non_null() static int func_e(int *b) { return 0; }"
            ]
        analyseLocal ["non-null"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: static function must have nullability attribute on its declaration if it has one [-Wnon-null]"
            , "test.c:1:   declaration was here [-Wnon-null]"
            ]

    it "warns when function with no pointers has nullability" $ do
        ast <- mustParse
            [ "non_null() void func_f(int b);"
            ]
        analyseLocal ["non-null"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: function declaration has no pointer-type parameters, nullability has no effect [-Wnon-null]"
            ]

    it "does not warn for correctly annotated function with per-parameter attributes" $ do
        ast <- mustParse
            [ "void func_g(non_null() int *b, nullable() int *c);"
            ]
        analyseLocal ["non-null"] ("test.c", ast) `shouldBe` []

    it "warns about per-parameter nullability attribute on non-pointer parameter" $ do
        ast <- mustParse
            [ "void func_h(non_null() int b);"
            ]
        analyseLocal ["non-null"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: nullability attribute on non-pointer parameter `b` [-Wnon-null]"
            ]

    it "warns when both function-level and per-parameter nullability are present" $ do
        ast <- mustParse
            [ "non_null() void func_i(nullable() int *b);"
            ]
        analyseLocal ["non-null"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: cannot mix function-level and per-parameter nullability attributes [-Wnon-null]"
            ]

    it "warns when destructor is not nullable with per-parameter attribute" $ do
        ast <- mustParse
            [ "void some_kill(non_null() int *b);"
            ]
        analyseLocal ["non-null"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: destructor function `some_kill` must accept nullable arguments [-Wnon-null]"
            ]

    it "warns when per-parameter nullability is on definition of global function" $ do
        ast <- mustParse
            [ "int func_k(non_null() int *b) { return 0; }"
            ]
        analyseLocal ["non-null"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: global function must only have nullability attribute on its declaration, not on its definition [-Wnon-null]"
            ]

    it "warns when per-parameter nullability is on definition of static function but not declaration" $ do
        ast <- mustParse
            [ "static void func_l(int *b);"
            , "static int func_l(non_null() int *b) { return 0; }"
            ]
        analyseLocal ["non-null"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: static function must have nullability attribute on its declaration if it has one [-Wnon-null]"
            , "test.c:1:   declaration was here [-Wnon-null]"
            ]
