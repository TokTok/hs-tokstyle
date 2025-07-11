{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.GlobalFuncsSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should give diagnostics on global function declaration in .c file" $ do
        ast <- mustParse
            [ "void foo(void);"
            ]
        analyseLocal ["global-funcs"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: global function `foo` declared in .c file [-Wglobal-funcs]"
            ]

    it "should not give diagnostics on global function declaration in .h file" $ do
        ast <- mustParse
            [ "void foo(void);"
            ]
        analyseLocal ["global-funcs"] ("test.h", ast)
            `shouldBe`
            []

    it "should not give diagnostics on static function declaration in .c file" $ do
        ast <- mustParse
            [ "static void foo(void);"
            ]
        analyseLocal ["global-funcs"] ("test.c", ast)
            `shouldBe`
            []

    it "should not give diagnostics on function definition in .c file" $ do
        ast <- mustParse
            [ "int foo(void) { return 0; }"
            ]
        analyseLocal ["global-funcs"] ("test.c", ast)
            `shouldBe`
            []
