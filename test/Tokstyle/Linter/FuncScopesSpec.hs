{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.FuncScopesSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on matching scopes" $ do
        ast <- mustParse
            [ "int foo(void);"
            , "int foo(void) { return 0; }"
            , "static int bar(void);"
            , "static int bar(void) { return 0; }"
            ]
        analyseLocal ["func-scopes"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on scope mismatch (static decl, extern def)" $ do
        ast <- mustParse
            [ "static int foo(void);"
            , "int foo(void) { return 0; }"
            ]
        analyseLocal ["func-scopes"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: function definition `foo` does not agree with its declaration about scope: declaration on line 1 is static but definition is extern [-Wfunc-scopes]"
            ]

    it "should give diagnostics on scope mismatch (extern decl, static def)" $ do
        ast <- mustParse
            [ "int foo(void);"
            , "static int foo(void) { return 0; }"
            ]
        analyseLocal ["func-scopes"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: function definition `foo` does not agree with its declaration about scope: declaration on line 1 is extern but definition is static [-Wfunc-scopes]"
            ]

    it "should not give diagnostics if there is no prior declaration" $ do
        ast <- mustParse
            [ "static int foo(void) { return 0; }"
            ]
        analyseLocal ["func-scopes"] ("test.c", ast)
            `shouldBe`
            []
