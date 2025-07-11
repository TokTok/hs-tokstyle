{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.DeclsHaveDefnsSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseGlobal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on valid decl/defn pairs" $ do
        ast1 <- mustParse
            [ "int some_function(void);"
            , "typedef struct Some_Struct Some_Struct;"
            ]
        ast2 <- mustParse
            [ "int some_function(void) { return 0; }"
            , "struct Some_Struct { int field; };"
            ]
        analyseGlobal ["decls-have-defns"] [("test1.h", ast1), ("test2.c", ast2)]
            `shouldBe`
            []

    it "should give diagnostics on missing function definition" $ do
        ast <- mustParse
            [ "void some_other_function(void);"
            ]
        analyseGlobal ["decls-have-defns"] [("test.h", ast)]
            `shouldBe`
            [ "test.h:1: missing definition for `some_other_function` [-Wdecls-have-defns]"
            ]

    it "should give diagnostics on missing struct definition" $ do
        ast <- mustParse
            [ "struct Some_Other_Struct;"
            ]
        analyseGlobal ["decls-have-defns"] [("test.h", ast)]
            `shouldBe`
            [ "test.h:1: missing definition for `Some_Other_Struct` [-Wdecls-have-defns]"
            ]

    it "should give suggestion for similar names" $ do
        ast1 <- mustParse
            [ "int my_long_function_name(void);"
            ]
        ast2 <- mustParse
            [ "int my_long_functoin_name(void) { return 0; }"
            ]
        analyseGlobal ["decls-have-defns"] [("test1.h", ast1), ("test2.c", ast2)]
            `shouldBe`
            [ "test1.h:1: missing definition for `my_long_function_name` [-Wdecls-have-defns]"
            , "test2.c:1: did you mean `my_long_functoin_name`? [-Wdecls-have-defns]"
            ]
