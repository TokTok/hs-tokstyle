{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.MemcpyStructsSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on valid memcpy calls" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  memcpy(a, b, 10);"
            , "}"
            ]
        analyseLocal ["memcpy-structs"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on invalid memcpy calls" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  memcpy(a, b, sizeof(My_Struct));"
            , "}"
            ]
        analyseLocal ["memcpy-structs"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: `memcpy` should not be used for structs like `\ESC[0;32mMy_Struct\ESC[0m`; use assignment instead [-Wmemcpy-structs]"
            ]

    it "should not give diagnostics on valid memset calls" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  memset(a, 0, 10);"
            , "}"
            ]
        analyseLocal ["memcpy-structs"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on invalid memset calls" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  memset(a, 0, sizeof(My_Struct));"
            , "}"
            ]
        analyseLocal ["memcpy-structs"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: `memset` should not be used for structs like `\ESC[0;32mMy_Struct\ESC[0m`; use `(Type) {0}` instead [-Wmemcpy-structs]"
            ]
