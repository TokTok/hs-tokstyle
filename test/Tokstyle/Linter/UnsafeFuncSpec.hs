{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.UnsafeFuncSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on safe functions" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  snprintf(buf, 5, \"foo\");"
            , "}"
            ]
        analyseLocal ["unsafe-func"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on unsafe functions" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  sprintf(buf, \"foo\");"
            , "}"
            ]
        analyseLocal ["unsafe-func"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: function `sprintf` should not be used, because it has no way of bounding the number of characters written; use `snprintf` instead [-Wunsafe-func]"
            ]
