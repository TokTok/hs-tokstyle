{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.DocCommentsSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseGlobal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on matching doc comments" $ do
        ast1 <- mustParse
            [ "/** @brief A matching foo. */"
            , "int matching_foo(void);"
            ]
        ast2 <- mustParse
            [ "/** @brief A matching foo. */"
            , "int matching_foo(void) { return 0; }"
            ]
        analyseGlobal ["doc-comments"] [("test.h", ast1), ("test.c", ast2)]
            `shouldBe`
            []

    it "should give diagnostics on mismatching doc comments" $ do
        ast1 <- mustParse
            [ "/** @brief A foo. */"
            , "int mismatching_foo(void);"
            ]
        ast2 <- mustParse
            [ "/** @brief A bar. */"
            , "int mismatching_foo(void) { return 0; }"
            ]
        analyseGlobal ["doc-comments"] [("test.h", ast1), ("test.c", ast2)]
            `shouldBe`
            [ "test.h:1: comment on definition of `mismatching_foo` does not match declaration:\n\ESC[0;33m/**\ESC[0m\ESC[0;36m @brief\ESC[0m\ESC[0;33m A\ESC[0m\ESC[0;33m foo\ESC[0m\ESC[0;33m.\ESC[0m\ESC[0;33m*/\ESC[0m\n [-Wdoc-comments]"
            , "test.c:1: mismatching comment found here:\n\ESC[0;33m/**\ESC[0m\ESC[0;36m @brief\ESC[0m\ESC[0;33m A\ESC[0m\ESC[0;33m bar\ESC[0m\ESC[0;33m.\ESC[0m\ESC[0;33m*/\ESC[0m\n [-Wdoc-comments]"
            ]

    it "should not give diagnostics if only one has a doc comment" $ do
        ast1 <- mustParse
            [ "/** @brief A foo. */"
            , "int single_comment_foo(void);"
            ]
        ast2 <- mustParse
            ["int single_comment_foo(void) { return 0; }"]
        analyseGlobal ["doc-comments"] [("test.h", ast1), ("test.c", ast2)]
            `shouldBe`
            []
