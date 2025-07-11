{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.LoggerNoEscapesSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on valid logger calls" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  LOGGER_INFO(log, \"foo\");"
            , "}"
            ]
        analyseLocal ["logger-no-escapes"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on invalid logger calls" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  LOGGER_INFO(log, \"foo\\n\");"
            , "}"
            ]
        analyseLocal ["logger-no-escapes"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: logger format \"foo\\n\" contains escape sequences (newlines, tabs, or escaped quotes) [-Wlogger-no-escapes]"
            ]

    it "should not give diagnostics on LOGGER_ASSERT" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  LOGGER_ASSERT(log, 1, \"foo\");"
            , "}"
            ]
        analyseLocal ["logger-no-escapes"] ("test.c", ast)
            `shouldBe`
            []
