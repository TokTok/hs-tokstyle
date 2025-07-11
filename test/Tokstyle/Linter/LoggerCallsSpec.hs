{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.LoggerCallsSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on valid logger calls" $ do
        ast <- mustParse
            [ "void foo(Logger *log) {"
            , "  LOGGER_INFO(log, \"foo\");"
            , "  LOGGER_INFO(log, \"foo\", 1);"
            , "}"
            ]
        analyseLocal ["logger-calls"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on invalid logger calls" $ do
        ast <- mustParse
            [ "void foo(Logger *log) {"
            , "  const char *foo = \"foo\";"
            , "  LOGGER_INFO(log, foo);"
            , "}"
            ]
        analyseLocal ["logger-calls"] ("test.c", ast)
            `shouldBe`
            [ "test.c:3: logger call `LOGGER_INFO` has a non-literal format argument [-Wlogger-calls]"
            ]

    it "should not give diagnostics on LOGGER_ASSERT" $ do
        ast <- mustParse
            [ "void foo(Logger *log) {"
            , "  LOGGER_ASSERT(log, 1, \"foo\");"
            , "}"
            ]
        analyseLocal ["logger-calls"] ("test.c", ast)
            `shouldBe`
            []
