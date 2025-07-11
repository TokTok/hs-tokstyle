{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.LoggerConstSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on valid logger declarations" $ do
        ast <- mustParse
            [ "void foo(const Logger *log);"
            ]
        analyseLocal ["logger-const"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on invalid logger declarations" $ do
        ast <- mustParse
            [ "void foo(Logger *log);"
            ]
        analyseLocal ["logger-const"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: Logger parameter should be pointer-to-const [-Wlogger-const]"
            ]

    it "should not give diagnostics on struct members" $ do
        ast <- mustParse
            [ "struct Foo {"
            , "  Logger *log;"
            , "};"
            ]
        analyseLocal ["logger-const"] ("test.c", ast)
            `shouldBe`
            []

    it "should not give diagnostics on variable declarations" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  Logger *log;"
            , "}"
            ]
        analyseLocal ["logger-const"] ("test.c", ast)
            `shouldBe`
            []
