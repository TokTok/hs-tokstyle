{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.BooleansSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (allWarnings, analyse)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should give diagnostics on simplifiable if-return" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  if (b == 0) { return true; }"
            , "  return false;"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: if-statement followed by boolean return can be simplified to return [-Wbooleans]"
            ]

    it "should give diagnostics on simplifiable if/else with boolean return" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  if (b == 0) { return true; }"
            , "  else { return false; }"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: if/else with return true/false can be simplified to return [-Wbooleans]"
            ]

    it "should give diagnostics when there are other statements before the violating code" $ do
        ast <- mustParse
            [ "int a(int b) {"
            , "  do_something();"
            , "  if (b == 0) { return true; }"
            , "  return false;"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:3: if-statement followed by boolean return can be simplified to return [-Wbooleans]"
            ]
