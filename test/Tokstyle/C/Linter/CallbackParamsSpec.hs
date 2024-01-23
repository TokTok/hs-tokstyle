{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.CallbackParamsSpec (spec) where

import           Test.Hspec            (Spec, it, shouldBe)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings, analyse)
import           Tokstyle.C.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "warns when callback param names don't match the callback type's param names" $ do
        ast <- mustParse
            [ "typedef void some_cb(int foo, char bar);"
            , "void some_handler(int boo, char bar);"
            , "void invoke(some_cb *handler, int blep, char bork);"
            , "void wrong(void) {"
            , "  invoke(some_handler, 123, 'a');"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:5: (column 10) [ERROR]  >>> AST invariant violated"
                , "  parameter 1 of some_handler is named `boo`, but in callback type `some_cb *` it is named `foo`"
                ]
            ]
