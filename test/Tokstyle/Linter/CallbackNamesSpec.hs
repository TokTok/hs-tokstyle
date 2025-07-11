{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CallbackNamesSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on valid callback names" $ do
        ast <- mustParse
            [ "typedef void bar_cb(void);"
            , "void foo(bar_cb *bar_callback);"
            ]
        analyseLocal ["callback-names"] ("test.h", ast)
            `shouldBe`
            []

    it "should give diagnostics on invalid callback names" $ do
        ast <- mustParse
            [ "typedef void bar_cb(void);"
            , "void foo(bar_cb *bar);"
            ]
        analyseLocal ["callback-names"] ("test.h", ast)
            `shouldBe`
            [ "test.h:2: function pointer `bar` should end in `callback` [-Wcallback-names]"
            ]

    it "should handle various allowed suffixes" $ do
        ast <- mustParse
            [ "typedef void foo_cb(void);"
            , "void foo(foo_cb *bar_callback, foo_cb *bar_function, foo_cb *bar_handler);"
            ]
        analyseLocal ["callback-names"] ("test.h", ast)
            `shouldBe`
            []
