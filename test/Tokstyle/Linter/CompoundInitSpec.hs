{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CompoundInitSpec (spec) where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "detects compound literal initialisers" $ do
        ast <- mustParse
            [ "void f(void) {"
            , "  Foo foo = (Foo){0};"
            , "}"
            ]
        analyseLocal ["compound-init"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: don't use compound literals in initialisations; use simple `Type var = {0};` [-Wcompound-init]"
            ]

    it "accepts aggregate initialisers" $ do
        ast <- mustParse
            [ "void f(void) {"
            , "  Foo foo = {0};"
            , "}"
            ]
        analyseLocal ["compound-init"] ("test.c", ast)
            `shouldBe` []
