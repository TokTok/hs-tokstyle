{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CallgraphSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseGlobal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should give diagnostic on unused macros" $ do
        ast <- mustParse
            [ "#define SIZE 10"
            ]
        analyseGlobal ["callgraph"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:1: unused symbol `SIZE` [-Wcallgraph]"
            ]

    it "should not give diagnostics on symbols used in array dimensions" $ do
        ast <- mustParse
            [ "#define SIZE 10"
            , "struct Foo {"
            , "  char c[SIZE];"
            , "};"
            , "int main() { Foo foo; }"
            ]
        analyseGlobal ["callgraph"] [("test.c", ast)]
            `shouldBe`
            []

    it "should give diagnostics on undefined symbols used in array dimensions" $ do
        ast <- mustParse
            [ "typedef struct Foo {"
            , "  char c[SIZE];"
            , "} Foo;"
            , "int main() { Foo foo; }"
            ]
        analyseGlobal ["callgraph"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:2: definition of `Foo` references undefined global function/constant `SIZE` [-Wcallgraph]"
            ]
