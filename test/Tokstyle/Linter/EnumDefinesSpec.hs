{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.EnumDefinesSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (allWarnings, analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "suggests using enums for long sequences of #defines" $ do
        ast <- mustParse
            [ "#define FOO_BAR_ONE 1"
            , "#define FOO_BAR_TWO 2"
            , "#define FOO_BAR_THREE 3"
            , "#define FOO_BAR_FOUR 4"
            , "#define FOO_BAR_FIVE 5"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:5: sequence of `#define`s longer than 5 could be written as `enum Foo_Bar` [-Wenum-defines]"
            ]

    it "allows comments to be interspersed in the enum" $ do
        ast <- mustParse
            [ "#define FOO_BAR_ONE 1"
            , "#define FOO_BAR_TWO 2"
            , "// some comment here"
            , "#define FOO_BAR_THREE 3"
            , "/* another comment here */"
            , "#define FOO_BAR_FOUR 4"
            , "#define FOO_BAR_FIVE 5"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:7: sequence of `#define`s longer than 5 could be written as `enum Foo_Bar` [-Wenum-defines]"
            ]

    it "ignores broken sequences" $ do
        ast <- mustParse
            [ "#define FOO_BAR_ONE 1"
            , "#define FOO_BAR_TWO 2"
            , "static const uint32_t xxx = 10;"  -- breaks the sequence, we ignore this because it doesn't look like an enum
            , "#define FOO_BAR_THREE 3"
            , "#define FOO_BAR_FOUR 4"
            , "#define FOO_BAR_FIVE 5"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe` []

    it "ignores defines with large values" $ do
        ast <- mustParse
            [ "#define FOO_BAR_ONE 1"
            , "#define FOO_BAR_TWO 0x20"
            , "#define FOO_BAR_THREE 300"
            , "#define FOO_BAR_FOUR 4"
            , "#define FOO_BAR_FIVE 5"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe` []
