{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.EnumNamesSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (allWarnings, analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on valid enum names" $ do
        ast <- mustParse
            [ "enum My_Enum {"
            , "  MY_ENUM_FOO,"
            , "  MY_ENUM_BAR"
            , "};"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on invalid enum names" $ do
        ast <- mustParse
            [ "enum My_Enum {"
            , "  FOO,"
            , "  MY_ENUM_BAR"
            , "};"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: enumerator `FOO` in enum `My_Enum` should start with `MY_ENUM_` [-Wenum-names]"
            ]

    it "should handle _T suffix correctly" $ do
        ast <- mustParse
            [ "typedef enum My_Enum_T {"
            , "  MY_ENUM_FOO,"
            , "  MY_ENUM_BAR"
            , "} My_Enum_T;"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            []

    it "should not check exempted enums" $ do
        ast <- mustParse
            [ "enum Friend_Status {"
            , "  FRIEND_ONLINE,"
            , "  FRIEND_OFFLINE"
            , "};"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on invalid enum names in typedef" $ do
        ast <- mustParse
            [ "typedef enum My_Enum {"
            , "  FOO,"
            , "  MY_ENUM_BAR"
            , "} My_Enum;"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: enumerator `FOO` in enum `My_Enum` should start with `MY_ENUM_` [-Wenum-names]"
            ]
