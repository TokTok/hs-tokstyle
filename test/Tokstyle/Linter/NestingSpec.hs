{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.NestingSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on valid nesting" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  if (1) {"
            , "    if (2) {"
            , "      if (3) {"
            , "        if (4) {"
            , "          if (5) {"
            , "            if (6) {"
            , "              return;"
            , "            }"
            , "          }"
            , "        }"
            , "      }"
            , "    }"
            , "  }"
            , "}"
            ]
        analyseLocal ["nesting"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on invalid nesting" $ do
        ast <- mustParse
            [ "void foo(void) {"
            , "  if (1) {"
            , "    if (2) {"
            , "      if (3) {"
            , "        if (4) {"
            , "          if (5) {"
            , "            if (6) {"
            , "              if (7) {"
            , "                return;"
            , "              }"
            , "            }"
            , "          }"
            , "        }"
            , "      }"
            , "    }"
            , "  }"
            , "}"
            ]
        analyseLocal ["nesting"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: function is too deeply nested: 8 is deeper than the maximum allowed of 7; consider inversion or extraction [-Wnesting]"
            ]
