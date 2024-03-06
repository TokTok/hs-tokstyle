{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.SemFmt.EnumFromIntSpec where

import           Test.Hspec             (Spec, it, shouldBe)

import           Language.Cimple.Pretty (ppTranslationUnit, render)

import           Tokstyle.Linter        (analyseGlobal)
import           Tokstyle.LinterSpec    (mustParse, mustParseStmt)


spec :: Spec
spec = do
    it "should give diagnostics on incorrect from_int function" $ do
        ast <- mustParse
            [ "typedef enum Foo {"
            , "  FOO_ONE,"
            , "  FOO_TWO,"
            , "} Foo;"
            , "bool foo_from_int(int b, Foo *out_enum) {"
            , "  switch (b) {"
            , "    case FOO_ONE: {"
            , "      *out_enum = FOO_ONE;"
            , "      return true;"
            , "    }"
            , "    case FOO_TWO: {"
            , "      *out_enum = FOO_ONE;"  -- mistake here
            , "      return true;"
            , "    }"
            , "    default: {"
            , "      *out_enum = FOO_ONE;"
            , "      return false;"
            , "    }"
            , "  }"
            , "}"
            ]
        expected <- render . ppTranslationUnit . (:[]) <$> mustParseStmt
            [ "{"
            , "  switch (b) {"
            , "    case FOO_ONE: {"
            , "      *out_enum = FOO_ONE;"
            , "      return true;"
            , "    }"
            , "    case FOO_TWO: {"
            , "      *out_enum = FOO_TWO;"
            , "      return true;"
            , "    }"
            , "    default: {"
            , "      *out_enum = FOO_ONE;"
            , "      return false;"
            , "    }"
            , "  }"
            , "}"
            ]
        analyseGlobal ["enum-from-int"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:5: enum `_from_int` function for `Foo` should be:\n"
              <> expected <> " [-Wenum-from-int]"
            ]

    it "should not give diagnostics on correct from_int function" $ do
        ast <- mustParse
            [ "typedef enum Foo {"
            , "  FOO_ONE,"
            , "  FOO_TWO,"
            , "} Foo;"
            , "bool foo_from_int(int b, Foo *out_enum) {"
            , "  switch (b) {"
            , "    case FOO_ONE: {"
            , "      *out_enum = FOO_ONE;"
            , "      return true;"
            , "    }"
            , "    case FOO_TWO: {"
            , "      *out_enum = FOO_TWO;"
            , "      return true;"
            , "    }"
            , "    default: {"
            , "      *out_enum = FOO_ONE;"
            , "      return false;"
            , "    }"
            , "  }"
            , "}"
            ]
        analyseGlobal ["enum-from-int"] [("test.c", ast)]
            `shouldBe` []
