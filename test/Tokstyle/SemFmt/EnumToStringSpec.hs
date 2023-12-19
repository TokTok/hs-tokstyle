{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.SemFmt.EnumToStringSpec where

import           Test.Hspec             (Spec, it, shouldBe)

import           Language.Cimple.Pretty (ppTranslationUnit, render)

import           Tokstyle.Linter        (analyseGlobal)
import           Tokstyle.LinterSpec    (mustParse, mustParseStmt)


spec :: Spec
spec = do
    it "should give diagnostics on incorrect to_string function" $ do
        ast <- mustParse
            [ "typedef enum Foo {"
            , "  FOO_ONE,"
            , "  FOO_TWO,"
            , "} Foo;"
            , "const char *foo_to_string(Foo value) {"
            , "  switch (value) {"
            , "    case FOO_ONE: return \"FOO_ONE\";"
            , "    case FOO_TWO: return \"FOO_ONE\";"
            , "  }"
            , "  return \"<invalid>\";"
            , "}"
            ]
        expected <- render . ppTranslationUnit . (:[]) <$> mustParseStmt
            [ "{"
            , "  switch (value) {"
            , "    case FOO_ONE: return \"FOO_ONE\";"
            , "    case FOO_TWO: return \"FOO_TWO\";"
            , "  }"
            , "  return \"<invalid Foo>\";"
            , "}"
            ]
        analyseGlobal ["enum-to-string"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:5: enum `_to_string` function for `Foo` should be:\n"
              <> expected <> " [-Wenum-to-string]"
            ]
