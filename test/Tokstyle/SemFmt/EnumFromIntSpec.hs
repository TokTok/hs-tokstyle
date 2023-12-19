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
            , "Foo foo_from_int(int b) {"
            , "  switch (b) {"
            , "    case FOO_ONE: return FOO_ONE;"
            , "    case FOO_TWO: return FOO_ONE;"
            , "    default: return FOO_ONE;"
            , "  }"
            , "}"
            ]
        expected <- render . ppTranslationUnit . (:[]) <$> mustParseStmt
            [ "{"
            , "  switch (b) {"
            , "    case FOO_ONE: return FOO_ONE;"
            , "    case FOO_TWO: return FOO_TWO;"
            , "    default: return FOO_ONE;"
            , "  }"
            , "}"
            ]
        analyseGlobal ["enum-from-int"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:5: enum `_from_int` function for `Foo` should be:\n"
              <> expected <> " [-Wenum-from-int]"
            ]
