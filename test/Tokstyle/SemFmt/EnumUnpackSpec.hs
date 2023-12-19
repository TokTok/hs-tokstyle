{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.SemFmt.EnumUnpackSpec where

import           Test.Hspec             (Spec, it, shouldBe)

import           Language.Cimple.Pretty (ppTranslationUnit, render)

import           Tokstyle.Linter        (analyseGlobal)
import           Tokstyle.LinterSpec    (mustParse, mustParseStmt)


spec :: Spec
spec = do
    it "should give diagnostics on incorrect unpack function" $ do
        ast <- mustParse
            [ "typedef enum Foo {"
            , "  FOO_ONE,"
            , "  FOO_TWO,"
            , "} Foo;"
            , "bool foo_unpack(Bin_Unpack *bu, Foo *val) {"
            , "  return true;"
            , "}"
            ]
        expected <- render . ppTranslationUnit . (:[]) <$> mustParseStmt
            [ "{"
            , "    uint32_t u32;"
            , "    return bin_unpack_u32(bu, &u32)"
            , "        && foo_from_int(u32, val);"
            , "}"
            ]
        analyseGlobal ["enum-unpack"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:5: enum `_unpack` function for `Foo` should be:\n"
              <> expected <> " [-Wenum-unpack]"
            ]
