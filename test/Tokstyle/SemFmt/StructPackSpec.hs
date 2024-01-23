{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.SemFmt.StructPackSpec where

import           Test.Hspec             (Spec, it, shouldBe)

import           Language.Cimple.Pretty (ppTranslationUnit, render)

import           Tokstyle.Linter        (analyseGlobal)
import           Tokstyle.LinterSpec    (mustParse, mustParseStmt)


spec :: Spec
spec = do
    it "warns about incorrect pack function" $ do
        ast <- mustParse
            [ "typedef enum Some_Enum { SOME_ENUM_FOO, } Some_Enum;"
            , "typedef struct Foo {"
            , "  uint8_t some_byte;"
            , "  uint16_t some_short;"
            , "  uint32_t some_int;"
            , "  uint64_t some_long;"
            , "  Some_Enum type;"
            , "  uint8_t *message;"
            , "  uint32_t message_length;"
            , "  uint8_t key[32];"
            , "} Foo;"
            , "bool foo_pack(const Foo *foo, Bin_Pack *bp)"
            , "{"
            , "    return bin_pack_array(bp, 5)"
            , "           && bin_pack_u08(bp, foo->some_byte)"
            , "           && bin_pack_u16(bp, foo->some_short)"
            , "           && some_enum_pack(foo->type, bp)"
            , "           && bin_pack_bin(bp, foo->message, foo->message_length)"
            , "           && bin_pack_bin(bp, foo->key, 32);"
            , "}"
            ]
        expected <- render . ppTranslationUnit . (:[]) <$> mustParseStmt
            [ "{"
            , "    return bin_pack_array(bp, 7)"
            , "           && bin_pack_u08(bp, foo->some_byte)"
            , "           && bin_pack_u16(bp, foo->some_short)"
            , "           && bin_pack_u32(bp, foo->some_int)"
            , "           && bin_pack_u64(bp, foo->some_long)"
            , "           && some_enum_pack(foo->type, bp)"
            , "           && bin_pack_bin(bp, foo->message, foo->message_length)"
            , "           && bin_pack_bin(bp, foo->key, 32);"
            , "}"
            ]
        analyseGlobal ["struct-pack"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:12: struct `_pack` function for `Foo` should be:\n"
              <> expected <> " [-Wstruct-pack]"
            ]
