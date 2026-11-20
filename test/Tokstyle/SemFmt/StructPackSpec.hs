{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.SemFmt.StructPackSpec where
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Test.Hspec             (Spec, it)

import           Language.Cimple.Pretty (plain, ppTranslationUnit, render)
import           Tokstyle.LinterSpec    (mustParseStmt, shouldAccept,
                                         shouldWarn)


shouldWarn' :: [(FilePath, [Text])] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["struct-pack"]


shouldAccept' :: [(FilePath, [Text])] -> IO ()
shouldAccept' = shouldAccept ["struct-pack"]


testC :: [Text] -> [(FilePath, [Text])]
testC code = [("test.c", code)]


spec :: Spec
spec = do
    it "warns about incorrect pack function" $ do
        let code =
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
        expectedLines <- Text.lines . render . plain . ppTranslationUnit . (:[]) <$> mustParseStmt
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
        let indent l = if Text.null (Text.strip l) then "" else "         " <> l
        shouldWarn' (testC code)
            [[ "warning: struct `_pack` function for `Foo` should be: [-Wstruct-pack]"
                , Text.intercalate "\n" $ map indent expectedLines
                , "   --> test.c:12:1"
                , "    |"
                , "  12| bool foo_pack(const Foo *foo, Bin_Pack *bp)"
                , "    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
                ]]

    it "packs a typedef'd integer as its integer" $ do
        shouldAccept' (testC
            [ "typedef uint16_t Foo_Port;"
            , "typedef struct Foo {"
            , "  uint8_t some_byte;"
            , "  Foo_Port port;"
            , "} Foo;"
            , "bool foo_pack(const Foo *foo, Bin_Pack *bp)"
            , "{"
            , "    return bin_pack_array(bp, 2)"
            , "           && bin_pack_u08(bp, foo->some_byte)"
            , "           && bin_pack_u16(bp, foo->port);"
            , "}"
            ])
