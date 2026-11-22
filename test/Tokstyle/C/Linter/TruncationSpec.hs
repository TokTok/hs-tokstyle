{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.TruncationSpec (spec) where
import           Data.Text             (Text)
import           Test.Hspec            (Spec, it)

import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["truncation"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["truncation"]


-- | The line markers put the declaration of @tox_set_name@ in a public header,
-- as the preprocessor does.
prelude :: [Text]
prelude =
    [ "typedef unsigned short uint16_t;"
    , "typedef unsigned long size_t;"
    , "int set_name(const char *name, uint16_t length);"
    , "int copy_name(const char *name, size_t length);"
    , "int api_set_name(const char *name, size_t length);"
    , "# 1 \"tox.h\""
    , "int tox_set_name(const char *name, size_t length);"
    , "# 9 \"test.c\""
    ]


helpers :: [Text]
helpers =
    [ "uint16_t min_u16(uint16_t a, uint16_t b) { return a < b ? a : b; }"
    , "uint16_t half(uint16_t a) { return a / 2; }"
    ]


spec :: Spec
spec = do
    it "warns about a public function's length passed as a narrower parameter" $ do
        shouldWarn' (prelude ++
            [ "int tox_set_name(const char *name, size_t length) {"
            , "  return set_name(name, length);"
            , "}"
            ])
            [[ "error: `length` can be 18446744073709551615, which doesn't fit in the 16-bit parameter 2 of `set_name` [-Wtruncation]"
             , "   --> test.c:10:25"
             , "    |"
             , "  10|   return set_name(name, length);"
             , "    |                         ^^^^^^"
             ]]

    it "warns about a public function's length stored in a narrower variable" $ do
        shouldWarn' (prelude ++
            [ "int tox_set_name(const char *name, size_t length) {"
            , "  const uint16_t size = length;"
            , "  return set_name(name, size);"
            , "}"
            ])
            [[ "error: `length` can be 18446744073709551615, which doesn't fit in the 16-bit variable `size` it is stored in [-Wtruncation]"
             , "   --> test.c:10:25"
             , "    |"
             , "  10|   const uint16_t size = length;"
             , "    |                         ^^^^^^"
             ]]

    it "accepts a length checked before it is passed" $ do
        shouldAccept' (prelude ++
            [ "int tox_set_name(const char *name, size_t length) {"
            , "  if (length > 128) {"
            , "    return -1;"
            , "  }"
            , "  return set_name(name, length);"
            , "}"
            ])

    it "accepts a length passed as a parameter that is as wide" $ do
        shouldAccept' (prelude ++
            [ "int tox_set_name(const char *name, size_t length) {"
            , "  return copy_name(name, length);"
            , "}"
            ])

    it "accepts a function no public header declares" $ do
        shouldAccept' (prelude ++
            [ "int api_set_name(const char *name, size_t length) {"
            , "  return set_name(name, length);"
            , "}"
            ])

    it "warns about a sum stored in a narrower variable and compared after" $ do
        shouldWarn' (prelude ++ helpers ++
            [ "int send(const char *data, uint16_t length) {"
            , "  const uint16_t size = length + 32;"
            , "  if (size > 1024) {"
            , "    return -1;"
            , "  }"
            , "  return set_name(data, size);"
            , "}"
            ])
            [[ "error: `length + 32` can be 65567, which doesn't fit in the 16-bit variable `size` it is stored in before it is compared [-Wtruncation]"
             , "   --> test.c:12:25"
             , "    |"
             , "  12|   const uint16_t size = length + 32;"
             , "    |                         ^^^^^^^^^^^"
             ]]

    it "warns about a product cut before a function like min compares it" $ do
        shouldWarn' (prelude ++ helpers ++
            [ "uint16_t interval(unsigned int peers) {"
            , "  const uint16_t seconds = peers * 50;"
            , "  return min_u16(3600, seconds);"
            , "}"
            ])
            [[ "error: `peers * 50` can be 4294967295, which doesn't fit in the 16-bit variable `seconds` it is stored in before it is compared [-Wtruncation]"
             , "   --> test.c:12:28"
             , "    |"
             , "  12|   const uint16_t seconds = peers * 50;"
             , "    |                            ^^^^^^^^^^"
             ]]

    it "warns about a product passed to a function like min" $ do
        shouldWarn' (prelude ++ helpers ++
            [ "uint16_t interval(unsigned int peers) {"
            , "  return min_u16(3600, peers * 50);"
            , "}"
            ])
            [[ "error: `peers * 50` can be 4294967295, which doesn't fit in the 16-bit parameter 2 of `min_u16`, which compares it [-Wtruncation]"
             , "   --> test.c:12:24"
             , "    |"
             , "  12|   return min_u16(3600, peers * 50);"
             , "    |                        ^^^^^^^^^^"
             ]]

    it "accepts a sum that is cut and never compared" $ do
        shouldAccept' (prelude ++ helpers ++
            [ "uint16_t next(uint16_t sequence) {"
            , "  const uint16_t after = sequence + 1;"
            , "  return half(after);"
            , "}"
            ])

    it "accepts a sum compared before it is cut" $ do
        shouldAccept' (prelude ++ helpers ++
            [ "int send(const char *data, uint16_t length) {"
            , "  const unsigned int size = length + 32;"
            , "  if (size > 1024) {"
            , "    return -1;"
            , "  }"
            , "  return set_name(data, (uint16_t)size);"
            , "}"
            ])

    it "accepts a sum of a field the program keeps small, in files with absolute paths" $ do
        shouldAccept' (prelude ++
            [ "# 1 \"/src/project/entry.h\""
            , "typedef struct Entry { unsigned int length; } Entry;"
            , "# 1 \"/src/project/entry.c\""
            , "void store(Entry *e, unsigned int length) {"
            , "  if (length > 512) {"
            , "    return;"
            , "  }"
            , "  e->length = length;"
            , "}"
            , "int reply(const Entry *e, uint16_t max) {"
            , "  const uint16_t size = 33 + e->length;"
            , "  if (max < size) {"
            , "    return -1;"
            , "  }"
            , "  return size;"
            , "}"
            ])

    it "warns about a sum of a field of a struct from a system header" $ do
        shouldWarn' (prelude ++
            [ "# 1 \"/usr/include/entry.h\""
            , "typedef struct Entry { unsigned int length; } Entry;"
            , "# 12 \"test.c\""
            , "void store(Entry *e, unsigned int length) {"
            , "  if (length > 512) {"
            , "    return;"
            , "  }"
            , "  e->length = length;"
            , "}"
            , "int reply(const Entry *e, uint16_t max) {"
            , "  const uint16_t size = 33 + e->length;"
            , "  if (max < size) {"
            , "    return -1;"
            , "  }"
            , "  return size;"
            , "}"
            ])
            [[ "error: `33 + e->length` can be 4294967295, which doesn't fit in the 16-bit variable `size` it is stored in before it is compared [-Wtruncation]"
             , "   --> test.c:19:25"
             , "    |"
             , "  19|   const uint16_t size = 33 + e->length;"
             , "    |                         ^^^^^^^^^^^^^^"
             ]]

    it "accepts a sum of values the conditions keep small" $ do
        shouldAccept' (prelude ++ helpers ++
            [ "int send(const char *data, uint16_t length) {"
            , "  if (length > 1000) {"
            , "    return -1;"
            , "  }"
            , "  const uint16_t size = length + 32;"
            , "  if (size > 1024) {"
            , "    return -1;"
            , "  }"
            , "  return set_name(data, size);"
            , "}"
            ])
