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
