{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.DocCommentsSpec (spec) where

import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Test.Hspec             (Spec, it)

import           Language.Cimple.Pretty (plain, ppTranslationUnit, render)
import           Tokstyle.LinterSpec    (mustParse, shouldAccept, shouldWarn)


shouldWarn' :: [(FilePath, [Text])] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["doc-comments"]


shouldAccept' :: [(FilePath, [Text])] -> IO ()
shouldAccept' = shouldAccept ["doc-comments"]


spec :: Spec
spec = do
    it "should not give diagnostics on matching doc comments" $ do
        shouldAccept'
            [ ("test.h", [ "/** @brief A matching foo. */"
                         , "int matching_foo(void);"
                         ])
            , ("test.c", [ "/** @brief A matching foo. */"
                         , "int matching_foo(void) { return 0; }"
                         ])
            ]

    it "should give diagnostics on mismatching doc comments" $ do
        let code1 = [ "/** @brief A foo. */"
                    , "int mismatching_foo(void);"
                    ]
            code2 = [ "/** @brief A bar. */"
                    , "int mismatching_foo(void) { return 0; }"
                    ]
        expected1Lines <- map Text.stripEnd . Text.lines . render . plain . ppTranslationUnit . take 1 <$> mustParse code1
        expected2Lines <- map Text.stripEnd . Text.lines . render . plain . ppTranslationUnit . take 1 <$> mustParse code2
        let indent l = if Text.null (Text.strip l) then "" else "         " <> l
        shouldWarn' [("test.h", code1), ("test.c", code2)]
            [[ "warning: comment on definition of `mismatching_foo` does not match declaration: [-Wdoc-comments]"
             , Text.intercalate "\n" $ map indent expected1Lines
             , "   --> test.h:1:4"
             , "    |"
             , "   1| /** @brief A foo. */"
             , "    |    ^^^^^^^^^^^^^^^^^"
             ]
            ,[ "warning: mismatching comment found here: [-Wdoc-comments]"
             , Text.intercalate "\n" $ map indent expected2Lines
             , "   --> test.c:1:4"
             , "    |"
             , "   1| /** @brief A bar. */"
             , "    |    ^^^^^^^^^^^^^^^^^"
             ]]

    it "should see functions with an allocation size attribute" $ do
        let code1 = [ "/** @brief A foo. */"
                    , "GNU_ALLOC_SIZE(1) void *alloc_foo(uint32_t size);"
                    ]
            code2 = [ "/** @brief A bar. */"
                    , "void *alloc_foo(uint32_t size) { return nullptr; }"
                    ]
        expected1Lines <- map Text.stripEnd . Text.lines . render . plain . ppTranslationUnit . take 1 <$> mustParse code1
        expected2Lines <- map Text.stripEnd . Text.lines . render . plain . ppTranslationUnit . take 1 <$> mustParse code2
        let indent l = if Text.null (Text.strip l) then "" else "         " <> l
        shouldWarn' [("test.h", code1), ("test.c", code2)]
            [[ "warning: comment on definition of `alloc_foo` does not match declaration: [-Wdoc-comments]"
             , Text.intercalate "\n" $ map indent expected1Lines
             , "   --> test.h:1:4"
             , "    |"
             , "   1| /** @brief A foo. */"
             , "    |    ^^^^^^^^^^^^^^^^^"
             ]
            ,[ "warning: mismatching comment found here: [-Wdoc-comments]"
             , Text.intercalate "\n" $ map indent expected2Lines
             , "   --> test.c:1:4"
             , "    |"
             , "   1| /** @brief A bar. */"
             , "    |    ^^^^^^^^^^^^^^^^^"
             ]]

    it "should not give diagnostics if only one has a doc comment" $ do
        shouldAccept'
            [ ("test.h", [ "/** @brief A foo. */"
                         , "int single_comment_foo(void);"
                         ])
            , ("test.c", ["int single_comment_foo(void) { return 0; }"])
            ]
