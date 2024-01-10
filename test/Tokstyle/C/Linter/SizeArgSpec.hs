{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.SizeArgSpec (spec) where

import           Test.Hspec            (Spec, it, shouldBe)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings, analyse)
import           Tokstyle.C.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "warns when constant size argument is not the array size" $ do
        ast <- mustParse
            [ "void consume(char *arr, int size);"
            , "void caller(void) {"
            , "  char arr[12];"
            , "  consume(arr, 13);"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:4: (column 16) [ERROR]  >>> Type mismatch"
                , "  size parameter `size` is passed constant value `13` (= 13),"
                , "  which is greater than the array size of `char [12]`,"
                , "  potentially causing buffer overrun in `consume`"
                ]
            ]

    it "can see through enum constants" $ do
        ast <- mustParse
            [ "enum { SIZE = 12 };"
            , "void consume(char *arr, int size);"
            , "void caller(void) {"
            , "  char arr[SIZE];"
            , "  consume(arr, SIZE + 1);"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:5: (column 16) [ERROR]  >>> Type mismatch"
                , "  size parameter `size` is passed constant value `SIZE + 1` (= 13),"
                , "  which is greater than the array size of `char [SIZE]`,"
                , "  potentially causing buffer overrun in `consume`"
                ]
            ]

    it "can see through typedefs" $ do
        ast <- mustParse
            [ "enum { SIZE = 12 };"
            , "typedef unsigned int size_t;"
            , "void consume(char *arr, size_t size);"
            , "void caller(void) {"
            , "  char arr[SIZE];"
            , "  consume(arr, SIZE + 1);"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:6: (column 16) [ERROR]  >>> Type mismatch"
                , "  size parameter `size` is passed constant value `SIZE + 1` (= 13),"
                , "  which is greater than the array size of `char [SIZE]`,"
                , "  potentially causing buffer overrun in `consume`"
                ]
            ]

    it "can see through array typedefs" $ do
        ast <- mustParse
            [ "typedef char My_Array[12];"
            , "void consume(char *arr, int size);"
            , "void caller(void) {"
            , "  My_Array arr;"
            , "  consume(arr, 13);"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:5: (column 16) [ERROR]  >>> Type mismatch"
                , "  size parameter `size` is passed constant value `13` (= 13),"
                , "  which is greater than the array size of `char [12]`,"
                , "  potentially causing buffer overrun in `consume`"
                ]
            ]

    it "can see through function typedefs" $ do
        ast <- mustParse
            [ "typedef void consume_cb(char *arr, int size);"
            , "consume_cb consume;"
            , "void caller(void) {"
            , "  char arr[12];"
            , "  consume(arr, 13);"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:5: (column 16) [ERROR]  >>> Type mismatch"
                , "  size parameter `size` is passed constant value `13` (= 13),"
                , "  which is greater than the array size of `char [12]`,"
                , "  potentially causing buffer overrun in `consume`"
                ]
            ]

    it "works on function pointers" $ do
        ast <- mustParse
            [ "typedef void consume_cb(char *arr, int size);"
            , "void caller(consume_cb *consume) {"
            , "  char arr[12];"
            , "  consume(arr, 13);"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:4: (column 16) [ERROR]  >>> Type mismatch"
                , "  size parameter `size` is passed constant value `13` (= 13),"
                , "  which is greater than the array size of `char [12]`,"
                , "  potentially causing buffer overrun in `consume`"
                ]
            ]

    it "works on array parameters" $ do
        ast <- mustParse
            [ "typedef void consume_cb(char *arr, int size);"
            , "void caller(consume_cb *consume, char arr[12]) {"
            , "  consume(arr, 13);"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:3: (column 16) [ERROR]  >>> Type mismatch"
                , "  size parameter `size` is passed constant value `13` (= 13),"
                , "  which is greater than the array size of `char [12]`,"
                , "  potentially causing buffer overrun in `consume`"
                ]
            ]

    it "warns about string literal overrun" $ do
        ast <- mustParse
            [ "void consume(char *arr, int size);"
            , "void caller(void) {"
            , "  consume(\"hello world\", 13);"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:3: (column 26) [ERROR]  >>> Type mismatch"
                , "  size parameter `size` is passed constant value `13` (= 13),"
                , "  which is greater than the array size of `char [static 11]`,"
                , "  potentially causing buffer overrun in `consume`"
                ]
            ]

    it "ignores calls where the parameter name does not indicate it's a size" $ do
        ast <- mustParse
            [ "typedef char My_Array[12];"
            , "void consume(char *file, int line);"
            , "void caller(void) {"
            , "  consume(\"hello.c\", 123);"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe` []
