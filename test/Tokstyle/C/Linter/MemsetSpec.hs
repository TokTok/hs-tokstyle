{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.MemsetSpec (spec) where

import           Test.Hspec            (Spec, it, shouldBe)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings, analyse)
import           Tokstyle.C.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should warn about passing structs with pointers to memset" $ do
        ast <- mustParse
            [ "void *memset(void *s, int c, unsigned int n);"
            , "struct Foo { char *p; };"
            , "void f(void) {"
            , "  struct Foo foo;"
            , "  struct Foo foos[10];"
            , "  memset(&foo, 0, sizeof(foo));"
            , "  memset(foos, 0, sizeof(foos));"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:6: (column 10) [ERROR]  >>> Type mismatch"
                , "  disallowed memset argument `&foo` of type `struct Foo *`, which contains pointers"
                ]
            , Text.unlines
                [ "test.c:7: (column 10) [ERROR]  >>> Type mismatch"
                , "  disallowed memset argument `foos` of type `struct Foo [10]`, which contains pointers"
                ]
            ]

    it "should detect nested structs" $ do
        ast <- mustParse
            [ "void *memset(void *s, int c, unsigned int n);"
            , "typedef struct Foo { char *p; } Foo;"
            , "struct Bar { Foo foo; };"
            , "void f(void) {"
            , "  struct Bar bar;"
            , "  memset(&bar, 0, sizeof(bar));"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:6: (column 10) [ERROR]  >>> Type mismatch"
                , "  disallowed memset argument `&bar` of type `struct Bar *`, which contains pointers"
                ]
            ]

    it "should not warn about structs without pointers" $ do
        ast <- mustParse
            [ "void *memset(void *s, int c, unsigned int n);"
            , "typedef struct Foo { char c; } Foo;"
            , "struct Bar { Foo foo; };"
            , "void f(void) {"
            , "  struct Bar bar;"
            , "  memset(&bar, 0, sizeof(bar));"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe` []
