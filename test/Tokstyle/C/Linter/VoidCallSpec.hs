{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.VoidCallSpec (spec) where

import           Test.Hspec            (Spec, it, shouldBe)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings, analyse)
import           Tokstyle.C.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "warns if there is code before the cast-from-void" $ do
        ast <- mustParse
            [ "void add_handler(int *a, void *user_data) {"
            , "  if (user_data == 0) { return; }"  -- this should be done *after* the cast
            , "  const int *b = (const int *)user_data;"
            , "  *a += *b;"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:3: (column 18) [ERROR]  >>> AST invariant violated"
                , "  first statement must cast `void *user_data` to `const int *`"
                ]
            ]

    it "warns if there is code before the cast-from-void in multi-vptr function" $ do
        ast <- mustParse
            [ "void add_handler(void *obj, void *user_data) {"
            , "  if (user_data == 0) { return; }"  -- this should be done *after* the cast
            , "  int *b = (int *)obj;"
            , "  *b += 3;"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:3: (column 12) [ERROR]  >>> AST invariant violated"
                , "  first statement must cast `void *obj` to `int *`"
                ]
            ]

    it "doesn't warn if there is no cast-from-void" $ do
        ast <- mustParse
            [ "void add_handler(int *a, void *user_data) {"
            , "  *a += 3;"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe` []

    it "accepts the correct handler function format" $ do
        ast <- mustParse
            [ "void add_handler(int *a, void *user_data) {"
            , "  int *b = (int *)user_data;"
            , "  *a += *b;"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe` []

    it "accepts multiple differently-typed void pointer arguments" $ do
        ast <- mustParse
            [ "void add_handler(void *va, void *vb) {"
            , "  long *a = (long *)va;"
            , "  int *b = (int *)vb;"
            , "  *a += *b;"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe` []

    it "allows the declaration to be const-qualified" $ do
        ast <- mustParse
            [ "void add_handler(int *a, void *user_data) {"
            , "  int *const b = (int *)user_data;"
            , "  *a += *b;"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe` []

    it "warns on cast-from-vptr outside the first declaration statement" $ do
        ast <- mustParse
            [ "void do_something(int *a, int *b);"
            , "void add_handler(int *a, void *user_data) {"
            , "  do_something(a, (int *)user_data);"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:3: (column 19) [ERROR]  >>> AST invariant violated"
                , "  first statement must cast `void *user_data` to `int *`"
                ]
            ]

    it "accepts passing void pointers as-is" $ do
        ast <- mustParse
            [ "void do_something(void *user_data);"
            , "void add_handler(int *a, void *user_data) {"
            , "  do_something(user_data);"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe` []
