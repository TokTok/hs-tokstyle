{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.NullabilitySpec where

import           Test.Hspec          (Spec, describe, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    describe "warnings" $ do
        it "warns when a nullable pointer is cast to a nonnull pointer without check" $ do
            ast <- mustParse
                [ "int *_Nonnull my_func(int *_Nullable p) {"
                , "  return (int *_Nonnull)p;"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe`
                [ "test.c:2: expression `p` is nullable and has not been checked before this cast [-Wnullability]"
                ]

        it "warns when a nullable pointer is cast to a nonnull pointer without check, in a function call" $ do
            ast <- mustParse
                [ "void my_other_func(int *_Nonnull p);"
                , "void my_func(int *_Nullable p) {"
                , "   my_other_func((int *_Nonnull)p);"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe`
                [ "test.c:3: expression `p` is nullable and has not been checked before this cast [-Wnullability]"
                ]

        it "warns when a checked pointer is re-assigned from a nullable source before cast" $ do
            ast <- mustParse
                [ "void my_other_func(int *_Nonnull p);"
                , "void my_func(int *_Nullable p, int *_Nullable q) {"
                , "   if (p != nullptr) {"
                , "     p = q; // p is no longer guaranteed to be non-null"
                , "     my_other_func((int *_Nonnull)p);"
                , "   }"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe`
                [ "test.c:5: expression `p` is nullable and has not been checked before this cast [-Wnullability]"
                ]

        it "warns when a nullable struct field is cast to nonnull" $ do
            ast <- mustParse
                [ "typedef struct Foo { int *_Nullable v; } Foo;"
                , "void my_void_func(int *_Nonnull p);"
                , "void my_func(Foo *_Nonnull foo) {"
                , "   my_void_func((int *_Nonnull)foo->v);"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe`
                [ "test.c:4: expression `foo->v` is nullable and has not been checked before this cast [-Wnullability]"
                ]

    describe "acceptance" $ do
        it "does not warn when a nonnull pointer is cast to a nullable pointer" $ do
            ast <- mustParse
                [ "int *_Nullable my_func(int *_Nonnull p) {"
                , "   return (int *_Nullable)p;"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe` []

        it "does not warn when a nonnull pointer is cast to a nonnull pointer" $ do
            ast <- mustParse
                [ "int *_Nonnull my_func(int *_Nonnull p) {"
                , "   return (int *_Nonnull)p;"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe` []

        it "does not warn when a nullable pointer is cast to a nonnull pointer with a preceding if-check" $ do
            ast <- mustParse
                [ "void my_other_func(int *_Nonnull p);"
                , "void my_func(int *_Nullable p) {"
                , "   if (p == nullptr) { return; }"
                , "   my_other_func((int *_Nonnull)p);"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe` []

        it "does not warn when a nullable pointer is cast to a nonnull pointer inside an if-statement checking for non-nullness" $ do
            ast <- mustParse
                [ "void my_other_func(int *_Nonnull p);"
                , "void my_func(int *_Nullable p) {"
                , "   if (p != nullptr) { my_other_func((int *_Nonnull)p); }"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe` []

        it "does not warn when a cast occurs in an else-block after a null-check" $ do
            ast <- mustParse
                [ "void my_other_func(int *_Nonnull p);"
                , "void my_func(int *_Nullable p) {"
                , "   if (p == nullptr) { return; } else { my_other_func((int *_Nonnull)p); }"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe` []

        it "does not warn when a cast is on the true-branch of a ternary operator" $ do
            ast <- mustParse
                [ "int my_other_func(int *_Nonnull p);"
                , "int my_func(int *_Nullable p) {"
                , "   return p ? my_other_func((int *_Nonnull)p) : 0;"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe` []

        it "supports struct field access for nullness checks" $ do
            ast <- mustParse
                [ "typedef struct Foo { int *_Nullable v; } Foo;"
                , "void my_other_func(int *_Nonnull p);"
                , "void my_func(Foo *_Nonnull foo) {"
                , "   if (foo->v != nullptr) { my_other_func((int *_Nonnull)foo->v); }"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe` []

        it "supports operator && as a context for nullness having been checked" $ do
            ast <- mustParse
                [ "typedef struct Foo { int *_Nullable v; } Foo;"
                , "bool my_bool_func(int *_Nonnull p);"
                , "int my_func(Foo *_Nonnull foo) {"
                , "   if (foo->v != nullptr && my_bool_func((int *_Nonnull)foo->v)) { return 123; }"
                , "   return 0;"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe` []

        it "does not warn when a provably non-null assignment of an address occurs in the null branch" $ do
            ast <- mustParse
                [ "void my_other_func(int *_Nonnull p);"
                , "void my_func(int *_Nullable p) {"
                , "   int i = 0;"
                , "   if (p == nullptr) { p = &i; }"
                , "   my_other_func((int *_Nonnull)p);"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe` []

        it "does not warn when a provably non-null assignment of an array occurs in the null branch" $ do
            ast <- mustParse
                [ "void my_other_func(int *_Nonnull p);"
                , "void my_func(int *_Nullable p) {"
                , "   int i = 0;"
                , "   if (p == nullptr) { p = &i; }"
                , "   my_other_func((int *_Nonnull)p);"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe` []

        it "does not warn when a provably non-null assignment of a non-null pointer occurs in the null branch" $ do
            ast <- mustParse
                [ "void my_other_func(int *_Nonnull p);"
                , "void my_func(int *_Nullable p, int *_Nonnull q) {"
                , "   if (p == nullptr) { p = q; }"
                , "   my_other_func((int *_Nonnull)p);"
                , "}"
                ]
            analyseLocal ["nullability"] ("test.c", ast)
                `shouldBe` []
