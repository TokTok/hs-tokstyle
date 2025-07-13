{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.CastSpec (spec) where

import           Test.Hspec            (Spec, describe, it, shouldBe)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings, analyse)
import           Tokstyle.C.LinterSpec (mustParse)


spec :: Spec
spec = describe "Cast linter" $ do
    describe "disallowed casts" $ do
        it "warns when casting between char* and int*" $ do
            ast <- mustParse
                [ "int *func(char *p) {"
                , "  return (int *)p;"
                , "}"
                ]
            analyse allWarnings ast
                `shouldBe`
                [ Text.unlines
                    [ "test.c:2: (column 17) [ERROR]  >>> Type mismatch"
                    , "  disallowed cast from char * to int *"
                    ]
                ]

        it "warns when casting between int* and char*" $ do
            ast <- mustParse
                [ "char *func(int *p) {"
                , "  return (char *)p;"
                , "}"
                ]
            analyse allWarnings ast
                `shouldBe`
                [ Text.unlines
                    [ "test.c:2: (column 18) [ERROR]  >>> Type mismatch"
                    , "  disallowed cast from int * to char *"
                    ]
                ]

        it "warns when casting between unrelated struct pointers" $ do
            ast <- mustParse
                [ "struct A { int a; };"
                , "struct B { int b; };"
                , "struct A *func(struct B *p) {"
                , "  return (struct A *)p;"
                , "}"
                ]
            analyse allWarnings ast
                `shouldBe`
                [ Text.unlines
                    [ "test.c:4: (column 22) [ERROR]  >>> Type mismatch"
                    , "  disallowed cast from struct B * to struct A *"
                    ]
                ]

        it "warns on cast between incompatible enums (different size)" $ do
            ast <- mustParse
                [ "enum A { A_1, A_2 };"
                , "enum B { B_1, B_2, B_3 };"
                , "enum A func(enum B b) {"
                , "  return (enum A)b;"
                , "}"
                ]
            analyse allWarnings ast
                `shouldBe`
                [ Text.unlines
                    [ "<internal>:: [ERROR]  >>> User Error"
                    , "  enum types `enum A` and `enum B` have different a number of enumerators"
                    ]
                ]

        it "warns on cast between enums with different enumerator values" $ do
            ast <- mustParse
                [ "enum A { A_1 = 1, A_2 = 2 };"
                , "enum B { B_1 = 1, B_2 = 3 };"
                , "enum A func(enum B b) {"
                , "  return (enum A)b;"
                , "}"
                ]
            analyse allWarnings ast
                `shouldBe`
                [ Text.unlines
                    [ "test.c:1: (column 24) [ERROR]  >>> Type mismatch"
                    , "  invalid cast: enumerator value for `A_2 = 2` does not match `B_2 = 3`"
                    ]
                ]

    describe "allowed casts" $ do
        it "allows casting to void" $ do
            ast <- mustParse
                [ "void func(int i) {"
                , "  (void)i;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting from void* to other pointers" $ do
            ast <- mustParse
                [ "int *func(void *vp) {"
                , "  int *p = (int *)vp;"
                , "  return p;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting from other pointers to void*" $ do
            ast <- mustParse
                [ "void *func(int *p) {"
                , "  return (void *)p;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting from char* to uint8_t*" $ do
            ast <- mustParse
                [ "typedef unsigned char uint8_t;"
                , "uint8_t *func(char *p) {"
                , "  return (uint8_t *)p;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting from uint8_t* to char*" $ do
            ast <- mustParse
                [ "typedef unsigned char uint8_t;"
                , "char *func(uint8_t *p) {"
                , "  return (char *)p;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting from 0 to a pointer" $ do
            ast <- mustParse
                [ "int *func() {"
                , "  return (int *)0;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting from sockaddr_storage* to sockaddr*" $ do
            ast <- mustParse
                [ "struct sockaddr {};"
                , "struct sockaddr_storage {};"
                , "struct sockaddr *func(struct sockaddr_storage *p) {"
                , "  return (struct sockaddr *)p;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting between numeric types" $ do
            ast <- mustParse
                [ "float func(int i) {"
                , "  return (float)i;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting from enum to int" $ do
            ast <- mustParse
                [ "enum A { A_1 };"
                , "int func(enum A a) {"
                , "  return (int)a;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting from int to enum" $ do
            ast <- mustParse
                [ "enum A { A_1 };"
                , "enum A func(int i) {"
                , "  return (enum A)i;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting between compatible enums" $ do
            ast <- mustParse
                [ "enum A { A_1 = 1, A_2 = 2 };"
                , "enum B { B_1 = 1, B_2 = 2 };"
                , "enum A func(enum B b) {"
                , "  return (enum A)b;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting from a pointer type to itself" $ do
            ast <- mustParse
                [ "struct Foo {};"
                , "struct Foo *func(struct Foo *p) {"
                , "  return (struct Foo *)p;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting from a pointer type to its const pointer version" $ do
            ast <- mustParse
                [ "struct Foo {};"
                , "const struct Foo *func(struct Foo *p) {"
                , "  return (const struct Foo *)p;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting from a typedef to its underlying type" $ do
            ast <- mustParse
                [ "typedef struct Foo {} Foo;"
                , "Foo *func(struct Foo *p) {"
                , "  return (Foo *)p;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting from a struct pointer to a compatible typedef pointer" $ do
            ast <- mustParse
                [ "typedef struct Foo {} Foo;"
                , "struct Foo *func(Foo *p) {"
                , "  return (struct Foo *)p;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows casting from a T array to a pointer to const T" $ do
            ast <- mustParse
                [ "char array[10];"
                , "const char *func() {"
                , "  return (const char *)array;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []
