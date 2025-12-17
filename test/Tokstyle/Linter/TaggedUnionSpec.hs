{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.TaggedUnionSpec where

import           Test.Hspec          (Spec, describe, it, shouldBe)

import           Tokstyle.Linter     (analyseGlobal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    describe "Tagged union linter" $ do
        it "detects untagged unions with pointers" $ do
            ast <- mustParse
                [ "typedef union Untagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Untagged;"
                , "struct My_Struct {"
                , "    Untagged u;"
                , "};"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:1: union `Untagged` must be tagged in a struct [-Wtagged-union]"
                ]

        it "accepts tagged unions" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe` []

        it "detects unguarded access to tagged union member" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    char *p = st->u.ptr;"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:11: access to union member `ptr` is not guarded by a check on `tag` [-Wtagged-union]"
                ]

        it "accepts guarded access in if statement" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->tag == TAG_PTR) {"
                , "        char *p = st->u.ptr;"
                , "    }"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe` []

        it "accepts guarded access in switch statement" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    switch (st->tag) {"
                , "    case TAG_PTR:"
                , "        { char *p = st->u.ptr; }"
                , "    }"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe` []

        it "detects access with the WRONG tag check" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->tag == TAG_INT) {"
                , "        char *p = st->u.ptr;"
                , "    }"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:12: access to union member `ptr` is not guarded by a check on `TAG_PTR` [-Wtagged-union]"
                ]

        it "detects unguarded access even if another member is guarded (updated expectation)" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->tag == TAG_INT) {"
                , "        char *p = st->u.ptr;"
                , "    }"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:12: access to union member `ptr` is not guarded by a check on `TAG_PTR` [-Wtagged-union]"
                ]

        it "accepts assignment-based tagging for union creation" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    st->tag = TAG_PTR;"
                , "    st->u.ptr = nullptr;"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe` []

        it "detects incorrect assignment-based tagging" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    st->tag = TAG_INT;"
                , "    st->u.ptr = nullptr;"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:12: access to union member `ptr` is not guarded by a check on `TAG_PTR` [-Wtagged-union]"
                ]

        it "detects access with wrong tag after assignment" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    st->tag = TAG_INT;"
                , "    char *p = st->u.ptr;"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:12: access to union member `ptr` is not guarded by a check on `TAG_PTR` [-Wtagged-union]"
                ]

        it "detects access before tag assignment" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    st->u.ptr = nullptr;"
                , "    st->tag = TAG_PTR;"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:11: access to union member `ptr` is not guarded by a check on `tag` [-Wtagged-union]"
                ]

        it "detects enum members matching union members out of order" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_B, TAG_A } Tag;"
                , "typedef union My_Union {"
                , "    int a;"
                , "    char *b;"
                , "} My_Union;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    My_Union u;"
                , "};"
                ]
            -- Enum is B, A. Union is A, B. Order mismatch.
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: order of members in union `My_Union` should be changed to `b, a` to match enum `Tag` [-Wtagged-union]"
                ]

        it "detects unguarded access to truth member" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_A, TAG_B } Tag;"
                , "typedef union Tagged {"
                , "    char *a;"
                , "    int b;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->u.a) { /* empty */ }"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:11: access to union member `a` is not guarded by a check on `tag` [-Wtagged-union]"
                ]

        it "detects unguarded access to union member" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_A, TAG_B } Tag;"
                , "typedef union Tagged {"
                , "    char *a;"
                , "    int b;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    char *p = st->u.a;"
                , "    st->u.a = (char*)1;"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:11: access to union member `a` is not guarded by a check on `tag` [-Wtagged-union]"
                , "test.c:12: access to union member `a` is not guarded by a check on `tag` [-Wtagged-union]"
                ]

        it "detects out-of-order enum and union members" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_INT, TAG_PTR, TAG_UNUSED } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: order of members in union `Tagged` should be changed to `i, ptr` to match enum `Tag` [-Wtagged-union]"
                ]

        it "supports switch on struct member tag" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st, Tag t) {"
                , "    st->tag = t;"
                , "    switch (st->tag) {"
                , "    case TAG_PTR: {"
                , "        st->u.ptr = nullptr;"
                , "        break;"
                , "    }"
                , "    }"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe` []

        it "ignores unknown structs even if union member name matches" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct Known {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct Unknown *st) {"
                , "    void *p = st->u.ptr;"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe` []

        it "ignores known untagged structs even if union member name matches" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct Known {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "struct Untagged_Struct {"
                , "    Tagged u;"
                , "};"
                , "void test(struct Untagged_Struct *st) {"
                , "    void *p = st->u.ptr;"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe` []

        it "handles logical AND in condition" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->tag == TAG_PTR && st->u.ptr != nullptr) {"
                , "        void *p = st->u.ptr;"
                , "    }"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe` []

        it "handles logical OR in condition" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR1, TAG_PTR2, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->tag == TAG_PTR1 || st->tag == TAG_PTR2) {"
                , "        void *p = st->u.ptr;"
                , "    }"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe` []

        it "handles early return" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->tag != TAG_PTR) return;"
                , "    void *p = st->u.ptr;"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe` []

        it "detects access when OR condition is partially unrelated" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st, int other) {"
                , "    if (st->tag == TAG_PTR || other) {"
                , "        void *p = st->u.ptr;"
                , "    }"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:12: access to union member `ptr` is not guarded by a check on `tag` [-Wtagged-union]"
                ]

        it "supports guards in while loop condition" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    while (st->tag == TAG_PTR) {"
                , "        void *p = st->u.ptr;"
                , "        st->tag = TAG_INT;"
                , "    }"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe` []

        it "supports guards in for loop condition" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    for (int i = 0; st->tag == TAG_PTR; ++i) {"
                , "        void *p = st->u.ptr;"
                , "        st->tag = TAG_INT;"
                , "    }"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe` []

        it "detects access after re-assignment to unknown value" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st, Tag t) {"
                , "    st->tag = TAG_PTR;"
                , "    st->tag = t;"
                , "    void *p = st->u.ptr;"
                , "}"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:13: access to union member `ptr` is not guarded by a check on `tag` [-Wtagged-union]"
                ]

        it "exempts IP_Union" $ do
            ast <- mustParse
                [ "typedef union IP4 { uint32_t u32; } IP4;"
                , "typedef union IP6 { uint8_t u8[16]; } IP6;"
                , "typedef union IP_Union {"
                , "    IP4 v4;"
                , "    IP6 v6;"
                , "} IP_Union;"
                , "struct IP {"
                , "    int family;"
                , "    IP_Union ip;"
                , "};"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe` []

        it "exempts compatible types (same size, no pointers)" $ do
            ast <- mustParse
                [ "typedef union Compatible_Union {"
                , "    uint32_t u32;"
                , "    uint16_t u16[2];"
                , "    uint8_t u8[4];"
                , "} Compatible_Union;"
                , "struct My_Struct {"
                , "    Compatible_Union c;"
                , "};"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe` []

        it "detects incompatible types (different sizes, no pointers)" $ do
            ast <- mustParse
                [ "typedef union Incompatible_Union {"
                , "    uint32_t u32;"
                , "    uint64_t u64;"
                , "} Incompatible_Union;"
                , "struct My_Struct {"
                , "    Incompatible_Union i;"
                , "};"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:1: union `Incompatible_Union` must be tagged in a struct [-Wtagged-union]"
                ]

        it "detects same-size pointers as incompatible (requiring tagging)" $ do
            ast <- mustParse
                [ "typedef union Pointer_Union {"
                , "    void *ptr1;"
                , "    char *ptr2;"
                , "} Pointer_Union;"
                , "struct My_Struct {"
                , "    Pointer_Union u;"
                , "};"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:1: union `Pointer_Union` must be tagged in a struct [-Wtagged-union]"
                ]

        it "detects incompatible sizes (uint32_t, uint8_t[3])" $ do
            ast <- mustParse
                [ "typedef union Incompatible_Size {"
                , "    uint32_t u32;"
                , "    uint8_t u8[3];"
                , "} Incompatible_Size;"
                , "struct My_Struct {"
                , "    Incompatible_Size i;"
                , "};"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:1: union `Incompatible_Size` must be tagged in a struct [-Wtagged-union]"
                ]

        it "detects same-size struct as incompatible (requiring tagging)" $ do
            ast <- mustParse
                [ "typedef struct Small_Struct { uint32_t x; } Small_Struct;"
                , "typedef union Same_Size_Union {"
                , "    Small_Struct s;"
                , "    uint32_t i;"
                , "} Same_Size_Union;"
                , "struct My_Struct {"
                , "    Same_Size_Union u;"
                , "};"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:2: union `Same_Size_Union` must be tagged in a struct [-Wtagged-union]"
                ]

        it "detects void pointers in tagged unions" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_A, TAG_B } Tag;"
                , "typedef union Tagged {"
                , "    void *a;"
                , "    int b;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: union `Tagged` contains a void pointer: `a` [-Wtagged-union]"
                ]

        it "detects void pointers even if used as truth member" $ do
            ast <- mustParse
                [ "typedef enum Tag { TAG_A, TAG_B } Tag;"
                , "typedef union Tagged {"
                , "    void *a;"
                , "    int b;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: union `Tagged` contains a void pointer: `a` [-Wtagged-union]"
                ]

        it "detects enum as incompatible (requiring tagging)" $ do
            ast <- mustParse
                [ "typedef enum My_Enum { VAL_A, VAL_B } My_Enum;"
                , "typedef union Enum_Union {"
                , "    My_Enum e;"
                , "    uint32_t i;"
                , "} Enum_Union;"
                , "struct My_Struct {"
                , "    Enum_Union u;"
                , "};"
                ]
            analyseGlobal ["tagged-union"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:2: union `Enum_Union` must be tagged in a struct [-Wtagged-union]"
                ]

-- end of tests
