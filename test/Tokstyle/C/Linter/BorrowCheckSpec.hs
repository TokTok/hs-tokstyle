{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.BorrowCheckSpec (spec) where
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Test.Hspec            (Spec, describe, it)

import           Tokstyle.C.LinterSpec (check, shouldAccept, shouldProduce,
                                        shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["borrow-check"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["borrow-check"]


spec :: Spec
spec = describe "BorrowCheck linter" $ do
    it "allows a simple function taking an int by value" $ do
        shouldAccept' ["void foo(int value) { }"]

    it "allows nested function calls with int by value" $ do
        shouldAccept'
            [ "void bar(int value) { }"
            , "void foo(int value) { bar(value); }"
            ]

    describe "false positive prevention (leaked temporaries)" $ do
        it "does not warn on PathParam inconsistency from different branches" $ do
            shouldAccept'
                [ "void use(char *p);"
                , "void test(int c, char *p, char *q) {"
                , "    if (c) use(p);"
                , "    else use(q);"
                , "}"
                ]

        it "does not warn on multiple calls in the same expression" $ do
            shouldAccept'
                [ "int use(char *p);"
                , "void test(char *p, char *q) {"
                , "    if (use(p) && use(q));"
                , "}"
                ]

        it "does not warn on overwriting non-owned value types" $ do
            shouldAccept'
                [ "void test() {"
                , "    int x = 1;"
                , "    x = 2;"
                , "}"
                ]

        it "does not warn on inconsistent borrows (not a leak)" $ do
            shouldAccept'
                [ "void test(int c, char *p, char *q) {"
                , "    char *s;"
                , "    if (c) s = p;"
                , "    else s = q;"
                , "}"
                ]

        it "does not warn on overwriting a bool (not a leak)" $ do
            shouldAccept'
                [ "extern const bool true;"
                , "extern const bool false;"
                , "bool test(int c) {"
                , "    bool ok = true;"
                , "    if (c) ok = false;"
                , "    return ok;"
                , "}"
                ]

    it "warns on owned parameters that are not used (leak)" $ do
        shouldWarn' ["int f(char *__attribute__ ((__owned__)) p) { return 3; }"]
            [[ "error: memory leak: p [-Wborrow-check]"
             , "   --> test.c:1:40"
             , "    |"
             , "   1| int f(char *__attribute__ ((__owned__)) p) { return 3; }"
             , "    |                                        ^"
             ]]

    it "allows nested owned pointers (pointer to owned is not owned)" $ do
        shouldAccept' ["int f(char *__attribute__ ((__owned__)) *p) { return 3; }"]

    it "warns on double owned pointers (leak of top-level owned)" $ do
        shouldWarn' ["int f(char *__attribute__ ((__owned__)) *__attribute__ ((__owned__)) p) { return 3; }"]
            [[ "error: memory leak: p [-Wborrow-check]"
             , "   --> test.c:1:69"
             , "    |"
             , "   1| int f(char *__attribute__ ((__owned__)) *__attribute__ ((__owned__)) p) { return 3; }"
             , "    |                                                                     ^"
             ]]


    it "warns on use after move" $ do
        shouldWarn'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void use(char *p);"
            , "void test(char *__attribute__ ((__owned__)) p) {"
            , "    consume(p);"
            , "    use(p);"
            , "}"
            ]
            [[ "error: use after move: p [-Wborrow-check]"
             , "   --> test.c:5:9"
             , "    |"
             , "   4|     consume(p);"
             , "    |             ^"
             , "    |             |"
             , "    |             variable moved here"
             , "   5|     use(p);"
             , "    |         ^"
             ]]

    it "warns on use after move in one branch" $ do
        shouldWarn'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void use(char *p);"
            , "void test(int c, char *__attribute__ ((__owned__)) p) {"
            , "    if (c) consume(p);"
            , "    use(p);"
            , "}"
            ]
            [[ "error: use of variable that may be moved: p [-Wborrow-check]"
             , "   --> test.c:5:9"
             , "    |"
             , "   4|     if (c) consume(p);"
             , "    |                    ^"
             , "    |                    |"
             , "    |                    variable may be moved here"
             , "   5|     use(p);"
             , "    |         ^"
             ]
            ,[ "error: memory leak: p may be leaked [-Wborrow-check]"
             , "   --> test.c:3:52"
             , "    |"
             , "   3| void test(int c, char *__attribute__ ((__owned__)) p) {"
             , "    |                                                    ^"
             , "    |                                                    |"
             , "    |                                                    variable defined here"
             ]]

    it "warns on move inside a loop" $ do
        shouldWarn'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void test(int c, char *__attribute__ ((__owned__)) p) {"
            , "    while (c) {"
            , "        consume(p);"
            , "    }"
            , "}"
            ]
            [[ "error: move of variable that may be moved: p [-Wborrow-check]"
             , "   --> test.c:4:17"
             , "    |"
             , "   4|         consume(p);"
             , "    |                 ^"
             , "    |                 |"
             , "    |                 variable may be moved here"
             ]
            ,[ "error: memory leak: p may be leaked [-Wborrow-check]"
             , "   --> test.c:2:52"
             , "    |"
             , "   2| void test(int c, char *__attribute__ ((__owned__)) p) {"
             , "    |                                                    ^"
             , "    |                                                    |"
             , "    |                                                    variable defined here"
             ]]

    it "warns on move via assignment" $ do
        shouldWarn'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void use(char *p);"
            , "void test(char *__attribute__ ((__owned__)) p) {"
            , "    char *__attribute__ ((__owned__)) q = p;"
            , "    use(p);"
            , "    use(q);"
            , "    consume(q);"
            , "}"
            ]
            [[ "error: use after move: p [-Wborrow-check]"
             , "   --> test.c:5:9"
             , "    |"
             , "   4|     char *__attribute__ ((__owned__)) q = p;"
             , "    |                                           ^"
             , "    |                                           |"
             , "    |                                           variable moved here"
             , "   5|     use(p);"
             , "    |         ^"
             ]]

    it "warns on move via assignment statement" $ do
        shouldWarn'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void use(char *p);"
            , "void test(char *__attribute__ ((__owned__)) p) {"
            , "    char *__attribute__ ((__owned__)) q;"
            , "    q = p;"
            , "    use(p);"
            , "    use(q);"
            , "    consume(q);"
            , "}"
            ]
            [[ "error: use after move: p [-Wborrow-check]"
             , "   --> test.c:6:9"
             , "    |"
             , "   5|     q = p;"
             , "    |         ^"
             , "    |         |"
             , "    |         variable moved here"
             , "   6|     use(p);"
             , "    |         ^"
             ]]

    it "warns on double move" $ do
        shouldWarn'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void test(char *__attribute__ ((__owned__)) p) {"
            , "    consume(p);"
            , "    consume(p);"
            , "}"
            ]
            [[ "error: move of already moved variable: p [-Wborrow-check]"
             , "   --> test.c:4:13"
             , "    |"
             , "   3|     consume(p);"
             , "    |             ^"
             , "    |             |"
             , "    |             variable first moved here"
             , "   4|     consume(p);"
             , "    |             ^"
             ]]

    it "warns on overwrite leak" $ do
        shouldWarn'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void test(char *__attribute__ ((__owned__)) p, char *__attribute__ ((__owned__)) q) {"
            , "    p = q;"
            , "    consume(p);"
            , "}"
            ]
            [[ "error: memory leak: p is overwritten [-Wborrow-check]"
             , "   --> test.c:3:5"
             , "    |"
             , "   2| void test(char *__attribute__ ((__owned__)) p, char *__attribute__ ((__owned__)) q) {"
             , "    |                                             ^"
             , "    |                                             |"
             , "    |                                             variable defined here"
             , "   3|     p = q;"
             , "    |     ^^^^^"
             ]]

    it "warns on conditional leak" $ do
        shouldWarn'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void test(int c, char *__attribute__ ((__owned__)) p) {"
            , "    if (c) consume(p);"
            , "}"
            ]
            [[ "error: memory leak: p may be leaked [-Wborrow-check]"
             , "   --> test.c:2:52"
             , "    |"
             , "   2| void test(int c, char *__attribute__ ((__owned__)) p) {"
             , "    |                                                    ^"
             , "    |                                                    |"
             , "    |                                                    variable defined here"
             ]]

    it "allows return as consumption" $ do
        shouldAccept'
            [ "char *__attribute__ ((__owned__)) test(char *__attribute__ ((__owned__)) p) {"
            , "    return p;"
            , "}"
            ]

    it "does not warn on uninitialized owned variable leak" $ do
        shouldAccept'
            [ "void test() {"
            , "    char *__attribute__ ((__owned__)) p;"
            , "}"
            ]

    it "does not warn on first assignment to uninitialized owned variable" $ do
        shouldAccept'
            [ "char *__attribute__ ((__owned__)) produce();"
            , "void consume(char *__attribute__ ((__owned__)) p);"
            , "void test() {"
            , "    char *__attribute__ ((__owned__)) p;"
            , "    p = produce();"
            , "    consume(p);"
            , "}"
            ]

    it "warns on ignored owned return value" $ do
        shouldWarn'
            [ "char *__attribute__ ((__owned__)) produce();"
            , "void test() {"
            , "    produce();"
            , "}"
            ]
            [[ "error: memory leak: <return value> [-Wborrow-check]"
             , "   --> test.c:3:5"
             , "    |"
             , "   3|     produce();"
             , "    |     ^^^^^^^^^"
             ]]

    it "provides better diagnostics for conditional moves" $ do
        shouldWarn'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void use(char *p);"
            , "void test(int c, char *__attribute__ ((__owned__)) p) {"
            , "    if (c) consume(p);"
            , "    use(p);"
            , "}"
            ]
            [[ "error: use of variable that may be moved: p [-Wborrow-check]"
             , "   --> test.c:5:9"
             , "    |"
             , "   4|     if (c) consume(p);"
             , "    |                    ^"
             , "    |                    |"
             , "    |                    variable may be moved here"
             , "   5|     use(p);"
             , "    |         ^"
             ]
            ,[ "error: memory leak: p may be leaked [-Wborrow-check]"
             , "   --> test.c:3:52"
             , "    |"
             , "   3| void test(int c, char *__attribute__ ((__owned__)) p) {"
             , "    |                                                    ^"
             , "    |                                                    |"
             , "    |                                                    variable defined here"
             ]]

    it "does not move on assignment to non-owned" $ do
        shouldAccept'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void use(const char *p);"
            , "void test(char *__attribute__ ((__owned__)) p) {"
            , "    const char *q = p;"
            , "    use(p);"
            , "    use(q);"
            , "    consume(p);"
            , "}"
            ]

    it "does not move on pointer arithmetic" $ do
        shouldAccept'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void use(char *p);"
            , "void test(char *__attribute__ ((__owned__)) p) {"
            , "    use(p + 1);"
            , "    use(p);"
            , "    consume(p);"
            , "}"
            ]

    it "warns on double move of dereferenced owned pointer" $ do
        shouldWarn'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void test(char *__attribute__ ((__owned__)) *p) {"
            , "    consume(*p);"
            , "    consume(*p);"
            , "}"
            ]
            [[ "error: move of already moved variable: *p [-Wborrow-check]"
             , "   --> test.c:4:13"
             , "    |"
             , "   3|     consume(*p);"
             , "    |             ^^"
             , "    |             |"
             , "    |             variable first moved here"
             , "   4|     consume(*p);"
             , "    |             ^^"
             ]]

    it "warns on move while borrowed" $ do
        shouldWarn'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void use(char *p);"
            , "void test(char *__attribute__ ((__owned__)) p) {"
            , "    char *q = p;"
            , "    consume(p);"
            , "    use(q);"
            , "}"
            ]
            [[ "error: move of p while borrowed by q [-Wborrow-check]"
             , "   --> test.c:5:13"
             , "    |"
             , "   4|     char *q = p;"
             , "    |               ^"
             , "    |               |"
             , "    |               borrowed here"
             , "   5|     consume(p);"
             , "    |             ^"
             ]]

    it "allows move after borrow is no longer live (NLL)" $ do
        shouldAccept'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void use(char *p);"
            , "void test(char *__attribute__ ((__owned__)) p) {"
            , "    char *q = p;"
            , "    use(q);"
            , "    consume(p);"
            , "}"
            ]

    it "warns on move while transitively borrowed" $ do
        shouldWarn'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void use(char *p);"
            , "void test(char *__attribute__ ((__owned__)) p) {"
            , "    char *q = p;"
            , "    char *r = q;"
            , "    consume(p);"
            , "    use(r);"
            , "}"
            ]
            [[ "error: move of p while borrowed by r [-Wborrow-check]"
             , "   --> test.c:6:13"
             , "    |"
             , "   5|     char *r = q;"
             , "    |               ^"
             , "    |               |"
             , "    |               borrowed here"
             , "   6|     consume(p);"
             , "    |             ^"
             ]]

    it "warns on move while field is borrowed" $ do
        shouldWarn'
            [ "struct S { char *f; };"
            , "void consume(struct S *__attribute__ ((__owned__)) p);"
            , "void use(char *p);"
            , "void test(struct S *__attribute__ ((__owned__)) p) {"
            , "    char *q = p->f;"
            , "    consume(p);"
            , "    use(q);"
            , "}"
            ]
            [[ "error: move of p while borrowed by q [-Wborrow-check]"
             , "   --> test.c:6:13"
             , "    |"
             , "   5|     char *q = p->f;"
             , "    |               ^^^^"
             , "    |               |"
             , "    |               borrowed here"
             , "   6|     consume(p);"
             , "    |             ^"
             ]]

    it "warns on move while return value is borrowed (auto-summary)" $ do
        shouldWarn'
            [ "struct S { char *f; };"
            , "char *get_f(struct S *s) { return s->f; }"
            , "void consume(struct S *__attribute__ ((__owned__)) s);"
            , "void use(char *p);"
            , "void test(struct S *__attribute__ ((__owned__)) s) {"
            , "    char *q = get_f(s);"
            , "    consume(s);"
            , "    use(q);"
            , "}"
            ]
            [[ "error: move of s while borrowed by q [-Wborrow-check]"
             , "   --> test.c:7:13"
             , "    |"
             , "   6|     char *q = get_f(s);"
             , "    |               ^^^^^^^^"
             , "    |               |"
             , "    |               borrowed here"
             , "   7|     consume(s);"
             , "    |             ^"
             ]]

    it "allows multiple shared borrows" $ do
        shouldAccept'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void use(const char *p);"
            , "void test(char *__attribute__ ((__owned__)) p) {"
            , "    const char *q = p;"
            , "    const char *r = p;"
            , "    use(q);"
            , "    use(r);"
            , "    consume(p);"
            , "}"
            ]

    it "warns on mutable borrow while shared borrowed" $ do
        shouldWarn'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void use(const char *p);"
            , "void mutate(char *p);"
            , "void test(char *__attribute__ ((__owned__)) p) {"
            , "    const char *q = p;"
            , "    char *r = p;"
            , "    use(q);"
            , "    mutate(r);"
            , "    consume(p);"
            , "}"
            ]
            [[ "error: mutable borrow of p while borrowed by q [-Wborrow-check]"
             , "   --> test.c:6:15"
             , "    |"
             , "   5|     const char *q = p;"
             , "    |                     ^"
             , "    |                     |"
             , "    |                     borrowed here"
             , "   6|     char *r = p;"
             , "    |               ^"
             ]]

    it "warns on shared borrow while mutable borrowed" $ do
        shouldWarn'
            [ "void consume(char *__attribute__ ((__owned__)) p);"
            , "void use(const char *p);"
            , "void mutate(char *p);"
            , "void test(char *__attribute__ ((__owned__)) p) {"
            , "    char *q = p;"
            , "    const char *r = p;"
            , "    mutate(q);"
            , "    use(r);"
            , "    consume(p);"
            , "}"
            ]
            [[ "error: shared borrow of p while borrowed by q [-Wborrow-check]"
             , "   --> test.c:6:21"
             , "    |"
             , "   5|     char *q = p;"
             , "    |               ^"
             , "    |               |"
             , "    |               borrowed here"
             , "   6|     const char *r = p;"
             , "    |                     ^"
             ]]

    describe "data = f(data) scenarios" $ do
        it "allows p = f(p) when f consumes and produces ownership" $ do
            shouldAccept'
                [ "char *__attribute__ ((__owned__)) f(char *__attribute__ ((__owned__)) p);"
                , "void consume(char *__attribute__ ((__owned__)) p);"
                , "void test(char *__attribute__ ((__owned__)) p) {"
                , "    p = f(p);"
                , "    consume(p);"
                , "}"
                ]

        it "warns on p = f(p) when f only borrows p" $ do
            shouldWarn'
                [ "char *__attribute__ ((__owned__)) f(char *p);"
                , "void consume(char *__attribute__ ((__owned__)) p);"
                , "void test(char *__attribute__ ((__owned__)) p) {"
                , "    p = f(p);"
                , "    consume(p);"
                , "}"
                ]
                [[ "error: memory leak: p is overwritten [-Wborrow-check]"
                 , "   --> test.c:4:5"
                 , "    |"
                 , "   3| void test(char *__attribute__ ((__owned__)) p) {"
                 , "    |                                             ^"
                 , "    |                                             |"
                 , "    |                                             variable defined here"
                 , "   4|     p = f(p);"
                 , "    |     ^^^^^^^^"
                 ]]

        it "allows p = g(p) when g consumes p but returns non-owned" $ do
            shouldAccept'
                [ "char *g(char *__attribute__ ((__owned__)) p);"
                , "void test(char *__attribute__ ((__owned__)) p) {"
                , "    p = g(p);"
                , "}"
                ]

        it "warns on leak when non-owned variable is assigned an owned value" $ do
            shouldWarn'
                [ "char *__attribute__ ((__owned__)) produce();"
                , "void test() {"
                , "    char *q = produce();"
                , "}"
                ]
                [[ "error: memory leak: q [-Wborrow-check]"
                 , "   --> test.c:3:11"
                 , "    |"
                 , "   3|     char *q = produce();"
                 , "    |           ^"
                 ]]

    describe "pointer walking (arithmetic)" $ do
        it "allows walking a pointer through multiple calls" $ do
            shouldAccept'
                [ "char *f(char *s, char c) { *s = c; return s + 1; }"
                , "void write_chars(char *s, char c1, char c2) {"
                , "    s = f(s, c1);"
                , "    s = f(s, c2);"
                , "}"
                ]

        it "tracks provenance through arithmetic for owned variables" $ do
            shouldAccept'
                [ "char *f(char *s) { return s + 1; }"
                , "void consume(char *__attribute__ ((__owned__)) p);"
                , "void use(char *p);"
                , "void test(char *__attribute__ ((__owned__)) p) {"
                , "    char *s = p;"
                , "    s = f(s);"
                , "    use(s);"
                , "    consume(p);"
                , "}"
                ]

        it "warns on move while walked pointer is live" $ do
            shouldWarn'
                [ "char *f(char *s) { return s + 1; }"
                , "void consume(char *__attribute__ ((__owned__)) p);"
                , "void use(char *p);"
                , "void test(char *__attribute__ ((__owned__)) p) {"
                , "    char *s = f(p);"
                , "    consume(p);"
                , "    use(s);"
                , "}"
                ]
                [[ "error: move of p while borrowed by s [-Wborrow-check]"
                 , "   --> test.c:6:13"
                 , "    |"
                 , "   5|     char *s = f(p);"
                 , "    |               ^^^^"
                 , "    |               |"
                 , "    |               borrowed here"
                 , "   6|     consume(p);"
                 , "    |             ^"
                 ]]

    describe "pointer to stack variables" $ do
        it "allows passing &b to set_char(char *b)" $ do
            shouldAccept'
                [ "void set_char(char *b);"
                , "void test() {"
                , "    char b = 0;"
                , "    set_char(&b);"
                , "}"
                ]

        it "warns on mutable borrow while shared borrowed (stack variable)" $ do
            shouldWarn'
                [ "void use(const char *b);"
                , "void mutate(char *b);"
                , "void test() {"
                , "    char b = 0;"
                , "    const char *p = &b;"
                , "    mutate(&b);"
                , "    use(p);"
                , "}"
                ]
                [[ "error: mutable borrow of b while borrowed by p [-Wborrow-check]"
                 , "   --> test.c:6:12"
                 , "    |"
                 , "   5|     const char *p = &b;"
                 , "    |                     ^^"
                 , "    |                     |"
                 , "    |                     borrowed here"
                 , "   6|     mutate(&b);"
                 , "    |            ^^"
                 ]]

        it "warns on shared borrow while mutable borrowed (stack variable)" $ do
            shouldWarn'
                [ "void use(const char *b);"
                , "void mutate(char *b);"
                , "void test() {"
                , "    char b = 0;"
                , "    char *p = &b;"
                , "    use(&b);"
                , "    mutate(p);"
                , "}"
                ]
                [[ "error: shared borrow of b while borrowed by p [-Wborrow-check]"
                 , "   --> test.c:6:9"
                 , "    |"
                 , "   5|     char *p = &b;"
                 , "    |               ^^"
                 , "    |               |"
                 , "    |               borrowed here"
                 , "   6|     use(&b);"
                 , "    |         ^^"
                 ]]

    describe "nullptr special handling" $ do
        it "does not warn on multiple uses of nullptr" $ do
            shouldAccept'
                [ "typedef void *nullptr_t;"
                , "extern nullptr_t nullptr;"
                , "struct S;"
                , "int f(const char *a, const char *b, struct S **c);"
                , "void test() {"
                , "    struct S *s = nullptr;"
                , "    f(nullptr, nullptr, &s);"
                , "}"
                ]

    it "allows walking a pointer in a for loop" $ do
        shouldAccept'
            [ "struct Node {"
            , "    struct Node *next;"
            , "    int data;"
            , "};"
            , "void test(struct Node *list) {"
            , "    for (struct Node *walker = list; walker != 0; walker = walker->next) {"
            , "        walker->data = 1;"
            , "    }"
            , "}"
            ]

    it "allows walking a pointer in two consecutive for loops" $ do
        shouldAccept'
            [ "struct Node {"
            , "    struct Node *next;"
            , "    int data;"
            , "};"
            , "void test(struct Node *list) {"
            , "    for (struct Node *walker = list; walker != 0; walker = walker->next) {"
            , "        walker->data = 1;"
            , "    }"
            , "    for (struct Node *walker = list; walker != 0; walker = walker->next) {"
            , "        walker->data = 2;"
            , "    }"
            , "}"
            ]

    it "allows shared borrows of const typedef pointers stored in structs" $ do
        shouldAccept'
            [ "typedef struct T T;"
            , "struct S { const T *f; };"
            , "void use(const T *t);"
            , "void test(const T *t, struct S *s) {"
            , "    s->f = t;"
            , "    use(t);"
            , "    use(s->f);"
            , "}"
            ]

    describe "conservative assumption for unknown functions" $ do
        it "assumes return value borrows from pointer arguments" $ do
            shouldWarn'
                [ "struct S;"
                , "char *get_data(struct S *s);"
                , "void mutate(struct S *s);"
                , "void use(char *p);"
                , "void test(struct S *s) {"
                , "    char *p = get_data(s);"
                , "    mutate(s);"
                , "    use(p);"
                , "}"
                ]
                [[ "error: mutable borrow of s while borrowed by p [-Wborrow-check]"
                 , "   --> test.c:7:12"
                 , "    |"
                 , "   6|     char *p = get_data(s);"
                 , "    |               ^^^^^^^^^^^"
                 , "    |               |"
                 , "    |               borrowed here"
                 , "   7|     mutate(s);"
                 , "    |            ^"
                 ]]

        it "does not assume borrow if return value is marked as owned" $ do
            shouldAccept'
                [ "struct S;"
                , "void consume(char * __attribute__((owned)) p);"
                , "char * __attribute__((owned)) produce(struct S *s);"
                , "void mutate(struct S *s);"
                , "void test(struct S *s) {"
                , "    char * __attribute__((owned)) p = produce(s);"
                , "    mutate(s);"
                , "    consume(p);"
                , "}"
                ]

        it "allows p = f(p) even with conservative borrow" $ do
            shouldAccept'
                [ "char *f(char *p);"
                , "void test(char *p) {"
                , "    p = f(p);"
                , "}"
                ]

    it "warns on move while field is borrowed (Onion kill scenario)" $ do
        shouldWarn'
            [ "struct S { const char *f; };"
            , "void consume(struct S *__attribute__ ((__owned__)) s);"
            , "void use(const char *f);"
            , "void test(struct S *__attribute__ ((__owned__)) s) {"
            , "    const char *f = s->f;"
            , "    consume(s);"
            , "    use(f);"
            , "}"
            ]
            [[ "error: move of s while borrowed by f [-Wborrow-check]"
             , "   --> test.c:6:13"
             , "    |"
             , "   5|     const char *f = s->f;"
             , "    |                     ^^^^"
             , "    |                     |"
             , "    |                     borrowed here"
             , "   6|     consume(s);"
             , "    |             ^"
             ]]

    it "allows self-assignment with provenance (p = f(p))" $ do
        shouldAccept'
            [ "char *f(char *p) { return p; }"
            , "void test(char *p) {"
            , "    p = f(p);"
            , "}"
            ]

-- end of file
