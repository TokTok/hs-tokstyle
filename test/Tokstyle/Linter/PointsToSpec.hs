{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.PointsToSpec where

import           Test.Hspec          (Spec, it, pendingWith, shouldBe,
                                      shouldMatchList)

import           Tokstyle.Linter     (analyseGlobal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should report the points-to set for a simple function pointer" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void foo(void) { return; }"
            , "void bar(void) { my_cb *my_ptr = &foo; }"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:3: `my_ptr` points to: {foo} [-Wpoints-to]"
            ]

    it "should report the points-to set for a function pointer reassigned in an if statement" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void foo(void) { return; }"
            , "void bar(void) { return; }"
            , "void baz(int c) { my_cb *my_ptr; if (c) { my_ptr = &foo; } else { my_ptr = &bar; } }"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:4: `my_ptr` points to: {bar, foo} [-Wpoints-to]"
            ]

    it "should report the points-to set for a function pointer in a struct" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void foo(void) { return; }"
            , "struct Functions { my_cb *my_ptr; };"
            , "void bar(void) {"
            , "  struct Functions s;"
            , "  s.my_ptr = &foo;"
            , "}"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:3: `s.my_ptr` points to: {foo} [-Wpoints-to]"
            ]

    it "should report the points-to set for a function pointer passed as an argument" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void foo(void) { return; }"
            , "void execute(my_cb *cb) { cb(); }"
            , "void bar(void) { execute(&foo); }"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldBe`
            []

    it "should report the points-to set for a function pointer returned from a function" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void foo(void) { return; }"
            , "my_cb *get_foo(void) { return &foo; }"
            , "void bar(void) { my_cb *my_ptr = get_foo(); }"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:4: `my_ptr` points to: {foo} [-Wpoints-to]"
            ]

    it "should report the points-to set for a function pointer in a complex struct" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void foo(void) { return; }"
            , "struct Inner { my_cb *inner_ptr; };"
            , "struct Outer { struct Inner inner; };"
            , "void bar(void) {"
            , "  struct Outer o;"
            , "  o.inner.inner_ptr = &foo;"
            , "}"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:3: `o.inner.inner_ptr` points to: {foo} [-Wpoints-to]"
            ]

    it "should report the points-to set for an indirect call" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void foo(void) { return; }"
            , "void bar(void) { my_cb *my_ptr = &foo; my_ptr(); }"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:3: `my_ptr` points to: {foo} [-Wpoints-to]"
            ]

    it "should distinguish between different instances of a struct" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void foo(void) { return; }"
            , "void bar(void) { return; }"
            , "struct Functions { my_cb *my_ptr; };"
            , "void baz(void) {"
            , "  struct Functions s1;"
            , "  struct Functions s2;"
            , "  s1.my_ptr = &foo;"
            , "  s2.my_ptr = &bar;"
            , "}"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldMatchList`
            [ "test.c:4: `s1.my_ptr` points to: {foo} [-Wpoints-to]"
            , "test.c:4: `s2.my_ptr` points to: {bar} [-Wpoints-to]"
            ]

    it "should handle deeper call chains for return values" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void foo(void) { return; }"
            , "my_cb* h(void) { return &foo; }"
            , "my_cb* g(void) { return h(); }"
            , "void f(void) { my_cb* ptr = g(); }"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:5: `ptr` points to: {foo} [-Wpoints-to]"
            ]

    it "should handle multiple assignments to the same pointer" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void foo(void) { return; }"
            , "void bar(void) { return; }"
            , "void baz(int c) {"
            , "  my_cb *my_ptr = &foo;"
            , "  if (c) { my_ptr = &bar; }"
            , "}"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:5: `my_ptr` points to: {bar, foo} [-Wpoints-to]"
            ]

    it "should handle function pointers within a struct passed by pointer" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void foo(void) { return; }"
            , "struct Functions { my_cb *my_ptr; };"
            , "void set_ptr(struct Functions *p) {"
            , "  p->my_ptr = &foo;"
            , "}"
            , "void bar(void) {"
            , "  struct Functions s;"
            , "  set_ptr(&s);"
            , "}"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:3: `s.my_ptr` points to: {foo} [-Wpoints-to]"
            ]

    it "should handle a function table struct" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void f1(void) { return; }"
            , "void f2(void) { return; }"
            , "struct Function_Table { my_cb *op1; my_cb *op2; };"
            , "void setup(struct Function_Table *ft) {"
            , "  ft->op1 = &f1;"
            , "  ft->op2 = &f2;"
            , "}"
            , "void main(void) {"
            , "  struct Function_Table table;"
            , "  setup(&table);"
            , "}"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldMatchList`
            [ "test.c:4: `table.op1` points to: {f1} [-Wpoints-to]"
            , "test.c:4: `table.op2` points to: {f2} [-Wpoints-to]"
            ]

    it "should handle passing function pointers through multiple calls" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void foo(void) { return; }"
            , "void h(my_cb *cb) { cb(); }"
            , "void g(my_cb *fun) { h(fun); }"
            , "void f(void) { g(&foo); }"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldBe`
            []

    it "should be context-sensitive enough to distinguish call chains of depth 2" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void foo(void) { return; }"
            , "void bar(void) { return; }"
            , "my_cb* identity(my_cb* ptr) { return ptr; }"
            , "my_cb* call_identity(my_cb* ptr) { return identity(ptr); }"
            , "void main(void) {"
            , "  my_cb* p_foo = call_identity(&foo);"
            , "  my_cb* p_bar = call_identity(&bar);"
            , "}"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldMatchList`
            [ "test.c:7: `p_foo` points to: {foo} [-Wpoints-to]"
            , "test.c:8: `p_bar` points to: {bar} [-Wpoints-to]"
            ]

    it "should be context-sensitive enough for call chains of depth 2 (variant)" $ do
        ast <- mustParse
            [ "typedef void my_cb(void);"
            , "void foo(void) { return; }"
            , "void bar(void) { return; }"
            , "my_cb* identity(my_cb* ptr) { return ptr; }"
            , "my_cb* call_identity(my_cb* ptr) { return identity(ptr); }"
            , "my_cb* g1() { return call_identity(&foo); }"
            , "my_cb* g2() { return call_identity(&bar); }"
            , "void main(void) {"
            , "  my_cb* p_foo = g1();"
            , "  my_cb* p_bar = g2();"
            , "}"
            ]
        pendingWith "2-CFA points-to analysis"
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldMatchList`
            [ "test.c:9: `p_foo` points to: {foo} [-Wpoints-to]"
            , "test.c:10: `p_bar` points to: {bar} [-Wpoints-to]"
            ]

    it "should support macros with assigns" $ do
        ast <- mustParse
            [ "#define ASSIGN_PTR(x, y) do { x = &y; } while (0)"
            , "void bar(void) { int a; int *p; ASSIGN_PTR(p, a); }"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldMatchList`
            [ "test.c:2: `p` points to: {a} [-Wpoints-to]"
            ]

    it "should support scoped macros with assigns" $ do
        ast <- mustParse
            [ "void bar(void) {"
            , "  #define ASSIGN_PTR(x, y) do { x = &y; } while (0)"
            , "  int a;"
            , "  int *p;"
            , "  ASSIGN_PTR(p, a);"
            , "  #undef ASSIGN_PTR"
            , "}"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldMatchList`
            [ "test.c:4: `p` points to: {a} [-Wpoints-to]"
            ]

    it "should correctly handle scoped macro undefinitions" $ do
        ast <- mustParse
            [ "void bar(void) {"
            , "  #define ASSIGN_PTR(x, y) do { x = &y; } while (0)"
            , "  int a;"
            , "  int b;"
            , "  int *p;"
            , "  int *q;"
            , "  ASSIGN_PTR(p, a);"
            , "  #undef ASSIGN_PTR"
            , "  ASSIGN_PTR(q, b);"
            , "}"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldMatchList`
            [ "test.c:5: `p` points to: {a} [-Wpoints-to]"
            ]

    it "should not infinitely loop on struct access inside a for-loop" $ do
        ast <- mustParse
            [ "struct IP4 {"
            , "  unsigned int uint32;"
            , "};"
            , ""
            , "void get_ip4(struct IP4 *result, unsigned int addr) {"
            , "  result->uint32 = addr;"
            , "}"
            , ""
            , "void addr_resolve(struct IP4 *to) {"
            , "  for (int i = 0; i < 10; ++i) {"
            , "    get_ip4(to, 0);"
            , "  }"
            , "}"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldBe` []

    it "should not infinitely loop on union access inside a for-loop" $ do
        ast <- mustParse
            [ "union IP4 {"
            , "  unsigned int uint32;"
            , "};"
            , ""
            , "void get_ip4(union IP4 *result, unsigned int addr) {"
            , "  result->uint32 = addr;"
            , "}"
            , ""
            , "void addr_resolve(union IP4 *to) {"
            , "  for (int i = 0; i < 10; ++i) {"
            , "    get_ip4(to, 0);"
            , "  }"
            , "}"
            ]
        analyseGlobal ["points-to"] [("test.c", ast)]
            `shouldBe` []
