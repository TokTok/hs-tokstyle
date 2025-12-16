{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.PointsToAssertsSpec where

import           Test.Hspec          (Spec, describe, it, shouldBe)

import           Tokstyle.Linter     (analyseGlobal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    describe "Points-To Asserts Linter" $ do
        it "does not warn when a pointer is correctly asserted to be heap-allocated" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void test() {"
                , "  int* p = (int*)malloc(sizeof(int));"
                , "  assert(mem_is_heap(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns when a stack variable is asserted to be on the heap" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void test() {"
                , "  int a;"
                , "  int* p = &a;"
                , "  assert(mem_is_heap(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be heap. It can point to {StackLoc}. [-Wpoints-to-asserts]"
                ]

        it "warns when a potentially null pointer is asserted to be on the heap" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void test(int cond) {"
                , "  int* p;"
                , "  if (cond) {"
                , "    p = (int*)malloc(sizeof(int));"
                , "  } else {"
                , "    p = nullptr;"
                , "  }"
                , "  assert(mem_is_heap(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:11: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be heap. It can point to {HeapLoc, NullLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn when a pointer is correctly asserted to be stack-allocated" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_stack(void* ptr);"
                , "void test() {"
                , "  int a;"
                , "  int* p = &a;"
                , "  assert(mem_is_stack(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns when a heap variable is asserted to be on the stack" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_stack(void* ptr);"
                , "void test() {"
                , "  int* p = (int*)malloc(sizeof(int));"
                , "  assert(mem_is_stack(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be stack. It can point to {HeapLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn when a pointer is correctly asserted to be not-null" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_not_null(void* ptr);"
                , "void test() {"
                , "  int a;"
                , "  int* p = &a;"
                , "  assert(mem_is_not_null(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns when a possibly null pointer is asserted to be not-null" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_not_null(void* ptr);"
                , "void test(int cond) {"
                , "  int a;"
                , "  int* p = &a;"
                , "  if (cond) {"
                , "    p = nullptr;"
                , "  }"
                , "  assert(mem_is_not_null(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:9: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be not null. It can point to {NullLoc, StackLoc}. [-Wpoints-to-asserts]"
                ]

        it "warns for incorrect assertion on pointer in a struct" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "struct My_S { int* field; };"
                , "void test() {"
                , "  struct My_S s;"
                , "  int a;"
                , "  s.field = &a;"
                , "  assert(mem_is_heap(s.field));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:8: Static analysis check failed: Pointer 'field' does not satisfy assertion: must be heap. It can point to {StackLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn when assertion is correct after interprocedural analysis" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void allocate(int** p) {"
                , "  *p = (int*)malloc(sizeof(int));"
                , "}"
                , "void test() {"
                , "  int* q = nullptr;"
                , "  allocate(&q);"
                , "  assert(mem_is_heap(q));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns when assertion is incorrect after interprocedural analysis" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void set_to_stack(int** p, int* val) {"
                , "  *p = val;"
                , "}"
                , "void test() {"
                , "  int a;"
                , "  int* q = nullptr;"
                , "  set_to_stack(&q, &a);"
                , "  assert(mem_is_heap(q));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:10: Static analysis check failed: Pointer 'q' does not satisfy assertion: must be heap. It can point to {StackLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn with correct assertion after call via function pointer" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void allocate(int** p) { *p = (int*)malloc(sizeof(int)); }"
                , "typedef void alloc_fn_cb(int** p);"
                , "void test() {"
                , "  alloc_fn_cb* f = allocate;"
                , "  int* q = nullptr;"
                , "  f(&q);"
                , "  assert(mem_is_heap(q));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns with assertion after ambiguous call via function pointer" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void allocate_heap(int** p) { *p = (int*)malloc(sizeof(int)); }"
                , "void set_to_null(int** p) { *p = nullptr; }"
                , "typedef void alloc_fn_cb(int** p);"
                , "void test(int cond) {"
                , "  alloc_fn_cb* f;"
                , "  if (cond) { f = allocate_heap; } else { f = set_to_null; }"
                , "  int* q = nullptr;"
                , "  f(&q);"
                , "  assert(mem_is_heap(q));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:12: Static analysis check failed: Pointer 'q' does not satisfy assertion: must be heap. It can point to {HeapLoc, NullLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn with correct assertion after a deep call chain" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void allocate_deep(int** p) { *p = (int*)malloc(sizeof(int)); }"
                , "void allocate_mid(int** p) { allocate_deep(p); }"
                , "void test() {"
                , "  int* q = nullptr;"
                , "  allocate_mid(&q);"
                , "  assert(mem_is_heap(q));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "handles assertions on fields of nested structs" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "struct Inner { int* p; };"
                , "struct Outer { struct Inner i; };"
                , "void test() {"
                , "  struct Outer o;"
                , "  o.i.p = (int*)malloc(sizeof(int));"
                , "  assert(mem_is_heap(o.i.p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns on incorrect assertions on fields of nested structs" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "struct Inner { int* p; };"
                , "struct Outer { struct Inner i; };"
                , "void test() {"
                , "  struct Outer o;"
                , "  int a;"
                , "  o.i.p = &a;"
                , "  assert(mem_is_heap(o.i.p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:9: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be heap. It can point to {StackLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn with correct assertion after call via function pointer in a struct" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void allocate(int** p) { *p = (int*)malloc(sizeof(int)); }"
                , "typedef void alloc_fn_cb(int** p);"
                , "struct Dispatcher { alloc_fn_cb* action; };"
                , "void test() {"
                , "  struct Dispatcher d;"
                , "  d.action = allocate;"
                , "  int* q = nullptr;"
                , "  d.action(&q);"
                , "  assert(mem_is_heap(q));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns with assertion after ambiguous call via function pointer in a struct" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void allocate_heap(int** p) { *p = (int*)malloc(sizeof(int)); }"
                , "void set_to_null(int** p) { *p = nullptr; }"
                , "typedef void alloc_fn_cb(int** p);"
                , "struct Dispatcher { alloc_fn_cb* action; };"
                , "void test(int cond) {"
                , "  struct Dispatcher d;"
                , "  if (cond) { d.action = allocate_heap; } else { d.action = set_to_null; }"
                , "  int* q = nullptr;"
                , "  d.action(&q);"
                , "  assert(mem_is_heap(q));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:13: Static analysis check failed: Pointer 'q' does not satisfy assertion: must be heap. It can point to {HeapLoc, NullLoc}. [-Wpoints-to-asserts]"
                ]

        it "warns on incorrect assertion about a pointer to a global" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "const int g_a = 0;"
                , "void test() {"
                , "  const int* p = &g_a;"
                , "  assert(mem_is_heap(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be heap. It can point to {GlobalVarLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn when an assertion in a nested callee holds" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void callee(int* p) { assert(mem_is_heap(p)); }"
                , "void caller(int* p) { callee(p); }"
                , "void test() {"
                , "  int* q = (int*)malloc(sizeof(int));"
                , "  caller(q);"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns when an assertion in a nested callee fails" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void callee(int* p) { assert(mem_is_heap(p)); }"
                , "void caller(int* p) { callee(p); }"
                , "void test() {"
                , "  int a;"
                , "  int* q = &a;"
                , "  caller(q);"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be heap. It can point to {StackLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn on correct assertion on aliased union field" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_stack(void* ptr);"
                , "union Ptr_Union { int* a; int* b; };"
                , "void test() {"
                , "  union Ptr_Union u;"
                , "  int x;"
                , "  u.a = &x;"
                , "  assert(mem_is_stack(u.b));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "does not warn on correct assertion after memcpy" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void* memcpy(void* dest, const void* src, unsigned long n);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "struct My_S { int* field; };"
                , "void test() {"
                , "  struct My_S s1;"
                , "  struct My_S s2;"
                , "  s1.field = (int*)malloc(sizeof(int));"
                , "  memcpy(&s2, &s1, sizeof(struct My_S));"
                , "  assert(mem_is_heap(s2.field));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns on incorrect assertion on a global pointer" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "typedef const int* Const_Int_Ptr;"
                , "static const int g_var = 42;"
                , "static const Const_Int_Ptr g_ptr = &g_var;"
                , "void test() {"
                , "  assert(mem_is_heap(g_ptr));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:7: Static analysis check failed: Pointer 'g_ptr' does not satisfy assertion: must be heap. It can point to {GlobalVarLoc}. [-Wpoints-to-asserts]"
                ]

        it "warns when an assertion is only correct in one #ifdef branch" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void test() {"
                , "  int a;"
                , "  int* p;"
                , "#ifdef SOME_FLAG"
                , "  p = (int*)malloc(sizeof(int));"
                , "#else"
                , "  p = &a;"
                , "#endif /* SOME_FLAG */"
                , "  assert(mem_is_heap(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:12: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be heap. It can point to {HeapLoc, StackLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn for a correct || assertion" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "int mem_is_stack(void* ptr);"
                , "void test(int cond) {"
                , "  int a;"
                , "  int* p;"
                , "  if (cond) {"
                , "    p = &a;"
                , "  } else {"
                , "    p = (int*)malloc(sizeof(int));"
                , "  }"
                , "  assert(mem_is_heap(p) || mem_is_stack(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns for an incorrect || assertion" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "int mem_is_stack(void* ptr);"
                , "void test(int cond) {"
                , "  int* p;"
                , "  if (cond) {"
                , "    p = (int*)malloc(sizeof(int));"
                , "  } else {"
                , "    p = nullptr;"
                , "  }"
                , "  assert(mem_is_heap(p) || mem_is_stack(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:12: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be heap or stack. It can point to {HeapLoc, NullLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn for a correct && assertion" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "int mem_is_not_null(void* ptr);"
                , "void test() {"
                , "  int* p = (int*)malloc(sizeof(int));"
                , "  assert(mem_is_heap(p) && mem_is_not_null(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns for an incorrect && assertion" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "int mem_is_stack(void* ptr);"
                , "void test() {"
                , "  int* p = (int*)malloc(sizeof(int));"
                , "  assert(mem_is_heap(p) && mem_is_stack(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:7: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be heap and stack. It can point to {HeapLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn for a correct nested (||) && assertion" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "int mem_is_stack(void* ptr);"
                , "int mem_is_not_null(void* ptr);"
                , "void test(int cond) {"
                , "  int a;"
                , "  int* p;"
                , "  if (cond) {"
                , "    p = &a;"
                , "  } else {"
                , "    p = (int*)malloc(sizeof(int));"
                , "  }"
                , "  assert((mem_is_heap(p) || mem_is_stack(p)) && mem_is_not_null(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns for an incorrect nested (||) && assertion" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "int mem_is_stack(void* ptr);"
                , "int mem_is_not_null(void* ptr);"
                , "void test(int cond) {"
                , "  int* p;"
                , "  if (cond) {"
                , "    p = nullptr;"
                , "  } else {"
                , "    p = (int*)malloc(sizeof(int));"
                , "  }"
                , "  assert((mem_is_heap(p) || mem_is_stack(p)) && mem_is_not_null(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:13: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be (heap or stack) and not null. It can point to {HeapLoc, NullLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn with correct assertion after v-table based allocation" $ do
            ast <- mustParse
                [ "void* calloc(unsigned int nmemb, unsigned int size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "typedef void *mem_calloc_cb(void *obj, unsigned int nmemb, unsigned int size);"
                , "typedef struct Memory_Funcs { mem_calloc_cb *calloc; } Memory_Funcs;"
                , "typedef struct Memory { const Memory_Funcs *funcs; void *obj; } Memory;"
                , "void *sys_calloc(void *obj, unsigned int nmemb, unsigned int size) { return calloc(nmemb, size); }"
                , "static const Memory_Funcs os_memory_funcs = { sys_calloc };"
                , "static const Memory os_memory_obj = { &os_memory_funcs, 0 };"
                , "const Memory *os_memory(void) { return &os_memory_obj; }"
                , "void *mem_alloc(const Memory *mem, unsigned int size) {"
                , "  return mem->funcs->calloc(mem->obj, 1, size);"
                , "}"
                , "void test() {"
                , "  const Memory* mem = os_memory();"
                , "  int* p = (int*)mem_alloc(mem, sizeof(int));"
                , "  assert(mem_is_heap(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "does not warn when asserting on a pointer returned from a struct" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "struct My_S { int* field; };"
                , "int* get_field(struct My_S* s) { return s->field; }"
                , "void test() {"
                , "  struct My_S s;"
                , "  s.field = (int*)malloc(sizeof(int));"
                , "  int* p = get_field(&s);"
                , "  assert(mem_is_heap(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns when asserting on a pointer returned from a struct" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "struct My_S { int* field; };"
                , "int* get_field(struct My_S* s) { return s->field; }"
                , "void test() {"
                , "  struct My_S s;"
                , "  int a;"
                , "  s.field = &a;"
                , "  int* p = get_field(&s);"
                , "  assert(mem_is_heap(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:10: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be heap. It can point to {StackLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn on correct assertion after memcpy of nested struct" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void* memcpy(void* dest, const void* src, unsigned long n);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "struct Inner { int* p; };"
                , "struct Outer { struct Inner i; };"
                , "void test() {"
                , "  struct Outer o1;"
                , "  struct Outer o2;"
                , "  o1.i.p = (int*)malloc(sizeof(int));"
                , "  memcpy(&o2, &o1, sizeof(struct Outer));"
                , "  assert(mem_is_heap(o2.i.p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns when an assertion in a callee fails for only one call site" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void callee(int* p) { assert(mem_is_heap(p)); }"
                , "void test() {"
                , "  int a;"
                , "  int* from_stack = &a;"
                , "  int* from_heap = (int*)malloc(sizeof(int));"
                , "  callee(from_heap);"
                , "  callee(from_stack);"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:4: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be heap. It can point to {StackLoc}. [-Wpoints-to-asserts]"
                ]

        it "warns on incorrect assertions on pointers from an array" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "typedef int* Int_Ptr;"
                , "void test() {"
                , "  Int_Ptr ptr_array[2];"
                , "  int a;"
                , "  ptr_array[0] = &a;"
                , "  assert(mem_is_heap(ptr_array[0]));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:8: Static analysis check failed: Pointer 'ptr_array' does not satisfy assertion: must be heap. It can point to {StackLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn on correct assertion with non-constant array index" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "typedef int* Int_Ptr;"
                , "void test(int i) {"
                , "  Int_Ptr ptr_array[2];"
                , "  ptr_array[i] = (int*)malloc(sizeof(int));"
                , "  assert(mem_is_heap(ptr_array[i]));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns on assertion on an ambiguous pointer returned from a function" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "static const int g = 0;"
                , "const int* get_ptr(int cond) {"
                , "  if (cond) {"
                , "    return (const int*)malloc(sizeof(int));"
                , "  } else {"
                , "    return &g;"
                , "  }"
                , "}"
                , "void test(int cond) {"
                , "  const int* p = get_ptr(cond);"
                , "  assert(mem_is_heap(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:14: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be heap. It can point to {GlobalVarLoc, HeapLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn when a pointer is correctly asserted to be an external parameter" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_external_param(void* ptr);"
                , "void test(int* p) {"
                , "  assert(mem_is_external_param(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns when a stack variable is asserted to be an external parameter" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_external_param(void* ptr);"
                , "void test() {"
                , "  int a;"
                , "  int* p = &a;"
                , "  assert(mem_is_external_param(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be external param. It can point to {StackLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn for a correct negated assertion" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void test() {"
                , "  int a;"
                , "  int* p = &a;"
                , "  assert(!mem_is_heap(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns for an incorrect negated assertion" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "void test() {"
                , "  int* p = (int*)malloc(sizeof(int));"
                , "  assert(!mem_is_heap(p));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be not heap. It can point to {HeapLoc}. [-Wpoints-to-asserts]"
                ]

        it "does not warn for a correct complex negated assertion" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "int mem_is_stack(void* ptr);"
                , "void test() {"
                , "  int* p = nullptr;"
                , "  assert(!(mem_is_heap(p) || mem_is_stack(p)));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)] `shouldBe` []

        it "warns for an incorrect complex negated assertion" $ do
            ast <- mustParse
                [ "void assert(int value);"
                , "int mem_is_heap(void* ptr);"
                , "int mem_is_stack(void* ptr);"
                , "void test() {"
                , "  int a;"
                , "  int* p = &a;"
                , "  assert(!(mem_is_heap(p) || mem_is_stack(p)));"
                , "}"
                ]
            analyseGlobal ["points-to-asserts"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:7: Static analysis check failed: Pointer 'p' does not satisfy assertion: must be not (heap or stack). It can point to {StackLoc}. [-Wpoints-to-asserts]"
                ]
