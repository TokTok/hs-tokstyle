{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.TypeCheckSpec (spec) where


import           Data.Text           (unlines)
import qualified Data.Text           as T
import           Prelude             hiding (unlines)
import           Test.Hspec          (Spec, describe, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldAcceptLocal,
                                      shouldWarn, shouldWarnLocal)


handler :: [T.Text]
handler =
    [ "typedef void handler_cb(void *object, int value);"
    , "typedef struct Handler { handler_cb *function; void *object; } Handler;"
    , "void handler_set(Handler *h, handler_cb *function, void *object) {"
    , "  h->function = function;"
    , "  h->object = object;"
    , "}"
    , "void handler_call(const Handler *h, int value) { h->function(h->object, value); }"
    , "typedef struct Foo { int x; } Foo;"
    , "typedef struct Bar { int y; } Bar;"
    , "void foo_handler(void *object, int value) {"
    , "  Foo *foo = (Foo *)object;"
    , "  foo->x = value;"
    , "}"
    ]


shouldWarn' :: [T.Text] -> [[T.Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["type-check"]


shouldAccept' :: [T.Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["type-check"]


spec :: Spec
spec = do
    describe "acceptance" $ do
        it "correctly type-checks polymorphic functions" $ do
            shouldAccept'
                [ "typedef bool some_cb(void *object, int param);"
                -- void pointers are like polymorphism in Haskell.
                -- invoke :: (a -> int -> bool) -> a -> bool
                , "bool invoke(some_cb *my_callback, void *object) { return my_callback(object, 123); }"
                -- This is actually "bool int_func(int *object, int param)", inferred from the
                -- cast inside the function body.
                , "bool int_func(void *object, int param) {"
                , "  int *p = (int *)object;"
                , "  return *p == param;"
                , "}"
                -- This is actually "bool long_func(int64_t *object, int param)", inferred from the
                -- cast inside the function body. In C, it needs to be void*, but our type checker
                -- uses the cast inside to understand that it's meant to be int64_t*.
                , "bool long_func(void *object, int param) {"
                , "  int64_t *p = (int64_t *)object;"
                , "  return *p == param;"
                , "}"
                , "void main_test_func(int *iptr, int64_t *lptr) {"
                , "  invoke(int_func, iptr);"
                , "  invoke(long_func, lptr);"
                , "}"
                ]

        it "accepts valid use of void pointers for separate generic instances" $ do
            shouldAccept'
                [ "typedef struct GenericBox { void *data; } GenericBox;"
                , "void set_box(GenericBox *box, void *data) { box->data = data; }"
                , "void main_test_func(int *iptr, char *cptr) {"
                , "  GenericBox int_box;"
                , "  GenericBox char_box;"
                -- this line infers int_box to be "GenericBox<int>"
                , "  set_box(&int_box, iptr);"
                -- this line infers char_box to be "GenericBox<char>"
                , "  set_box(&char_box, cptr);"
                , "}"
                ]

        it "accepts correctly typed ternary operators" $ do
            shouldAccept'
                [ "int ternary_test(int cond) {"
                , "  return cond ? 1 : 0;"
                , "}"
                ]

        it "accepts valid struct member access" $ do
            shouldAccept'
                [ "typedef struct Foo { int x; } Foo;"
                , "void struct_access(Foo f) {"
                , "  int y = f.x;"
                , "}"
                ]

        it "accepts valid struct pointer member access" $ do
            shouldAccept'
                [ "typedef struct Foo { int x; } Foo;"
                , "void struct_ptr_access(Foo *f) {"
                , "  int y = f->x;"
                , "}"
                ]

        it "accepts a separate typedef from the struct where the typedef comes first" $ do
            let headerCode =
                    [ "typedef struct cmp_ctx_s cmp_ctx_t;"
                    , ""
                    , "typedef bool cmp_reader(cmp_ctx_t *ctx, void *data, size_t limit);"
                    , ""
                    , "struct cmp_ctx_s {"
                    , "  cmp_reader *read;"
                    , "};"
                    ]
                sourceCode =
                    [ "#include \"cmp.h\""
                    , ""
                    , "static bool read_byte(cmp_ctx_t *ctx, uint8_t *x) {"
                    , "  return ctx->read(ctx, x, sizeof(uint8_t));"
                    , "}"
                    ]
            shouldAccept ["type-check"] [("cmp.h", headerCode), ("cmp.c", sourceCode)]

        it "accepts valid array access and bitwise operations" $ do
            shouldAccept'
                [ "void data_checksum(const uint8_t *data, uint32_t length) {"
                , "    uint8_t checksum[2] = {0, 3};"
                , "    for (uint32_t i = 0; i < length; ++i) {"
                , "        checksum[i % 2] ^= data[i];"
                , "    }"
                , "}"
                ]

        it "accepts comparing an array parameter to nullptr" $ do
            shouldAccept'
                [ "bool array_is_null(const uint8_t arr[]) {"
                , "  return arr == nullptr;"
                , "}"
                ]

        it "accepts passing an array parameter to another array parameter" $ do
            shouldAccept'
                [ "void callee(const uint8_t arr[]);"
                , "void caller(const uint8_t arr[]) {"
                , "  callee(arr);"
                , "}"
                ]

        it "accepts a correct call to a variadic function" $ do
            shouldAccept'
                [ "void my_printf(const char* format, ...);"
                , "void test() {"
                , "    my_printf(\"hello %d\", 1);"
                , "}"
                ]

        it "accepts a correct call to a variadic macro" $ do
            shouldAccept'
                [ "void my_printf(const char* format, ...);"
                , "#define MY_PRINTF(format, ...) my_printf(format, __VA_ARGS__)"
                , "void test() {"
                , "    MY_PRINTF(\"hello %d\", 1);"
                , "}"
                ]

        it "accepts #define macros" $ do
            shouldAccept'
                [ "#define FOO_BAR 123"
                , "#define FLOATING_NUMBER 123.4"
                , "#define ADD_EXPR (FOO_BAR + FOO_BAR)"
                , "int get_foo(int val) {"
                , "  return FOO_BAR + val;"
                , "}"
                , "float get_foo(float val) {"
                , "  return FLOATING_NUMBER + val;"
                , "}"
                , "int get_addition() {"
                , "  return ADD_EXPR;"
                , "}"
                ]

        it "accepts #define macros with pointer assignments" $ do
            shouldAccept'
                [ "#define SET_ERROR_PARAMETER(param, x) do { if (param != nullptr) { *param = x; } } while (0)"
                , "typedef enum My_Error {"
                , "    MY_ERROR_OK,"
                , "    MY_ERROR_FAIL"
                , "} My_Error;"
                , "void my_func(My_Error *error) {"
                , "    SET_ERROR_PARAMETER(error, MY_ERROR_OK);"
                , "}"
                ]

        it "accepts functions with (void) parameter list" $ do
            shouldAccept'
                [ "int my_func(void);"
                , "void call_it() {"
                , "  my_func();"
                , "}"
                ]

    describe "function calls" $ do
        it "errors on a pointer passed as a number" $ do
            shouldWarn'
                [ "int func(int param);"
                , "void other_func(int i, char *p) {"
                , "  func(i);"
                , "  func(p);"
                , "}"
                ]
                [[ "error: `p` is `char *`, but `param` of `func` is `int` [-Wtype-check]"
                 , "   --> test.c:4:8"
                 , "    |"
                 , "   4|   func(p);"
                 , "    |        ^"
                 ]]

        it "errors on a wrong argument before the variable ones" $ do
            shouldWarn'
                [ "void my_printf(const char* format, ...);"
                , "void test() {"
                , "    my_printf(1, \"hello %d\");"
                , "}"
                ]
                [[ "error: `1` is `int`, but `format` of `my_printf` is `char *` [-Wtype-check]"
                 , "   --> test.c:3:15"
                 , "    |"
                 , "   3|     my_printf(1, \"hello %d\");"
                 , "    |               ^"
                 ]]

        it "reports what a macro does at the place that uses the macro" $ do
            shouldWarn'
                [ "void my_printf(const char* format, ...);"
                , "#define MY_PRINTF(format, ...) my_printf(format, __VA_ARGS__)"
                , "void test() {"
                , "    MY_PRINTF(1, \"hello %d\");"
                , "}"
                ]
                [[ "error: `1` is `int`, but `format` of `my_printf` is `char *` [-Wtype-check]"
                 , "   --> test.c:4:5"
                 , "    |"
                 , "   4|     MY_PRINTF(1, \"hello %d\");"
                 , "    |     ^^^^^^^^^^^^^^^^^^^^^^^"
                 ]]

        it "accepts a macro that uses the variables of the function" $ do
            shouldAccept'
                [ "typedef struct Foo { int x; int y; } Foo;"
                , "int sum(const Foo *foo) {"
                , "  int total = 0;"
                , "#define ADD_MEMBER(member) do { total += foo->member; } while (0)"
                , "  ADD_MEMBER(x);"
                , "  ADD_MEMBER(y);"
                , "#undef ADD_MEMBER"
                , "  return total;"
                , "}"
                ]

        it "says how a callback differs from the one that is wanted" $ do
            shouldWarn'
                [ "typedef struct Foo { int x; } Foo;"
                , "typedef struct Bar { int y; } Bar;"
                , "typedef int use_cb(int value, Foo *foo);"
                , "void run(use_cb *callback);"
                , "int bar_use(int value, Bar *bar);"
                , "void test() { run(bar_use); }"
                ]
                [[ "error: `bar_use` is a function that takes `Bar *`, but `callback` of `run` is a function that takes `Foo *` [-Wtype-check]"
                 , "   --> test.c:6:19"
                 , "    |"
                 , "   6| void test() { run(bar_use); }"
                 , "    |                   ^^^^^^^"
                 ]]

        it "errors on a wrong number of arguments" $ do
            shouldWarn'
                [ "int my_func(int a);"
                , "void arg_count_mismatch() {"
                , "  my_func(1, 2);"
                , "}"
                ]
                [[ "error: `my_func` takes 1 argument, but is called with 2 [-Wtype-check]"
                 , "   --> test.c:3:3"
                 , "    |"
                 , "   3|   my_func(1, 2);"
                 , "    |   ^^^^^^^^^^^^"
                 ]]

        it "accepts functions and types the program doesn't define" $ do
            shouldAccept'
                [ "void test(pthread_mutex_t *mutex, uint8_t *data) {"
                , "  pthread_mutex_lock(mutex);"
                , "  memset(data, 0, SOME_SIZE);"
                , "  struct timespec ts;"
                , "  ts.tv_sec = time(nullptr);"
                , "}"
                ]

        it "keeps static functions of the same name apart" $ do
            shouldAccept ["type-check"]
                [ ("a.c", [ "static int get(const int *p) { return *p; }"
                          , "int a(const int *p) { return get(p); }" ])
                , ("b.c", [ "static int get(const char *p) { return *p; }"
                          , "int b(const char *p) { return get(p); }" ])
                ]

    describe "void pointers" $ do
        it "errors on an object the callback uses as another type" $ do
            shouldWarn'
                [ "typedef bool some_cb(void *object, int param);"
                , "bool invoke(some_cb *my_callback, void *object) { return my_callback(object, 123); }"
                , "bool long_func(void *object, int param) {"
                , "  const int64_t *p = (const int64_t *)object;"
                , "  return *p == param;"
                , "}"
                , "void main_test_func(int *iptr) {"
                , "  invoke(long_func, iptr);"
                , "}"
                ]
                [[ "error: `iptr` is `int *`, but `object` of `invoke` is `int64_t *` [-Wtype-check]"
                 , "   --> test.c:8:21"
                 , "    |"
                 , "   4|   const int64_t *p = (const int64_t *)object;"
                 , "    |                             ^^^^^^^^^^^^^^^^"
                 , "    |                             |"
                 , "    |                             `long_func` uses `object` as `int64_t *`"
                 , "   8|   invoke(long_func, iptr);"
                 , "    |          ^^^^^^^^^"
                 , "    |          |"
                 , "    |          `int64_t *` because of this"
                 , "    |                     ^^^^"
                 ]]

        it "errors on a struct's void pointer used with two types" $ do
            shouldWarn'
                [ "typedef struct GenericBox { void *data; } GenericBox;"
                , "void set_box(GenericBox *box, void *data) { box->data = data; }"
                , "void main_test_func(int *iptr, char *cptr) {"
                , "  GenericBox box;"
                , "  set_box(&box, iptr);"
                , "  set_box(&box, cptr);"
                , "}"
                ]
                [[ "error: `cptr` is `char *`, but `data` of `set_box` is `int *` [-Wtype-check]"
                 , "   --> test.c:6:17"
                 , "    |"
                 , "   5|   set_box(&box, iptr);"
                 , "    |                 ^^^^"
                 , "    |                 |"
                 , "    |                 `int *` because of this"
                 , "   6|   set_box(&box, cptr);"
                 , "    |                 ^^^^"
                 ]]

        it "accepts a function pointer called with two types" $ do
            shouldAccept'
                [ "typedef void my_generic_cb(void *object);"
                , "void call_it(my_generic_cb *ptr, int i, int64_t l) {"
                , "  ptr(&i);"
                , "  ptr(&l);"
                , "}"
                ]

        it "errors on a void pointer cast to two types" $ do
            shouldWarn'
                [ "typedef struct Foo { int x; } Foo;"
                , "typedef struct Bar { int y; } Bar;"
                , "int both(void *object) {"
                , "  const Foo *foo = (const Foo *)object;"
                , "  const Bar *bar = (const Bar *)object;"
                , "  return foo->x + bar->y;"
                , "}"
                ]
                [[ "error: `object` is cast to `Bar *`, but it is `Foo *` [-Wtype-check]"
                 , "   --> test.c:5:27"
                 , "    |"
                 , "   4|   const Foo *foo = (const Foo *)object;"
                 , "    |                           ^^^^^^^^^^^^"
                 , "    |                           |"
                 , "    |                           `Foo *` because of this"
                 , "   5|   const Bar *bar = (const Bar *)object;"
                 , "    |                           ^^^^^^^^^^^^"
                 ]]

        it "accepts any object read as bytes" $ do
            shouldAccept'
                [ "typedef struct Foo { int x; } Foo;"
                , "typedef struct Bar { int y; } Bar;"
                , "uint8_t first(const void *data) {"
                , "  const uint8_t *bytes = (const uint8_t *)data;"
                , "  return bytes[0];"
                , "}"
                , "int both(const Foo *foo, const Bar *bar) { return first(foo) + first(bar); }"
                ]

    describe "callbacks in structs" $ do
        it "accepts a callback stored with the object it uses" $ do
            shouldAccept' $ handler ++
                [ "void setup(Handler *h, Foo *foo) { handler_set(h, foo_handler, foo); }"
                ]

        it "errors on a callback stored with another object" $ do
            shouldWarn' (handler ++
                [ "void setup(Handler *h, Bar *bar) { handler_set(h, foo_handler, bar); }"
                ])
                [[ "error: `bar` is `Bar *`, but `object` of `handler_set` is `Foo *` [-Wtype-check]"
                 , "   --> test.c:14:64"
                 , "    |"
                 , "  11|   Foo *foo = (Foo *)object;"
                 , "    |               ^^^^^^^^^^^^"
                 , "    |               |"
                 , "    |               `foo_handler` uses `object` as `Foo *`"
                 , "  14| void setup(Handler *h, Bar *bar) { handler_set(h, foo_handler, bar); }"
                 , "    |                                                   ^^^^^^^^^^^"
                 , "    |                                                   |"
                 , "    |                                                   `Foo *` because of this"
                 , "    |                                                                ^^^"
                 ]]

        it "accepts handlers for different objects in an array" $ do
            shouldAccept' $ handler ++
                [ "void bar_handler(void *object, int value) {"
                , "  Bar *bar = (Bar *)object;"
                , "  bar->y = value;"
                , "}"
                , "typedef struct Handlers { Handler handlers[2]; } Handlers;"
                , "void setup(Handlers *hs, Foo *foo, Bar *bar) {"
                , "  handler_set(&hs->handlers[0], foo_handler, foo);"
                , "  handler_set(&hs->handlers[1], bar_handler, bar);"
                , "}"
                , "void call(const Handlers *hs) {"
                , "  for (int i = 0; i < 2; ++i) {"
                , "    hs->handlers[i].function(hs->handlers[i].object, i);"
                , "  }"
                , "}"
                ]

        it "accepts a callback that is called with any type" $ do
            shouldAccept'
                [ "typedef bool write_cb(void *ctx, const void *data, uint32_t size);"
                , "typedef struct Writer { write_cb *write; void *ctx; } Writer;"
                , "bool write_all(const Writer *w, uint16_t a, uint32_t b) {"
                , "  return w->write(w->ctx, &a, sizeof(a)) && w->write(w->ctx, &b, sizeof(b));"
                , "}"
                ]

        it "errors on an object that is used as another type elsewhere" $ do
            shouldWarn'
                [ "typedef struct Foo { int x; } Foo;"
                , "typedef struct Bar { int y; } Bar;"
                , "typedef struct Box { void *data; } Box;"
                , "typedef struct Shelf { Box *box; } Shelf;"
                , "int shelf_get(const Shelf *shelf) {"
                , "  const Foo *foo = (const Foo *)shelf->box->data;"
                , "  return foo->x;"
                , "}"
                , "void shelf_set(Shelf *shelf, Bar *bar) { shelf->box->data = bar; }"
                ]
                [[ "error: `bar` is `Bar *`, but it is used as `Foo *` [-Wtype-check]"
                 , "   --> test.c:9:61"
                 , "    |"
                 , "   6|   const Foo *foo = (const Foo *)shelf->box->data;"
                 , "    |                           ^^^^^^^^^^^^^^^^^^^^^^"
                 , "    |                           |"
                 , "    |                           used as `Foo *` here"
                 , "   9| void shelf_set(Shelf *shelf, Bar *bar) { shelf->box->data = bar; }"
                 , "    |                                                             ^^^"
                 ]]

        it "accepts objects of two types that are each used as their type" $ do
            shouldAccept'
                [ "typedef struct Foo { int x; } Foo;"
                , "typedef struct Bar { int y; } Bar;"
                , "typedef struct Box { void *data; } Box;"
                , "int foo_get(const Box *box) { const Foo *foo = (const Foo *)box->data; return foo->x; }"
                , "int bar_get(const Box *box) { const Bar *bar = (const Bar *)box->data; return bar->y; }"
                , "void foo_set(Box *box, Foo *foo) { box->data = foo; }"
                , "void bar_set(Box *box, Bar *bar) { box->data = bar; }"
                ]

        it "errors on an object given to functions that use another type" $ do
            shouldWarn'
                [ "typedef struct Foo { int x; } Foo;"
                , "typedef struct Bar { int y; } Bar;"
                , "typedef void run_cb(void *self);"
                , "typedef struct Funcs { run_cb *run; } Funcs;"
                , "typedef struct Runner { const Funcs *funcs; void *self; } Runner;"
                , "void runner_run(const Runner *runner) { runner->funcs->run(runner->self); }"
                , "static void foo_run(void *self) {"
                , "  Foo *foo = (Foo *)self;"
                , "  foo->x = 1;"
                , "}"
                , "static const Funcs foo_funcs = {foo_run};"
                , "void runner_init(Runner *runner, Bar *bar) {"
                , "  runner->funcs = &foo_funcs;"
                , "  runner->self = bar;"
                , "}"
                ]
                [[ "error: `bar` is `Bar *`, but it is used as `Foo *` [-Wtype-check]"
                 , "   --> test.c:14:18"
                 , "    |"
                 , "  11| static const Funcs foo_funcs = {foo_run};"
                 , "    |                                 ^^^^^^^"
                 , "    |                                 |"
                 , "    |                                 used as `Foo *` here"
                 , "  14|   runner->self = bar;"
                 , "    |                  ^^^"
                 ]]

        it "accepts a callback and object set differently in two branches" $ do
            shouldAccept'
                [ "typedef struct Foo { int x; } Foo;"
                , "typedef int time_cb(void *user_data);"
                , "typedef struct Clock { time_cb *callback; void *user_data; int time; } Clock;"
                , "int clock_now(const Clock *clock) { return clock->callback(clock->user_data); }"
                , "static int default_time(void *user_data) {"
                , "  const Clock *clock = (const Clock *)user_data;"
                , "  return clock->time;"
                , "}"
                , "void clock_set(Clock *clock, time_cb *callback, void *user_data) {"
                , "  if (callback == nullptr) {"
                , "    clock->callback = default_time;"
                , "    clock->user_data = clock;"
                , "  } else {"
                , "    clock->callback = callback;"
                , "    clock->user_data = user_data;"
                , "  }"
                , "}"
                , "static int foo_time(void *user_data) {"
                , "  const Foo *foo = (const Foo *)user_data;"
                , "  return foo->x;"
                , "}"
                , "void setup(Clock *clock, Foo *foo) { clock_set(clock, foo_time, foo); }"
                ]

    describe "structs" $ do
        it "errors on another struct" $ do
            shouldWarn'
                [ "typedef struct Foo { int x; } Foo;"
                , "typedef struct Bar { int y; } Bar;"
                , "void struct_assign(Foo f) {"
                , "  Bar b;"
                , "  b = f;"
                , "}"
                ]
                [[ "error: `f` is `Foo`, but `b` is `Bar` [-Wtype-check]"
                 , "   --> test.c:5:7"
                 , "    |"
                 , "   5|   b = f;"
                 , "    |       ^"
                 ]]

        it "errors on a member the struct doesn't have" $ do
            shouldWarn'
                [ "typedef struct Foo { int x; } Foo;"
                , "int struct_access(Foo f, const Foo *p) {"
                , "  return f.y + p->z;"
                , "}"
                ]
                [[ "error: `Foo` has no member `y` [-Wtype-check]"
                 , "   --> test.c:3:10"
                 , "    |"
                 , "   3|   return f.y + p->z;"
                 , "    |          ^^^"
                 ]
                ,[ "error: `Foo` has no member `z` [-Wtype-check]"
                 , "   --> test.c:3:16"
                 , "    |"
                 , "   3|   return f.y + p->z;"
                 , "    |                ^^^^"
                 ]]

        it "accepts a struct defined in another file" $ do
            shouldAccept ["type-check"]
                [ ("test.h", [ "typedef struct Foo Foo;", "int foo_x(const Foo *foo);" ])
                , ("test.c", [ "struct Foo { int x; };", "int foo_x(const Foo *foo) { return foo->x; }" ])
                ]

        it "accepts members of a nested struct through a pointer and an array" $ do
            shouldAccept'
                [ "typedef struct Inner { int values[4]; } Inner;"
                , "typedef struct Outer { Inner *inner; Inner inners[2]; } Outer;"
                , "int get(const Outer *outer, int i) {"
                , "  return outer->inner->values[i] + outer->inners[1].values[i];"
                , "}"
                ]

        it "checks the values a struct is made from" $ do
            shouldWarn'
                [ "typedef struct Foo { int x; const char *name; } Foo;"
                , "void make(int *p) {"
                , "  const Foo zero = {0};"
                , "  const Foo good = {1, \"one\"};"
                , "  const Foo bad = {1, p};"
                , "}"
                ]
                [[ "error: `p` is `int *`, but `bad.name` is `char *` [-Wtype-check]"
                 , "   --> test.c:5:23"
                 , "    |"
                 , "   5|   const Foo bad = {1, p};"
                 , "    |                       ^"
                 ]]

        it "accepts union members" $ do
            shouldAccept'
                [ "typedef union Value { int i; const char *s; } Value;"
                , "int get(Value v, const Value *p) { return v.i + p->i; }"
                ]

    describe "values" $ do
        it "errors on a returned value of another type" $ do
            shouldWarn'
                [ "int get_int() {"
                , "  return \"hello\";"
                , "}"
                ]
                [[ "error: `\"hello\"` is `char *`, but `get_int` returns `int` [-Wtype-check]"
                 , "   --> test.c:2:10"
                 , "    |"
                 , "   2|   return \"hello\";"
                 , "    |          ^^^^^^^"
                 ]]

        it "errors on a value returned from a void function" $ do
            shouldWarn'
                [ "void my_void_func() {"
                , "  return 123;"
                , "}"
                ]
                [[ "error: `123` is `int`, but `my_void_func` returns `void` [-Wtype-check]"
                 , "   --> test.c:2:10"
                 , "    |"
                 , "   2|   return 123;"
                 , "    |          ^^^"
                 ]]

        it "errors on a pointer to another type" $ do
            shouldWarn'
                [ "void ptr_mismatch(int *p) {"
                , "  char *c = p;"
                , "}"
                ]
                [[ "error: `p` is `int *`, but `c` is `char *` [-Wtype-check]"
                 , "   --> test.c:2:13"
                 , "    |"
                 , "   2|   char *c = p;"
                 , "    |             ^"
                 ]]

        it "errors on a ternary with two types" $ do
            shouldWarn'
                [ "int ternary_mismatch(int cond) {"
                , "  return cond ? 1 : \"hello\";"
                , "}"
                ]
                [[ "error: `\"hello\"` is `char *`, but `1` is `int` [-Wtype-check]"
                 , "   --> test.c:2:21"
                 , "    |"
                 , "   2|   return cond ? 1 : \"hello\";"
                 , "    |                     ^^^^^^^"
                 ]]

        it "errors on arithmetic with a struct" $ do
            shouldWarn'
                [ "typedef struct Foo { int x; } Foo;"
                , "int add(Foo foo) { return foo * 2; }"
                ]
                [[ "error: `foo` is `Foo`, but `*` needs a number [-Wtype-check]"
                 , "   --> test.c:2:27"
                 , "    |"
                 , "   2| int add(Foo foo) { return foo * 2; }"
                 , "    |                           ^^^"
                 ]]

        it "accepts numbers of any type for each other" $ do
            shouldAccept'
                [ "typedef enum Colour { COLOUR_RED, COLOUR_BLUE } Colour;"
                , "enum { MAX_THINGS = 3 };"
                , "#define FACTOR 2"
                , "uint32_t conversion_test(uint8_t val8, Colour colour, bool flag) {"
                , "  uint32_t val32 = MAX_THINGS;"
                , "  val32 += val8 * FACTOR;"
                , "  const char c = 'a' + colour;"
                , "  if (123) { return flag; }"
                , "  return val32 + c + COLOUR_BLUE;"
                , "}"
                ]

        it "accepts nullptr for any pointer" $ do
            shouldAccept'
                [ "void takes_ptr(uint8_t *ptr);"
                , "bool some_bool();"
                , "uint8_t *nullptr_test(uint8_t *ptr) {"
                , "  takes_ptr(nullptr);"
                , "  if (ptr == nullptr) { ptr = nullptr; }"
                , "  return some_bool() ? ptr : nullptr;"
                , "}"
                ]
