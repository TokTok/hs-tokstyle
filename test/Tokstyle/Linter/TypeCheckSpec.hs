{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.TypeCheckSpec where

import           Test.Hspec          (Spec, describe, it, shouldBe)

import           Tokstyle.Linter     (analyseGlobal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    describe "acceptance" $ do
        it "correctly type-checks polymorphic functions" $ do
            ast <- mustParse
                -- `void*` behaves like a generic type variable in Haskell.
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
                -- This is actually "bool char_func(char *object, int param)", inferred from the
                -- cast inside the function body. In C, it needs to be void*, but our type checker
                -- uses the cast inside to understand that it's meant to be char*.
                , "bool char_func(void *object, int param) {"
                , "  char *p = (char *)object;"
                , "  return *p == 'a';"
                , "}"
                , "void main_test_func(int *iptr, char *cptr) {"
                , "  invoke(int_func, iptr);"
                , "  invoke(char_func, cptr);"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts valid use of void pointers for separate generic instances" $ do
            ast <- mustParse
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
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts correctly typed ternary operators" $ do
            ast <- mustParse
                [ "int ternary_test(int cond) {"
                , "  return cond ? 1 : 0;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts valid struct member access" $ do
            ast <- mustParse
                [ "typedef struct Foo { int x; } Foo;"
                , "void struct_access(Foo f) {"
                , "  int y = f.x;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts valid struct pointer member access" $ do
            ast <- mustParse
                [ "typedef struct Foo { int x; } Foo;"
                , "void struct_ptr_access(Foo *f) {"
                , "  int y = f->x;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts a separate typedef from the struct where the typedef comes first" $ do
            headerAst <- mustParse
                [ "typedef struct cmp_ctx_s cmp_ctx_t;"
                , ""
                , "typedef bool cmp_reader(cmp_ctx_t *ctx, void *data, size_t limit);"
                , ""
                , "struct cmp_ctx_s {"
                , "  cmp_reader *read;"
                , "};"
                ]
            sourceAst <- mustParse
                [ "#include \"cmp.h\""
                , ""
                , "static bool read_byte(cmp_ctx_t *ctx, uint8_t *x) {"
                , "  return ctx->read(ctx, x, sizeof(uint8_t));"
                , "}"
                ]
            analyseGlobal ["type-check"] [("cmp.h", headerAst), ("cmp.c", sourceAst)]
                `shouldBe` []

        it "accepts valid array access and bitwise operations" $ do
            ast <- mustParse
                [ "void data_checksum(const uint8_t *data, uint32_t length) {"
                , "    uint8_t checksum[2] = {0, 3};"
                , "    for (uint32_t i = 0; i < length; ++i) {"
                , "        checksum[i % 2] ^= data[i];"
                , "    }"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts comparing an array parameter to nullptr" $ do
            ast <- mustParse
                [ "bool array_is_null(const uint8_t arr[]) {"
                , "  return arr == nullptr;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts passing an array parameter to another array parameter" $ do
            ast <- mustParse
                [ "void callee(const uint8_t arr[]);"
                , "void caller(const uint8_t arr[]) {"
                , "  callee(arr);"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts a correct call to a variadic function" $ do
            ast <- mustParse
                [ "void my_printf(const char* format, ...);"
                , "void test() {"
                , "    my_printf(\"hello %d\", 1);"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts a correct call to a variadic macro" $ do
            ast <- mustParse
                [ "void my_printf(const char* format, ...);"
                , "#define MY_PRINTF(format, ...) my_printf(format, __VA_ARGS__)"
                , "void test() {"
                , "    MY_PRINTF(\"hello %d\", 1);"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts #define macros" $ do
            ast <- mustParse
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
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts #define macros with pointer assignments" $ do
            ast <- mustParse
                [ "#define SET_ERROR_PARAMETER(param, x) do { if (param != nullptr) { *param = x; } } while (0)"
                , "typedef enum My_Error {"
                , "    MY_ERROR_OK,"
                , "    MY_ERROR_FAIL"
                , "} My_Error;"
                , "void my_func(My_Error *error) {"
                , "    SET_ERROR_PARAMETER(error, MY_ERROR_OK);"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts functions with (void) parameter list" $ do
            ast <- mustParse
                [ "int my_func(void);"
                , "void call_it() {"
                , "  my_func();"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

    describe "hindley-milner type failures" $ do
        it "warns about incompatible function calls" $ do
            ast <- mustParse
                [ "int func(int param);"
                , "void other_func(int i, char *p) {"
                , "  func(i);"
                , "  func(p);"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:4: type mismatch in function call (argument 1): expected int, but got char* [-Wtype-check]"
                ]

        it "errors when a variadic function call has the wrong type" $ do
            ast <- mustParse
                [ "void my_printf(const char* format, ...);"
                , "void test() {"
                , "    my_printf(1, \"hello %d\");"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: type mismatch in function call (argument 1): expected char*, but got int [-Wtype-check]"
                ]

        it "errors when a variadic macro call has the wrong type" $ do
            ast <- mustParse
                [ "void my_printf(const char* format, ...);"
                , "#define MY_PRINTF(format, ...) my_printf(format, __VA_ARGS__)"
                , "void test() {"
                , "    MY_PRINTF(1, \"hello %d\");"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:4: type mismatch in function call (argument 1): expected char*, but got int [-Wtype-check]"
                ]

--      it "warns about incompatible polymorphic functions" $ do
--          ast <- mustParse
--              -- Equivalent to Haskell
--              -- `type SomeCb a = a -> int -> bool`.
--              [ "typedef bool some_cb(void *object, int param);"
--              -- void pointers are like polymorphism in Haskell.
--              -- `invoke :: SomeCb a -> a -> bool`
--              -- `invoke` is a function that at first looks like its `some_cb<a>` and `object :: b` are different `a` and `b`,
--              -- but since the body contains `my_callback(object, 123)`, the algorithm should then infer that `a` and `b`
--              -- are actually the same, i.e. they need to be unified.
--              , "bool invoke(some_cb *my_callback, void *object) { return my_callback(object, 123); }"
--              -- inferred as "bool char_func(char *object, int param)"
--              , "bool char_func(void *object, int param) {"
--              , "  char *p = (char *)object;"
--              , "  return *p == 'a';"
--              , "}"
--              , "void main_test_func(int *iptr, char *cptr) {"
--              -- this should error because char_func requires its void pointer to be a char pointer (inferred based on the cast in the function).
--              , "  invoke(char_func, iptr);"
--              , "}"
--              ]
--          analyseGlobal ["type-check"] [("test.c", ast)]
--              `shouldBe`
--              [ "test.c:12: type mismatch: int* vs char* [-Wtype-check]"
--              ]

--      it "warns on incorrect use of void pointers for generics" $ do
--          ast <- mustParse
--              -- GenericBox<T> making the void pointer inside like a generic T.
--              [ "typedef struct GenericBox { void *data; } GenericBox;"
--              -- inferred as something like "void set_box(GenericBox<T> *box, T *data)".
--              , "void set_box(GenericBox *box, void *data) { box->data = data; }"
--              , "void main_test_func(int *iptr, char *cptr) {"
--              -- here, the box is still generic, we don't know what the type inside is, yet.
--              , "  GenericBox box;"
--              -- this line infers box to be "GenericBox<int>"
--              , "  set_box(&box, iptr);"
--              -- this line errors because box is "GenericBox<int>" and "set_box(GenericBox<int> *box, int *data)"
--              -- is incompatible with passing a char pointer.
--              , "  set_box(&box, cptr);"
--              , "}"
--              ]
--          analyseGlobal ["type-check"] [("test.c", ast)]
--              `shouldBe`
--              [ "test.c:6: type mismatch: char* vs int* [-Wtype-check]"
--              ]

    describe "Function Pointers" $ do
        it "warns when calling a function pointer with wrong argument types" $ do
            ast <- mustParse
                -- my_func_cb here is not generic.
                [ "typedef int my_func_cb(int i, char c);"
                , "void call_it(my_func_cb *ptr) {"
                , "  ptr(1, 'a');"
                , "  ptr(1, 2);"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:4: type mismatch in function call (argument 2): expected char, but got int [-Wtype-check]"
                ]

        it "warns when calling a generic function pointer with incompatible types" $ do
            ast <- mustParse
                -- kind of like my_generic_cb<T>
                [ "typedef void my_generic_cb(void *object);"
                -- initially, we don't know what the complete type of `call_it` is.
                -- we start out with `void call_it(my_generic_cb<?> *ptr, int i, char c)`.
                , "void call_it(my_generic_cb *ptr, int i, char c) {"
                -- this line infers my_generic_cb<int>
                -- now we know that the type of call_it is actually: `void call_it(my_generic_cb<int> *ptr, int i, char c)`
                , "  ptr(&i);"
                -- this line errors because ptr is already my_generic_cb<int>, and this expression infers it to be
                -- my_generic_cb<char>, which fails unification.
                , "  ptr(&c);"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:4: type mismatch in function call (argument 1): expected int*, but got char* [-Wtype-check]"
                ]

    describe "Structs" $ do
        it "warns on incompatible struct assignment" $ do
            ast <- mustParse
                [ "typedef struct Foo { int x; } Foo;"
                , "typedef struct Bar { int y; } Bar;"
                , "void struct_assign(Foo f) {"
                , "  Bar b;"
                , "  b = f;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:5: type mismatch in assignment: expected struct Bar, but got struct Foo [-Wtype-check]"
                ]

        it "warns on incorrect struct member access" $ do
            ast <- mustParse
                [ "typedef struct Foo { int x; } Foo;"
                , "void struct_access(Foo f) {"
                , "  int y = f.y;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: Struct Foo has no member: y [-Wtype-check]"
                ]

        it "warns on incorrect struct pointer member access" $ do
            ast <- mustParse
                [ "typedef struct Foo { int x; } Foo;"
                , "void struct_ptr_access(Foo *f) {"
                , "  int y = f->y;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: Struct Foo has no member: y [-Wtype-check]"
                ]

        it "correctly type-checks structs defined in a separate header file" $ do
            headerAst <- mustParse
                [ "typedef struct Bar { int z; } Bar;"
                ]
            sourceAst <- mustParse
                [ "#include \"bar.h\""
                , "void struct_access_from_header(Bar b) {"
                , "  int w = b.z;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("bar.h", headerAst), ("test.c", sourceAst)]
                `shouldBe` []

        it "correctly type-checks structs defined inside a header guard" $ do
            headerAst <- mustParse
                [ "#ifndef TEST_H_INCLUDED"
                , "#define TEST_H_INCLUDED"
                , "typedef struct Bar { int z; } Bar;"
                , "#endif /* TEST_H_INCLUDED */"
                ]
            sourceAst <- mustParse
                [ "#include \"bar.h\""
                , "void struct_access_from_header(Bar b) {"
                , "  int w = b.z;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("bar.h", headerAst), ("test.c", sourceAst)]
                `shouldBe` []

        it "warns on incorrect struct member access from a separate header file" $ do
            headerAst <- mustParse
                [ "typedef struct Bar { int z; } Bar;"
                ]
            sourceAst <- mustParse
                [ "#include \"bar.h\""
                , "void struct_access_from_header(Bar b) {"
                , "  int w = b.a;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("bar.h", headerAst), ("test.c", sourceAst)]
                `shouldBe`
                [ "test.c:3: Struct Bar has no member: a [-Wtype-check]"
                ]

        it "correctly type-checks structs with different tag and typedef names" $ do
            ast <- mustParse
                [ "typedef struct MyStructTag { int x; } MyStructTypedef;"
                , "void my_func(MyStructTypedef s) {"
                , "  int y = s.x;"
                , "}"
                , "void my_other_func(struct MyStructTag s) {"
                , "  int z = s.x;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "correctly type-checks structs with a separate typedef" $ do
            ast <- mustParse
                [ "struct MyStruct { int x; };"
                , "typedef struct MyStruct MyStruct_t;"
                , "void my_func(MyStruct_t s) {"
                , "  int y = s.x;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts accessing members of a nested struct through a pointer and array" $ do
            ast <- mustParse
                [ "struct InnerStruct {"
                , "    int inner_member;"
                , "};"
                , "struct OuterStruct {"
                , "    struct InnerStruct inner_array[10];"
                , "};"
                , "void test_function(struct OuterStruct *outer) {"
                , "    outer->inner_array[0].inner_member = 123;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts struct initialization with an initializer list" $ do
            ast <- mustParse
                [ "typedef struct Packet {"
                , "  const uint8_t *data;"
                , "  uint16_t len;"
                , "} Packet;"
                , "void send_packet(const uint8_t *packet_data, size_t len) {"
                , "  const Packet packet = {packet_data, (uint16_t)len};"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

    describe "Unions" $ do
        it "accepts valid union member access" $ do
            ast <- mustParse
                [ "typedef union MyUnion { int i; float f; } MyUnion;"
                , "void union_access(MyUnion u) {"
                , "  int x = u.i;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "accepts valid union pointer member access" $ do
            ast <- mustParse
                [ "typedef union MyUnion { int i; float f; } MyUnion;"
                , "void union_ptr_access(MyUnion *u) {"
                , "  int x = u->i;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "warns on incorrect union member access" $ do
            ast <- mustParse
                [ "typedef union MyUnion { int i; float f; } MyUnion;"
                , "void union_access(MyUnion u) {"
                , "  int x = u.z;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: Union MyUnion has no member: z [-Wtype-check]"
                ]

        it "warns on incorrect union pointer member access" $ do
            ast <- mustParse
                [ "typedef union MyUnion { int i; float f; } MyUnion;"
                , "void union_ptr_access(MyUnion *u) {"
                , "  int x = u->z;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: Union MyUnion has no member: z [-Wtype-check]"
                ]

    describe "Return Types" $ do
        it "warns when a function returns a value of the wrong type" $ do
            ast <- mustParse
                [ "int get_int() {"
                , "  return \"hello\";"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:2: type mismatch in return value: expected int, but got char* [-Wtype-check]"
                ]

        it "allows returning nullptr for a pointer type" $ do
            ast <- mustParse
                [ "uint8_t *get_ptr() {"
                , "  return nullptr;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

    describe "Pointer Type Mismatches" $ do
        it "warns on assignment of incompatible pointer types" $ do
            ast <- mustParse
                [ "void ptr_mismatch(int *p) {"
                , "  char *c = p;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:2: type mismatch in initialisation of c: expected char*, but got int* [-Wtype-check]"
                ]

    describe "Ternary Operator Mismatches" $ do
        it "warns when ternary branches have incompatible types" $ do
            ast <- mustParse
                [ "int ternary_mismatch(int cond) {"
                , "  return cond ? 1 : \"hello\";"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:2: type mismatch in ternary expression: expected int, but got char* [-Wtype-check]"
                ]

    describe "Integer Conversions" $ do
        it "allows implicit conversion from uint8_t to uint32_t" $ do
            ast <- mustParse
                [ "void conversion_test(uint8_t val8) {"
                , "  uint32_t val32 = 0;"
                , "  val32 += val8;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

    describe "nullptr" $ do
        it "allows assigning nullptr to a pointer type" $ do
            ast <- mustParse
                [ "void assign_nullptr() {"
                , "  uint8_t *ptr;"
                , "  ptr = nullptr;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "allows passing nullptr to a function expecting a pointer" $ do
            ast <- mustParse
                [ "void takes_ptr(uint8_t *ptr);"
                , "void pass_nullptr() {"
                , "  takes_ptr(nullptr);"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

        it "allows returning nullptr from a ternary operator" $ do
            ast <- mustParse
                [ "uint8_t *some_func();"
                , "bool some_bool();"
                , "uint8_t *ternary_nullptr() {"
                , "  return some_bool() ? some_func() : nullptr;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

    describe "Function Argument Count" $ do
        it "warns on wrong number of arguments to a function" $ do
            ast <- mustParse
                [ "int my_func(int a);"
                , "void arg_count_mismatch() {"
                , "  my_func(1, 2);"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: mismatched number of arguments in function call: expected 1, but got 2 [-Wtype-check]"
                ]

    describe "Void Return" $ do
        it "warns on returning a value from a void function" $ do
            ast <- mustParse
                [ "void my_void_func() {"
                , "  return 123;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:2: type mismatch in return value: expected void, but got int [-Wtype-check]"
                ]

    describe "Implicit bool" $ do
        it "warns when a non-bool type is used implicitly as bool" $ do
            ast <- mustParse
                [ "void my_void_func() {"
                , "  if (123) { return; }"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", ast)]
                `shouldBe` []

    describe "Enums" $ do
        it "accepts casting an enum to an int" $ do
            astH <- mustParse
                [ "#ifndef TEST_H_INCLUDED"
                , "#define TEST_H_INCLUDED"
                , "typedef enum MyEnum { MY_ENUM_ABC, MY_ENUM_DEF, MY_ENUM_GHI } MyEnum;"
                , "void take_int(int i);"
                , "#endif /* TEST_H_INCLUDED */"
                ]
            astC <- mustParse
                [ "int enum_cast(MyEnum e) {"
                , "  int x = e;"
                , "  int y = MY_ENUM_DEF;"
                , "  take_int(MY_ENUM_ABC);"
                , "  return e;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.h", astH), ("test.c", astC)]
                `shouldBe` []

        it "accepts unnamed enums" $ do
            astH <- mustParse
                [ "enum { MY_ENUM_ABC, MY_ENUM_DEF, MY_ENUM_GHI };"
                , "void take_int(int i);"
                ]
            astC <- mustParse
                [ "int enum_cast() {"
                , "  take_int(MY_ENUM_ABC);"
                , "  return MY_ENUM_DEF;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.h", astH), ("test.c", astC)]
                `shouldBe` []

        it "errors when a name is misspelled" $ do
            astH <- mustParse
                [ "typedef enum MyEnum { MY_ENUM_ABC } MyEnum;"
                ]
            astC <- mustParse
                [ "int enum_cast(MyEnum e) {"
                , "  int y = MY_ENUM_NOT_AVAILABLE;"
                , "  return y;"
                , "}"
                ]
            analyseGlobal ["type-check"] [("test.c", astH), ("test.c", astC)]
                `shouldBe`
                [ "test.c:2: Unbound constant: MY_ENUM_NOT_AVAILABLE [-Wtype-check]"
                ]
