# Cimple-based linters (`check-cimple`)

There are currently 36 linters implemented, out of which 8 perform global analyses.
In the list below, the global ones are marked specially.

## `-Wassert`

Checks whether `assert` is side-effect-free. Only pure expressions
(no function calls, no assignments) and an allowlist of exemptions are permitted
within `assert`. The current list of exemptions is:

- `make_family`
- `memcmp`
- `shared_key_is_empty`
- `tox_events_get_size`

**Reason:** `assert` is compiled out in `NDEBUG` builds, so should not influence
logic of the code in debug modes to avoid different behaviours in different
compilation modes.

## `-Wboolean-return`

Checks for functions that always return constant integers and thus seem to be
semantically boolean functions. E.g. a function returning -1 for error and 0 for
success should rather return `false` for error and `true` for success and change
its return type to `bool`.

**Reason:** boolean returns using `bool` (or an `enum` type) are clearer than
ones returning an `int` that happens to only have 2 possible values.

## `-Wbooleans`

Checks for if/else statements that return true/false and could be simplified to
just return. E.g.:

```cpp
bool foo(void) {
  if (check_something()) {
    return false;
  }
  return true;
}
```

could be simplified to:

```cpp
bool foo(void) {
  return !check_something();
}
```

Also checks for the use of `true` or `false` in binary expressions. E.g.
`a == true` should be `a` and `a != true` should be `!a`.

**Reason:** simpler code is easier to read.

## `-Wcallback-names`

Checks for naming conventions for callbacks. Callback names should end in
`callback`, but the following list of suffixes is permitted:

- `callback`
- `cb`
- `function`
- `handler`

**Reason:** naming conventions help quickly understand the code.

## `-Wcallgraph` (global)

Performs various call graph related checks:

- There should be no unused functions. Even unused `extern` functions are not
  permitted, except for the exported library interface.
- Only a subset of standard library, POSIX, WinAPI, or Darwin API functions are
  allowed. Any use of unvetted functions (such as `setjmp`) is not permitted.
- Recursion is not allowed outside of a few exemptions that should be fixed.
  Code should be written to use iteration, instead, possibly with a manually
  managed stack to keep intermediate results for algorithms like DFS.

**Reason:**

- Unused symbols require useless maintenance.
- We want to keep control over how much of the standard library we use.
- Unbounded recursion can cause stack overflows and makes it impossible to
  statically determine the maximum stack memory requirements of a program, which
  is especially useful in embedded software.

## `-Wcalloc-args`

Checks that `mem_alloc`, `mem_valloc`, and `mem_vrealloc` are used correctly:

- The `size` argument (e.g. for `mem_alloc`, the second argument) should be a
  pure `sizeof` expression without additions or multiplications.
- There should be no `sizeof` in the `nmemb` argument of a memory allocation
  call.

**Reason:** we want to avoid arbitrary computations in allocation sizes to
ensure the allocation size is exactly correct for the type of the object
being allocated.

## `-Wcalloc-type`

Checks that `mem_alloc` and other `calloc`-like functions are cast to the
correct type. The types in the `sizeof` expression and the type-cast expression
must be the same. Also, `calloc`-like functions should not be used for built-in
types such as `uint8_t` arrays. For this, use `mem_balloc`, instead.

**Reason:** ensures that the allocation size is appropriate for the allocated
object. This makes allocation functions behave more like C++ `new`. For byte
arrays, we provide a separate function that doesn't need to zero out its memory
for efficiency and to make it easier to detect logic errors using msan or
valgrind that can detect uninitialised memory use.

## `-Wcompound-init`

Checks that compound literals aren't used in initialisations. E.g.:

```cpp
Foo foo = (Foo){0};
```

should be written as:

```cpp
Foo foo = {0};
```

**Reason:** compound literals aren't needed in initialisations. Without them,
the code is clearer.

## `-Wconstness`

Warns if a variable can be marked as `const`, i.e. it is only initialised and
then never assigned again. Pointer types are exempt, i.e. `int *p = get_p();`
is fine and doesn't need to be written as `int *const p = get_p();`, but
`int q = get_q();`, if then `q` is never assigned again, should be written as
`const int q = get_q();`.

**Reason:** `const` makes the no-assign local invariant clear. We exempt pointer
types at the moment, because making that change in toxcore would be a lot of
work and we perceive less value in that than in local integer constants, since
pointers, especially aggregate object pointers, already change less often.

## `-Wdeclared-once` (global)

Checks that any function is declared exactly once.

**Reason:** functions should never be declared in multiple files, and within the
same file, declaring it twice is unnecessary and confusing.

## `-Wdecls-have-defns` (global)

Checks that all function declarations also have matching definitions.

**Reason:** extern function declarations without definitions are not implemented
and cannot be used. This likely means the declaration was forgotten when
deleting a function.

## `-Wdoc-comments` (global)

Checks that doc comments on function definitions match the ones on their
corresponding declarations.

**Reason:** ideally, documentation should be only in one place, but if it is
duplicated, it should not be different.

## `-Wenum-defines`

Suggests using `enum` instead of a sequence of `#define`s for enumerations.
Only matches sequences of `#define`s longer than 5 to avoid some false positives.
Also, the sequence must have a common prefix of at least 2 components. I.e.
`AAA` is not a sufficient common prefix, but `AAA_BBB` is.
Lastly, we only require enums for small-int enums, i.e. all enumerators have a
constant int expression value less than or equal to 255.

**Reason:** `enum` constants are safer, and can potentially be type-checked
more thoroughly.

## `-Wenum-from-int` (global)

Checks that `_from_int` functions for `enum`s are complete.

**Reason:** ensures that no enumerators are missed in conversion functions that
turn `int`s into `enum`s. Type-cast is not permitted, because some values of
type `int` are not in the enumeration.

## `-Wenum-names`

Checks that `enum` value constants have the same prefix as the `enum` type,
except they should be SCREAMING_CASE instead of Camel_Snake. There are currently
37 exemptions to this rule. New enums should follow the naming convention.

**Reason:** this naming convention helps identify the type of an `enum` constant
at first glance.

## `-Wenum-to-string` (global)

Checks that `_to_string` functions for `enum`s are complete.

**Reason:** we provide `to_string` functions for `enum` but don't want to
manually maintain them. This linter checks that the function is exactly what
we want it to be, and the error message will say what the function should look
like.

## `-Wenum-unpack` (global)

Checks that `_unpack` functions for `enum`s are complete.

**Reason:** we provide `unpack` functions for `enum` but don't want to
manually maintain them. This linter checks that the function is exactly what
we want it to be, and the error message will say what the function should look
like.

## `-Wfunc-prototypes`

Checks that empty parameter lists in C functions are written as `(void)`.

**Reason:** old-style empty parameter lists written as `()` are risky, because
C interprets them as variadic. GCC warns about this but sometimes misses one.

## `-Wfunc-scopes`

Checks that static function definitions are marked with `static`.

In C, a function is `static` even if the definition doesn't use `static`, but
there happens to be another declaration of the function which does.

**Reason:** static/extern qualification of functions should be visible locally.
It takes mental effort otherwise to look up the declaration to check for storage
qualifiers.

## `-Wglobal-funcs`

Checks that no extern functions are declared in .c files.

Extern functions must only be declared in .h files. In .c files all declarations
must be static.

**Reason:** extern declarations in .c files mean that we depend on a function
not declared in a .h file we can include. This means we're depending on an
unexported implementation detail, and there is no compiler that can check
whether our declaration matches the implementation's definition.

## `-Wlarge-struct-params`

Checks that large structs are passed by pointer rather than by value.

Exemptions are enums and some well-known small structs:

- `Family`
- `IP4`
- `Logger_Level`
- `Net_Packet_Type`
- `Onion_Connection_Status`
- `Packet`
- `Packet_Direction`
- `Socket`
- `State_Type`

and anything with one of the following prefixes, which are probably enums:

- `Group_`
- `MSI`
- `Tox_`
- `Toxav_`

**Reason:** some structs in toxcore are up to 5MB in size, which would cause
stack overflows. Since we can't currently measure the size, we avoid any struct
passing altogether apart from some well-known exemptions.

## `-Wlogger-calls`

Checks that the format argument in LOGGER calls is a string literal.

**Reason:** format arguments must always be string literals so they can be
statically checked to match with their argument list.

## `-Wlogger-const`

Checks that `Logger` is always passed as `const Logger *` except in `logger.c`.

**Reason:** no functions should be modifying the logger after creation. We can
store a non-const logger in a struct so we can later pass it to `logger_free`,
but passing it down to other functions must never mutate the logger.

## `-Wlogger-no-escapes`

Checks that no escape sequences are present in the logger format string.

**Reason:** newlines, tabs, or double quotes are not permitted in log outputs
to ensure that each log output is a single line. It's particularly easy to
accidentally add `\n` to the end of a log format. This avoids that problem.

## `-Wmalloc-call`

Checks that allocation functions like `mem_balloc` are always first assigned to
a local variable. The exception is in a return statement, e.g. in simple typed
allocation functions like `logger_new()`. If the allocation is stored in a local
variable, that variable must immediately be checked against `nullptr` before
doing anything else.

Invalid code:

```c
ob->mem = (My_Struct *)mem_alloc(mem, sizeof(My_Struct));
```

Valid code:

```c
My_Struct *tmp = (My_Struct *)mem_alloc(mem, sizeof(My_Struct))
if (tmp == nullptr) {
  return false;
}
ob->mem = tmp;
```

**Reason:** This avoids accidentally putting `nullptr` into a location without
checking first. Putting `nullptr` somewhere may be ok, but we must do it
intentionally.

## `-Wmalloc-type`

Checks that `mem_balloc` is only used for built-in types. For struct allocations
`mem_alloc` and other `calloc`-like functions should be used.

**Reason:** `mem_balloc` does not zero-initialise its memory, which is ok for
byte arrays (at most it can cause incorrect behaviour on most systems), but very
risky for aggregate types containing pointers, which can point at random (or
worse, attacker-controlled) memory.

## `-Wmemcpy-structs`

Checks that `memcpy` and `memset` aren't used for struct pointers.

Exemptions are:

- `IP_Port`
- `IP4`
- `IP6`

**Reason:** structs can contain pointers, so `memset` is risky (it can create
invalid null pointer representations) and `memcpy` should be replaced by an
assignment, possibly in a loop, to avoid messing up the size argument of the
`memcpy` call.

## `-Wmissing-non-null`

Checks that all function declarations have nullability annotations (`non_null`
and/or `nullable`).

**Reason:** in TokTok code, we want to be explicit about which pointer
parameters can be passed a NULL pointer. This forces the developer to think
about nullability and allows static analysers to ensure that all possibly-NULL
pointers are checked before being dereferenced or passed to a non-NULL parameter.

## `-Wnesting`

Warns if a function has more than 7 nesting levels.

**Reason:** deep nesting makes functions more difficult to comprehend.

## `-Wnon-null`

Checks that all pointer parameters are listed in either `non_null` or
`nullable`, and that none of the numbers in these annotations are non-pointers.

**Reason:** see `-Wmissing-non-null` for more context. This check ensures that
nullability annotations are updated when parameter lists change.

## `-Wparens`

Suggests removing parentheses where they are not needed:

- in return expressions, e.g. `return(something);` should be `return something;`.
- in initialisers, e.g. `int foo = (something);` should be `int foo = something;`.
- in assignments, e.g. `foo = (something);` should be `foo = something;`.
- in parentheses, e.g. `((something))` should be `(something)`.

**Reason:** sometimes extra parentheses add clarity, so we don't forbid all
redundant parentheses, but in the above cases, they don't add clarity and only
add more syntax and confusion as to why there are extra parentheses there.

## `-Wswitch-if`

Suggests turning sequences of `if`/`else` statements into `switch`, if there are
at least 3 sequential if-conditions
comparing a variable to a constant.

**Reason:** switch-case statements are clearer in expressing long sequences of
comparisons against constants. They also come with duplication checks in most C
compilers.

## `-Wtype-check` (global)

Performs Hindley-Milner like type checking.

This is very much work in progress, so it may fail in cryptic ways. Talk to
@iphydf if it produces an error.

**Reason:** this allows us to validate various difficult to check aspects of C.

## `-Wtypedef-name`

Checks that typedef names match the struct/union name. E.g.
`typedef struct Foo_ { ... } Foo;` should instead be
`typedef struct Foo { ... } Foo;`.

**Reason:** there is no good reason for them to be different, and it adds
confusion and a potential for C++ code to pick the wrong name and later break
in refactorings.

## `-Wunsafe-func`

Explicitly forbids the use of some C functions considered unsafe:

- `atexit`, because it creates global state that should be avoided.
- `atof`, because it does not perform error checking.
  `strtod` should be used, instead.
- `atoi`, because it does not perform error checking.
  `strtol` should be used, instead.
- `atoll`, because it does not perform error checking.
  `strtoll` should be used, instead.
- `atol`, because it does not perform error checking.
  `strtol` should be used, instead.
- `gets`, because it performs unbounded writes to buffers.
  `fgets` should be used, instead.
- `sprintf`, because it has no way of bounding the number of characters written.
  `snprintf` should be used, instead.
- `strerror`, because it is not thread safe.
  `strerror_r` or `net_new_strerror` should be used, instead.
- `strcat`, because it has no way of bounding the number of characters written.
  `snprintf` should be used, instead.
- `strcpy`, because it has no way of bounding the number of characters written.
  `snprintf` or `strlen` and `memcpy` should be used, instead.
- `strncpy`, because it may not null-terminate the target string.
  `snprintf` or `strlen` and `memcpy` should be used, instead.
- `strdup`, because it is non-portable.
  `mem_balloc` followed by `memcpy` should be used, instead.
- `strtok`, because it is not thread-safe.
- `vsprintf`, because it has no way of bounding the number of characters written.
  `vsnprintf` should be used, instead.

**Reason:** .

## `-Wvar-unused-in-scope`

Suggests reducing the scope of a local variable definition when possible.

E.g.:

```cpp
{
  int a = get_a();
  if (cond) {
    do_something(a);
    do_something_else(a);
  }
}
```

could be written as:

```cpp
{
  if (cond) {
    int a = get_a();
    do_something(a);
    do_something_else(a);
  }
}
```

This can be semantically different if `get_a` has side-effects, so some care
should be taken when applying suggested changes.

**Reason:** having variables declared in their inner-most possible scope makes
it clearer how much code can be influenced by that variable.
