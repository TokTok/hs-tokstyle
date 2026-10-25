# C-based linters (`check-c`)

There are currently 15 linters implemented.

## `-Wbool-conversion`

Checks for implicit conversions to bool.

## `-Wborrow-check`

Checks for borrow-checker violations in C code.

This linter implements a Rust-like borrow checker for C. It tracks ownership,
moves, and borrows of pointers marked with `__attribute__((owned))`.

It also makes conservative assumptions about un-annotated functions to ensure
soundness: it assumes the return value of a function borrows from all its
non-owned pointer arguments.

## `-Wcallback-data`

Checks that the context pointer passed to a callback matches the expected type inferred from the callback's first argument.

## `-Wcallback-discipline`

Ensures callback discipline is followed.

Callback and userdata fields should only be mutated within registry
functions (functions that simply assign a parameter to a callback field).
Registry functions themselves must remain simple and side-effect free.

## `-Wcallback-params`

Checks that the parameter names of a callback match its definition.

## `-Wcast`

Checks for disallowed casts between incompatible types.

## `-Wconversion`

Checks for disallowed implicit conversions.

## `-Wmemcpy`

Checks compatibility of dst and src in memcpy/memcmp.

## `-Wmemset`

Checks for memset calls on types that contain pointers.

## `-Wmemzero`

Checks that a local wiped with `crypto_memzero` on some paths through a
function is wiped on every path that returns after writing it.

**Reason:** a wipe says the local holds a secret, such as a key or a
password. An early return that skips the wipe, often on an error path,
leaves that secret on the stack.

## `-Wsize-arg`

Checks that the size argument passed to a function matches the array size of the preceding argument.

## `-Wsizeof`

Checks for `sizeof(buf)` where `buf` is a pointer instead of an array.

## `-Wstrict-typedef`

Implements strict typedef checking.

This linter ensures that logically distinct types (represented by different
typedefs) are not accidentally mixed, even if they have the same underlying
representation. This is particularly useful for identifiers, indices, and
role-based types.

## `-Wvoid-call`

Checks that the first statement of a function with a `void*` parameter casts it to a specific type.

## `-Wwraparound`

Checks for integer arithmetic that wraps around where it is used as an index:
compared to an index reduced with `%` or `&`, reduced with `%` by a size that
doesn't divide the type's range, or used to index an array it can be past the
end of. Also checks for the difference of two 16-bit values that wraps around
at 32 bits because integer promotion computes it in `int`.

The checks follow values through locals and fields, take the conditions that
guard them into account, and know which values the program stores in struct
fields, returns from functions, and passes to static functions.

**Reason:** in a ring buffer of size `N`, `(uint16_t)(start - 1)` wraps to
65535 when `start` is 0, not to `N - 1`. Comparing it to `id % N` then never
matches. Use `(start + N - 1) % N` instead.
