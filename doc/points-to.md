# Points-To Analysis Design Document

## 1. Overview & Goal

This document outlines the design for a static points-to analysis for the C
library codebase. The primary goal is to build a high-precision analysis that is
**sound** and capable of determining the precise set of possible memory
locations that every pointer variable can point to at any given program point.

The analysis will be **inter-procedural**, **field-sensitive**, and **fully
context-sensitive**. It will serve as a core component for subsequent, more
advanced static analyses, such as taint analysis or null-pointer dereference
detection. The emphasis is on achieving maximum precision, even at the cost of
increased analysis time, to eliminate false positives wherever possible.

The implementation will be in Haskell, leveraging the existing `hs-cimple` AST
and the `hs-tokstyle` analysis frameworks.

## 2. Core Requirements & Constraints

This section details the fundamental properties and limitations of the analysis.

### 2.1. Soundness (No False Negatives)

**Requirement:** The analysis must be _sound_. This means it must compute a
conservative over-approximation of the points-to sets. For any given pointer,
the analysis must identify _all_ memory locations it could possibly point to
during any execution of the program. It is acceptable for the analysis to report
that a pointer _might_ point to a location when it actually doesn't (a false
positive), but it is **unacceptable** for it to miss a potential location (a
false negative).

**Example:** `c int a, b; int *p = &a; if (condition) { p = &b; } // At this
point, a sound analysis MUST report that 'p' can point to {a, b}. // An unsound
analysis might only report {a} or {b}.`

### 2.2. Field-Sensitivity

**Requirement:** The analysis must distinguish between the different fields of a
`struct`. Pointers to different fields of the same struct instance will be
tracked separately. This will be achieved by leveraging the `TypeSystem` module
to get struct definitions.

**Example:** ```c struct S { int x; int y; }; struct S s; int *p = &s.x; int
*q = &s.y;

// A field-sensitive analysis will know that 'p' points to the 'x' field of 's'
// and 'q' points to the 'y' field of 's'. It will not confuse them. ```

### 2.3. Inter-Procedural Analysis

**Requirement:** The analysis must track pointers across function call
boundaries. This is essential for completeness in any non-trivial program.

**Example:** ```c int* allocate() { int* heap_var = (int\*)mem_alloc(...); return
heap_var; }

void use() { int\* p = allocate(); // An inter-procedural analysis is required to
know that 'p' points to the // heap location allocated inside 'allocate'. } ```

### 2.4. Full Context-Sensitivity

**Requirement:** The analysis will be _fully context-sensitive_. This means that
for any given function, a separate analysis will be performed for each unique
call path (or "context") leading to that function. This approach maximizes
precision by avoiding the merging of information from different call sites.

**Rationale:** The primary goal is precision. While this approach is
computationally more expensive than a context-insensitive one, it is feasible
for this specific codebase and will provide more accurate results, significantly
reducing false positives. This high-precision foundation is critical for the
effectiveness of subsequent analyses.

**Example:** ```c void set_ptr(int \**pp, int *v) { \*pp = v; }

void main() { int a, b; int *p1, *p2; set_ptr(&p1, &a); // Call site 1
set_ptr(&p2, &b); // Call site 2 }

// A fully context-sensitive analysis will analyze 'set_ptr' twice, once for //
each call site. // 1. In the context of call site 1, it knows 'pp' points to
'p1' and 'v' // points to 'a'. It concludes that 'p1' now points to 'a'. // 2.
In the context of call site 2, it knows 'pp' points to 'p2' and 'v' // points to
'b'. It concludes that 'p2' now points to 'b'. // The final result is precise:
p1 points to {a} and p2 points to {b}. An // insensitive analysis would have
conservatively concluded both could point to // {a, b}. ```

**Constraint: Handling Recursion:** A fully context-sensitive analysis cannot
naively handle recursion, as it would lead to infinite call chains. The analysis
will therefore incorporate a recursion detector. If a function is called while
it is already on the current call stack, the analysis will halt and emit a
diagnostic pointing to the recursive call. The codebase will then be refactored
to eliminate the recursion, as it is considered an undesirable pattern for this
library.

### 2.5. Simplifying Assumptions (Enforced Externally)

To achieve high precision in a feasible manner, the points-to analysis operates
under a set of strong, simplifying assumptions about the codebase. These
assumptions deviate from the full semantics of the C language but are considered
valid in this project's context because they are enforced by a separate suite of
linters that run prior to this analysis.

1.  **Strict Type Safety:** The analysis assumes the code is completely
    type-safe and free of undefined behavior related to memory and types. This
    specifically precludes:
    - **Unsafe Pointer Casts:** Type punning via pointer casts (e.g., casting
      a `struct S*` to a `char*` to perform manual byte-offset arithmetic) is
      assumed not to occur. The analysis therefore models casts on pointers as
      no-ops, trusting that they do not fundamentally change the memory being
      referenced in a way that would be invisible to the type system.
    - **Union Type Punning:** Accessing a `union` member other than the one
      that was last written is assumed not to happen.
    - **Out-of-Bounds Access:** All array and pointer accesses are assumed to
      be within their legal bounds.

2.  **Single-Threaded Execution:** The analysis models the program as a purely
    sequential execution. It does not account for any concurrency. This means:
    - The codebase is assumed not to use threading libraries (e.g., pthreads).
    - The analysis does not model or reason about data races, mutexes, locks,
      or atomic operations.

3.  **Benign Unsummarized External Functions:** For any external library
    function that does not have a specific manual summary, the analysis makes a
    critical assumption about its behavior: - The function is assumed to have **no side effects on pointer values
    passed to it**. It may read from or write to the _data_ at the memory
    location a pointer refers to, but it will not change the pointer itself
    to make it point to a different location. - For example, a call to an unsummarized function `void process_data(int*
p)` is assumed not to modify the value of `p`. In contrast, a function
    like `getline(&buffer, ...)` which allocates memory and modifies the
    `buffer` pointer _must_ be manually summarized, as it violates this
    assumption.

4.  **No Standard Program Termination:** The analysis assumes that functions
    within the library do not call standard program termination functions like
    `exit`, `abort`, or `err`. The only exception is for assertion failures
    (e.g., `assert(0)`), which are considered to halt the program in a way that
    is outside the scope of this analysis. Therefore, all non-asserting
    functions are assumed to eventually return control to their caller.

5.  **Strict Field Access for Pointers in Unions:** When dealing with `union`
    types, the analysis assumes a strict and type-safe access pattern for any
    members that are pointers. Specifically, if a pointer value is written to a
    particular field of a union, any subsequent read of a pointer from that
    _exact same field_. The code is assumed not to perform type punning on
    pointers by writing to one union field and reading from another.

These assumptions are fundamental to the soundness of the analysis within its
intended operational context. Violations of these assumptions must be caught by
other static analysis tools before the points-to analysis is run.

## 3. Memory Model

To reason about memory, we will abstract concrete memory locations into a set of
symbolic identifiers. The `MemLoc` data type will represent these abstract
locations. Crucially, it will use the `ScopedId` type from
`Tokstyle.Analysis.Scope` to uniquely identify variables, thus correctly
handling shadowing.

```haskell
-- Defined in Tokstyle.Analysis.PointsTo.Types
data MemLoc
  = StackLoc { locId :: ScopedId }
  | HeapLoc { allocSite :: Text } -- "filepath:line:col"
  | GlobalVarLoc { locId :: ScopedId }
  | ExternalParamLoc { funcName :: Text, paramName :: Text }
  | FieldLoc { baseLoc :: MemLoc, fieldName :: Text }
  | NullLoc
  | UnknownLoc
  deriving (Eq, Ord, Show, Generic)
```

- **`StackLoc`**: Represents a local variable on the stack, uniquely
  identified by its `ScopedId`.
- **`HeapLoc`**: Represents a block of memory allocated on the heap,
  identified by the source code location of the allocation call.
- **`GlobalVarLoc`**: Represents a global or static variable, uniquely
  identified by its `ScopedId`.
- **`ExternalParamLoc`**: Represents an abstract memory location for a pointer
  passed as a parameter to a public API function whose body is not analyzed.
- **`FieldLoc`**: Represents a field within a struct. The `baseLoc` will be
  another `MemLoc`.
- **`NullLoc`**: Represents the `NULL` pointer.
- **`UnknownLoc`**: A conservative catch-all for situations the analysis
  cannot fully resolve (e.g., pointers returned from un-summarized external
  functions). Its use is minimized in a context-sensitive analysis.

### 3.1. Interning for Performance

To improve the performance of the analysis, especially when dealing with deeply
recursive `FieldLoc` structures, `MemLoc` values are **interned**.

- **`IMemLoc`**: A newtype around `Int`, representing a unique ID for a
  `MemLoc`.
- **`MemLocPool`**: A data structure that maintains the bidirectional mapping
  between `MemLoc` and `IMemLoc`.

The analysis primarily operates on `IMemLoc` values, using efficient `IntMap`
and `IntSet` data structures for storing points-to facts.

### 3.2. Heap Allocation Model

The `HeapLoc` is defined by its allocation site (e.g., the source location of a
call to `malloc`). This is a standard technique known as an **allocation-site
abstraction**.

A key consequence of this model is that all heap objects allocated at the exact
same source code location are represented by a single, abstract `HeapLoc`. This
is particularly relevant for allocations inside loops. For example, if a
`malloc` call inside a `for` loop creates 100 distinct objects on the heap, our
analysis will model them as a single abstract `HeapLoc`.

This is a deliberate design trade-off:

- **It is sound:** It correctly over-approximates the real behavior. If a
  pointer can point to any of the 100 real objects, having it point to the one
  abstract object is a safe conclusion.
- **It is simple and efficient:** It keeps the number of abstract heap
  locations bounded and manageable.

The primary limitation is a loss of precision when reasoning about complex,
iteratively-built data structures like linked lists or trees, as the analysis
cannot distinguish between the different nodes if they were allocated at the
same location. For this codebase, this trade-off is considered acceptable.

### 3.3. Handling `UnknownLoc` for Soundness

In a fully context-sensitive analysis, the role of `UnknownLoc` is significantly
reduced. It is no longer used to merge information from different call paths.
Instead, it serves as a "poison" value for true sources of uncertainty that the
analysis cannot see into, primarily:

1.  **External Code:** Pointers returned from external library functions for
    which we do not have a precise summary.
2.  **Unsafe Operations:** Pointers derived from unsafe casts or complex pointer
    arithmetic that the analysis does not model.
3.  **Unresolvable Function Pointers:** Calls made via a function pointer that
    the analysis cannot resolve to a finite set of concrete functions.

When a pointer's points-to set becomes `{UnknownLoc}`, this uncertainty is
propagated to ensure soundness:

- **Assignment (`q = p`):** If `PointsTo(p)` contains `UnknownLoc`, then
  `PointsTo(q)` will also contain `UnknownLoc`.
- **Store (`*p = q`):** If `PointsTo(p)` contains `UnknownLoc`, it means the
  store could modify _any_ memory location. To model this, the analysis will
  conservatively assume that the pointers in `PointsTo(q)` have "escaped" and
  may be accessible from other unknown pointers.
- **Load (`q = *p`):** If `PointsTo(p)` contains `UnknownLoc`, `q` could be
  pointing to anything that has previously escaped.

This infectious nature of `UnknownLoc` ensures that even if precision is lost at
the boundaries of the analysis, the overall result remains sound.

**Status:** The `MemLoc` data type and interning mechanism have been implemented
in `Tokstyle.Analysis.PointsTo.Types`.

## 4. Analysis Architecture

The analysis will proceed in two stages: a preprocessing stage to gather
necessary information, followed by the main iterative, context-sensitive
analysis.

### 4.1. Preprocessing Stage

Before the main analysis, the following steps will be performed on the entire
codebase:

1.  **Scope Resolution:** Run the `Tokstyle.Analysis.Scope.runScopePass` to
    transform the AST, replacing all variable names with unique `ScopedId`s. The
    rest of the analysis will operate on this scope-resolved AST.
2.  **Type Definition Collection:** Run `Tokstyle.Common.TypeSystem.collect` to
    build a comprehensive `TypeSystem` map. This will provide the analysis with
    all struct, union, and enum definitions needed for field-sensitive analysis.
3.  **V-Table Resolution:** A custom traversal pass has been implemented to find
    all constant-initialized global variables corresponding to function pointer
    v-tables (e.g., `os_memory_funcs`). This pass uses heuristics to identify
    them, such as searching for global variables whose type is a struct
    containing only function pointers, and which are initialized with a constant
    aggregate of function designators. The results are stored in an immutable
    map for the main analysis to use. See
    `hs-tokstyle/src/Tokstyle/Analysis/VTable.hs` for the implementation.

    **Status:** Implemented and tested.

### 4.2. Main Analysis: Global Fixpoint Driver

This component, implemented as a new global linter, is responsible for achieving
a whole-program fixpoint in a fully context-sensitive manner.

1.  **Data Structures:**
    - **`Context`**: A call stack, represented as a list of `ScopedId`s,
      identifying the unique call path to a function.
    - **`FunctionSummary`**: Captures the effect of a function _in a specific
      context_. For this analysis, it primarily stores the points-to sets of
      the function's return value.
    - **`GlobalEnv`**: A map from a `(ScopedId, Context)` pair to the
      `FunctionSummary` for that specific analysis instance.
    - **`CallGraph`**: A map representing the program's call graph, which is
      discovered dynamically as the analysis progresses.

2.  **Algorithm:** a. **Initialization:** i. Build an initial `CallGraph` with
    an empty points-to map. This graph will contain all direct calls. ii.
    Identify the entry point of the analysis (e.g., the `main` function or all
    public API functions). iii. Initialize a `GlobalEnv` as an empty map. iv.
    Initialize a worklist (using `Tokstyle.Worklist.hs`) containing the entry
    point(s), each paired with an empty `Context`. For example, `(main_func_id,
[])`. b. **Fixpoint Iteration:** i. While the worklist is not empty, pop an
    item `(f, context)`. ii. **Recursion Check:** Before analyzing, check if `f`
    is already present in `context`. If it is, a recursive cycle has been
    detected. Halt the analysis and emit a diagnostic error detailing the call
    stack (`context ++ [f]`). iii. Run the intra-procedural analysis on `f`,
    providing it with the current `GlobalEnv`, the current `context`, and other
    pre-processed data. The analysis of `f` will use summaries from the
    `GlobalEnv` for callees, keyed by `(callee_id, new_context)`. iv. **Discover
    New Call Edges:** During the analysis of `f`, if a call to a function `g` is
    encountered, a new worklist item `(g, context ++ [f])` is created and added
    to the worklist. New edges are also added to the `CallGraph`. v. **Compute
    New Summary:** Compute a new `FunctionSummary` for `(f, context)`. vi.
    **Check for Changes:** If this new summary is different from the one
    currently in `GlobalEnv` for `(f, context)`, update the environment and add
    all _callers_ of `f` (found via the `CallGraph`) back to the worklist, each
    with their respective contexts. c. **Termination:** The process terminates
    when the worklist is empty, which signifies that the `GlobalEnv` has

3.  stabilized.

**Status:** Implemented. The global fixpoint driver in
`hs-tokstyle/src/Tokstyle/Analysis/PointsTo/Fixpoint.hs` has been rewritten to
be fully context-sensitive.

### 4.3. Intra-Procedural Analysis

This component will be an instance of the `DataFlow` typeclass from
`DataFlow.hs`.

1.  **Dataflow Fact:** A `PointsToFact` structure containing:
    - `varMap`: A map from `ScopedId` to `IntSet` (representing a set of
      `IMemLoc`s).
    - `memMap`: An `IntMap` from `IMemLoc` to `IntSet` (representing
      heap/global memory).
    - `unknownWrites`: An `IntSet` representing locations that might have been
      written to by an unknown pointer.

2.  **Monadic Analysis:** The analysis runs in the `PointsToAnalysis` monad,
    which is an alias for `State MemLocPool`. This allows it to intern new
    `MemLoc`s on the fly as they are discovered (e.g., new `FieldLoc`s).

3.  **`DataFlow` Instance:**
    - **`join`:** Merges two `PointsToFact`s by taking the union of their
      respective maps and sets.
    - **`transfer`:** Models the effect of each C statement. The key change
      for context-sensitivity is in how it handles function calls:
      - When a call to function `g` is encountered within function `f`
        (being analyzed in `context`), the `transfer` function will not
        immediately analyze `g`.
      - Instead, it will look up the summary for `(g, context ++ [f])` in
        the current `GlobalEnv`.
      - If a summary exists, it applies its effects (e.g., modeling the
        return value).
      - If no summary exists, it assumes a conservative default (e.g.,
        return value is `UnknownLoc`) and adds `(g, context ++ [f])` to the
        global worklist so it will be analyzed later.

**Status:** Implemented. The `DataFlow` instance in `Tokstyle.Analysis.PointsTo`
has been updated to support context-propagation and uses interning for
performance.

## 5. Handling Specific C Features

- **Function Pointers (V-Tables):** Treated as constants, resolved during the
  preprocessing stage.
- **Memory Allocation Wrappers:** The codebase uses a v-table pattern for all
  memory allocation (e.g., `mem_alloc`, `mem_valloc`), dispatching through a
  `Memory` object. The analysis will handle this by:
  1.  Using the V-Table Resolution pass to determine that the function
      pointers in the global `os_memory_funcs` object point to the
      corresponding `sys_*` functions (e.g., `sys_calloc`).
  2.  Using the fully context-sensitive, inter-procedural, and field-sensitive
      analysis to precisely track the pointer to the `Memory` object itself
      (e.g., the return from `os_memory()`) as it is passed through the call
      chain.
  3.  Resolving indirect calls like `mem->funcs->calloc` to a direct call to
      the appropriate `sys_*` function based on the precise points-to set of
      `mem` in the current context.
  4.  Applying the manual summary for the underlying external call (e.g., to
      `calloc` within `sys_calloc`), which will model the allocation by
      returning a new, unique `HeapLoc`.

  **Status:** Implemented. The main analysis now leverages the V-Table
  resolution pass in a context-sensitive manner.

- **Type System:** The analysis will assume the code is **type-safe** and will
  not model unsafe casts or `union` type punning.

- **External Function Calls:** Calls to functions whose source is not
  available (e.g., from libc) will be handled with a hybrid approach:
  1.  **Manual Summaries:** A pre-defined library of summaries for common,
      well-behaved functions will be used. These summaries are critical for
      maintaining precision at the boundary with external code.
      - For functions like `malloc`, the summary will indicate that it
        returns a pointer to a new, unique `HeapLoc`.
      - For functions like `printf` or `strcpy`, the summary will model
        their known behavior (e.g., `printf` has no effect on points-to
        sets, `strcpy` copies character data but not pointers).
      - For memory copying functions like `memcpy` and `memmove`, the
        analysis will rely on a key simplifying assumption: **the source and
        destination arguments are always pointers to the same type**. This
        allows the summary to model the operation as a precise,
        field-sensitive copy. The analysis will iterate through all
        pointer-typed fields of the destination's type and update their
        points-to sets with the sets from the corresponding fields in the
        source. This avoids the need for complex reasoning about the `size`
        parameter and prevents a significant loss of precision. Initially,
        these summaries will be implemented directly in a dedicated Haskell
        module (e.g., `Tokstyle.Analysis.PointsTo.ExternalSummaries`).

      **Status:** Implemented in
      `hs-tokstyle/src/Tokstyle/Analysis/PointsTo/ExternalSummaries.hs`.

  2.  **Conservative Fallback:** For any external function _not_ in our
      pre-defined library, a conservative rule will be applied to ensure
      soundness. Since internal calls are handled with full precision, this
      fallback is now reserved for true external boundaries:
      - **Return Value:** If the function returns a pointer, its points-to
        set is considered `{UnknownLoc}`.
      - **Pointer Arguments:** Any pointer passed as an argument is assumed
        to be potentially modified. Its contents could now point to anything
        that has previously "escaped" to an unknown location.

- **Preprocessor Conditionals (`#ifdef`):** To ensure soundness across all
  build configurations, the analysis will explore all conditional paths and
  merge the results.
  - **Global Scope:** If a function has multiple definitions under different
    `#ifdef` blocks, the analysis will compute a `FunctionSummary` for each
    definition and then merge them into a single, conservative summary for
    that function name.
  - **Intra-Procedural:** Inside a function, an `#ifdef`/`#else` block is
    treated as a standard control-flow branch, like an `if` statement. The
    `buildCFG` function will create separate paths for each branch, and the
    `join` operator of the dataflow analysis will automatically merge the
    points-to information at the convergence point.

- **Non-Local Control Flow (`setjmp`/`longjmp`):** This analysis assumes that
  the code does not contain calls to `setjmp` or `longjmp`. These functions
  are considered out of scope, as their usage is disallowed by a separate,
  preceding analysis.

## 6. Applications of the Analysis

The primary output of the points-to analysis is a detailed map of what each
pointer can point to. This information is consumed by other tools and linters to
enforce higher-level invariants.

### 6.1. The `points-to-asserts` Linter

One of the key consumers of the points-to analysis is a linter named
`points-to-asserts`. This linter allows developers to write assertions directly
in their C code that are checked at compile time using the results of the static
analysis. This provides a powerful way to enforce invariants about pointer
properties.

The linter recognizes calls to a function named `assert()` where the argument is
a combination of specific predicate functions. If the points-to analysis shows
that a pointer can point to a location that violates the assertion, a
compile-time error is generated.

#### Supported Predicates

The following predicate functions are supported inside an `assert()`:

- `mem_is_heap(p)`: Asserts that the pointer `p` must point to a memory
  location allocated on the heap (i.e., a `HeapLoc`).
- `mem_is_stack(p)`: Asserts that `p` must point to a local variable on the
  stack (i.e., a `StackLoc`).
- `mem_is_not_null(p)`: Asserts that `p` cannot be `NULL` (i.e., does not
  point to `NullLoc`).
- `mem_is_external_param(p)`: Asserts that `p` must point to an abstract
  memory location representing a pointer passed as a parameter to a public API
  function (i.e., an `ExternalParamLoc`).

#### Combining and Negating Predicates

These predicates can be combined using the standard C logical operators `&&`
(AND) and `||` (OR). They can also be negated using `!` (NOT). This allows for
expressing complex invariants.

**Example:**

```c
void process_data(int* data, int cond) {
  // Assert that 'data' is not null.
  assert(mem_is_not_null(data));

  int* local_ptr;
  if (cond) {
    local_ptr = data;
    // Assert that 'local_ptr' is now an external parameter.
    assert(mem_is_external_param(local_ptr));
  } else {
    local_ptr = (int*)malloc(sizeof(int));
    // Assert that 'local_ptr' is either from the heap or an external parameter,
    // but definitely not null.
    assert((mem_is_heap(local_ptr) || mem_is_external_param(local_ptr)) && mem_is_not_null(local_ptr));
  }

  int stack_var;
  int* p = &stack_var;
  // Assert that 'p' is NOT a heap pointer.
  assert(!mem_is_heap(p));
}
```

## 7. Reading List for Implementers

To effectively implement this analysis, a good understanding of the existing
infrastructure is required. The following files are essential reading:

### Core Frameworks & Data Structures

- **`hs-cimple/src/Language/Cimple/Ast.hs`**: Defines the core Abstract Syntax
  Tree (`NodeF`) that the analysis will traverse.
- **`hs-tokstyle/src/Tokstyle/Analysis/CFG.hs`**: Provides the logic for
  building the Control Flow Graph (CFG) from the C AST. This is the structural
  foundation for the dataflow analysis.
- **`hs-tokstyle/src/Tokstyle/Analysis/DataFlow.hs`**: Provides the generic
  intra-procedural dataflow framework, including the `DataFlow` typeclass and
  the `fixpoint` solver. The points-to analysis will be an instance of this
  class, and the solver will be used for each function analysis.
- **`hs-tokstyle/src/Tokstyle/Worklist.hs`**: Provides the worklist data
  structure for the global, context-sensitive fixpoint algorithm.
- **`hs-tokstyle/src/Tokstyle/Linter.hs`**: The central hub for registering
  and running linters. The new analysis will be registered here as a
  `GlobalLinter`.
- **`c-toxcore/toxcore/mem.h`**: Defines the `Memory` and `Memory_Funcs`
  structs that abstract memory management.
- **`c-toxcore/toxcore/mem.c`**: Implements the memory allocation wrappers
  (e.g., `mem_alloc`) and the default `os_memory` implementation.

### Key Analysis Modules to Leverage

- **`hs-tokstyle/src/Tokstyle/Analysis/Scope.hs`**: Crucial for resolving
  variable names to unique IDs (`ScopedId`) before the analysis runs.
- **`hs-tokstyle/src/Tokstyle/Common/TypeSystem.hs`**: Provides the `collect`
  function to get all struct and union definitions, which is essential for
  field-sensitivity.
- **`hs-tokstyle/src/Tokstyle/Analysis/VTable.hs`**: Implements the v-table
  resolution pass, which identifies and resolves function pointers in global
  v-table structs.
- **`hs-tokstyle/src/Tokstyle/Analysis/PointsTo/Fixpoint.hs`**: The location
  of the global fixpoint driver, which will be rewritten to be fully
  context-sensitive.

### AST Traversal & Manipulation

- **`hs-cimple/src/Language/Cimple/TraverseAst.hs`**: Defines the
  `traverseAst` function, the standard way to walk the AST for analysis.
- **`hs-cimple/src/Language/Cimple/MapAst.hs`**: Defines `mapAst` for
  transformations, used by the `Scope` pass.

### Example Implementations

- **`hs-tokstyle/test/Tokstyle/Analysis/VTableSpec.hs`**: Provides a clear
  example of how to set up and use the v-table resolution analysis.
- **`hs-tokstyle/test/Tokstyle/Analysis/PointsToSpec.hs`**: Essential for
  understanding how to write tests for the points-to analysis, showing how to
  construct minimal C examples and assert the final points-to state for the
  high-precision, context-sensitive analysis.

**Note on Testing:** The test suite for `hs-tokstyle`, including the points-to
analysis tests, can be run from the workspace root using the command `bazel test
//hs-tokstyle:testsuite`. To run a specific test or a group of tests, you can
use the `--test_arg` flag to pass the `--match` option to the underlying test
runner. For example, to run a single test, you would use a command like this:
`bazel test //hs-tokstyle:testsuite --test_arg=--match
--test_arg="/Tokstyle.Analysis.PointsTo/Points-To Analysis/should handle simple
pointer assignments/"`

## 8. Alternative Considered: Constraint-Based Analysis

An alternative architecture for achieving a fully context-sensitive analysis is
a **constraint-based** approach, often referred to as an inclusion-based
analysis (e.g., Andersen's style). This approach was considered and is
documented here as a potential future direction.

Instead of the operational, worklist-driven abstract interpretation described in
Section 4, a constraint-based analysis works in two distinct phases:

1.  **Constraint Generation:** First, the entire codebase is traversed once to
    generate a global system of symbolic inclusion constraints that represent
    all data flow in the program.
2.  **Constraint Solving:** Second, this system of constraints is solved
    iteratively until a fixpoint is reached, and no new inclusions can be added
    to any points-to set.

### How It Works

The core idea is to represent the `points-to` relationship as a set of
variables. For a context-insensitive analysis, the constraints would look like
this:

- `p = &a;` generates `loc(a) ⊆ points-to(p)`
- `p = q;` generates `points-to(q) ⊆ points-to(p)`
- `*p = q;` generates a complex constraint: `∀ l ∈ points-to(p), points-to(q)
⊆ points-to(l)`
- `q = *p;` generates `∀ l ∈ points-to(p), points-to(l) ⊆ points-to(q)`

To achieve **full context-sensitivity**, the symbolic variables themselves are
indexed by a context `C` (the call stack). The constraints are then generated
with this context information:

- An assignment `p = q;` occurring inside a function `f` being analyzed in
  context `C` generates the constraint: `points-to(q, C) ⊆ points-to(p, C)`.
- A function call `r = g(arg);` from within function `f` (in context `C`)
  generates constraints that link the caller to the callee. A new context
  `C' = C ++ [f]` is created for the callee, and constraints like the
  following are generated:
  - `points-to(arg, C) ⊆ points-to(param_of_g, C')` (for argument passing)
  - `return_value_of(g, C') ⊆ points-to(r, C)` (for the return value)

The solver then works on this global set of context-indexed constraints until
all `points-to(var, C)` sets stabilize.

### Comparison and Trade-offs

#### Pros:

- **Separation of Concerns:** The logic for generating constraints
  (understanding the C language semantics) is cleanly separated from the logic
  for solving them. This can make the implementation of each part simpler in
  isolation.
- **Potential for Optimized Solvers:** The constraint-solving phase is a
  well-understood problem. It's possible to use or build highly optimized
  graph-based solvers (e.g., using a graph library to represent the inclusion
  relationships) that might be more performant than a worklist-based approach
  for certain program structures.
- **Declarative Nature:** The constraint system is a declarative
  representation of the program's data flow, which can be easier to reason
  about formally.

#### Cons:

- **Implementation Complexity:** While the phases are separate, the overall
  implementation can be more complex. The constraint generation phase requires
  building a complete model of the program's data flow upfront, and the solver
  needs to be carefully designed to handle the context-indexed variables
  efficiently.
- **Debugging:** Debugging can be more difficult. If a pointer incorrectly
  points to a location, the reason is an emergent property of the entire
  global constraint system, not a direct step in an operational analysis.
  Tracing the specific chain of inclusions that led to the incorrect result
  can be challenging. In contrast, the worklist approach has an explicit call
  stack for every analysis step, making it easier to see "why" a decision was
  made.
- **Recursion Detection:** Detecting and reporting recursion requires finding
  cycles in the constraint graph, which is a more complex graph-theory problem
  than simply checking for an element in a list (the call stack). Providing a
  clear, user-friendly diagnostic with the exact recursive call chain is less
  straightforward.

For the current implementation, the worklist-based abstract interpretation
approach was chosen because it is a more direct and operational model, it makes
debugging and recursion detection significantly simpler, and it integrates
naturally with the existing intra-procedural dataflow framework. However, a
constraint-based approach remains a valid and powerful alternative for future
consideration.
