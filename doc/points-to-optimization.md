# Points-To Analysis Optimization Plan

This document outlines potential performance improvements for the fully
context-sensitive points-to analysis. These optimizations aim to reduce analysis
time and memory footprint without compromising the established soundness or
precision requirements.

## 1. Optimize Bulk Memory Operations (`memcpy`/`memmove`)

**Current Status:**

The handling of `memcpy` and `memmove` in `Tokstyle.Analysis.PointsTo.hs` is
currently a major performance bottleneck, especially for large codebases
involving many structures. It performs a full linear scan of the entire abstract
heap (`memMap`) to find locations that are fields of the source object.

```haskell
-- Current O(HeapSize) approach
updatesMap <- foldM (\acc (kInt, v) -> do
    -- Checks if EVERY location in the heap is a field of srcBaseLoc
    ) IntMap.empty (IntMap.toList (memMap facts))
```

**Proposed Solution: Secondary Index for Fields**

Maintain a secondary index that maps a base memory location to all its known
field locations.

- **New State Component:** Add `fieldIndex :: IntMap [IMemLoc]` to
  `MemLocPool` or a similar persistent state. This maps the `IMemLoc` of a
  base location (like a `StackLoc` or `HeapLoc`) to a list of `IMemLoc`s
  representing its `FieldLoc`s.
- **Update on Intern:** Whenever a new `FieldLoc base field` is interned,
  update this index.
- **Optimized `memcpy`:** Instead of scanning `memMap`, look up the source
  base location in `fieldIndex`. This yields exactly the list of locations
  that need to be copied.

**Impact:**

Changes `memcpy` complexity from $O(\text{TotalHeapSize})$ to
$O(\text{SizeOfCopiedStruct})$.

**Status:** Implemented. Benchmarking on the current test suite showed minimal
performance gain (approx. 0.6%), likely due to the small size of the abstract
heap in tests. However, it is kept to prevent quadratic scaling issues as the
codebase grows.

## 2. Adopt "Relevant Context" Sensitivity

**Current Status:**

The analysis uses full call-string sensitivity (`[ScopedId]`). This
distinguishes every unique call path, which is precise but can lead to redundant
analysis. If `func(a)` is called from two different paths but with identical
abstract values for `a`, it is currently analyzed twice.

**Proposed Solution: Memoization on Input State**

Change the memoization key in the `GlobalEnv` from `(FunctionId, CallStack)` to
`(FunctionId, RelevantInputState)`.

- **Relevant Input State:** This is a subset of the `PointsToFact` that is
  actually reachable by the function being called (i.e., facts about its
  parameters and global variables it accesses).
- **Mechanism:** Before analyzing a function, compute its
  `RelevantInputState`. Check if this state has already been analyzed for this
  function. If so, reuse the existing summary.
- **Preserves Precision:** This is still fully context-sensitive in effect, as
  it only merges contexts that are indistinguishable from the perspective of
  the called function.

**Impact:**

Potentially drastic reduction in the number of analysis passes for utility
functions called from many places with similar data.

## 3. Data Structure Optimizations

**Current Status:**

The `PointsToFact` uses `Map ScopedId IntSet` for `varMap`. `Data.Map` is
generally slower than `Data.IntMap`.

**Proposed Solution: Int-based Variable Mapping**

- **Pre-processing:** Assign a unique dense `Int` ID to every `ScopedId` in
  the program during the initialization phase.
- **Implementation:** Replace `Map ScopedId IntSet` with `IntMap IntSet` in
  `PointsToFact`.
- **Benefit:** Faster lookups, insertions, and merges (`join` operations),
  which are the core of the fixpoint algorithm.

**Status:** Rejected. Benchmarking showed that `IntMap` was not significantly
faster than `Map ScopedId` for this use case, likely because `ScopedId`
comparison is already quite fast (just an `Int` comparison). The added
complexity of maintaining a separate mapping was not justified.

## 4. Reduce Memory Footprint of the Cache

**Current Status:**

The `FixpointState` caches the full Control Flow Graph (CFG) and the dataflow
facts for _every node_ of _every analyzed context_ in `fsCFGCache`.

```haskell
fsCFGCache :: Map (ScopedId, Context) ([CFG ScopedId PointsToFact], Map ScopedId Int)
```

For large programs, this results in massive memory usage, as the entire detailed
analysis state of the program is held in RAM simultaneously.

**Proposed Solution: On-Demand Recomputation for Linting**

- **Analysis Phase:** During the global fixpoint phase, _discard_ the
  intra-procedural CFG and node facts once a `FunctionSummary` (exit facts and
  effects) has been computed. Only keep the summaries in memory.
- **Linting Phase:** In `Tokstyle.Linter.PointsTo`, when diagnostics need to
  be reported for a function, re-run the intra-procedural `fixpoint` analysis
  for that specific function using the established global summaries.
- **Trade-off:** Increases CPU time during the final reporting phase but
  significantly reduces peak memory usage, preventing out-of-memory errors on
  large projects.

## 5. Differential Worklist Algorithm

**Current Status:**

When a function summary changes, all its callers are added back to the worklist
and fully re-analyzed from scratch.

**Proposed Solution: Differential Dataflow**

Implement a differential analysis where only the _changes_ (deltas) in a summary
are propagated to callers. This is a more complex algorithmic change but can
avoid redundant re-computation of stable facts.
