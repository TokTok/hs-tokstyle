{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Analysis.PointsToSpec where

import qualified Data.Map.Strict                        as Map
import qualified Data.Set                               as Set
import           Debug.Trace                            (traceShow)
import           Test.Hspec
import           Tokstyle.Analysis.CallGraph            (buildCallGraph,
                                                         getCallees)
import           Tokstyle.Analysis.PointsTo
import           Tokstyle.Analysis.SecurityRank.Lattice
import           Tokstyle.Analysis.Types
import           Tokstyle.LinterSpec                    (mustParse,
                                                         mustParseStmt)

spec :: Spec
spec = do
    describe "Intra-procedural PointsTo Transfer Function" $ do
        let emptyCallGraph = buildCallGraph [] Map.empty
        let ctx = PointsToContext emptyCallGraph Map.empty Map.empty Map.empty Map.empty Map.empty [] Map.empty Map.empty Set.empty Map.empty
        let pLoc = VarLocation "p"
        let qLoc = VarLocation "q"
        let xLoc = VarLocation "x"
        let yLoc = VarLocation "y"

        it "handles address-of assignment" $ do
            stmt <- mustParseStmt ["p = &x;"]
            let (finalMap, _) = analyzeStatementForPointers Map.empty ctx "f" Map.empty stmt
            finalMap `shouldBe` Map.singleton pLoc (Set.singleton xLoc)

        it "handles pointer assignment" $ do
            stmt <- mustParseStmt ["q = p;"]
            let stateBefore = Map.singleton pLoc (Set.singleton xLoc)
            let (finalMap, _) = analyzeStatementForPointers Map.empty ctx "f" stateBefore stmt
            finalMap `shouldBe` Map.fromList [(pLoc, Set.singleton xLoc), (qLoc, Set.singleton xLoc)]

        it "handles assignment to a dereferenced pointer" $ do
            stmt <- mustParseStmt ["*p = &y;"]
            let stateBefore = Map.singleton pLoc (Set.singleton xLoc)
            let (finalMap, _) = analyzeStatementForPointers Map.empty ctx "f" stateBefore stmt
            -- The map for `p` is unchanged, but now `x` (what `p` points to)
            -- is treated like a pointer pointing to `y`.
            let expected = Map.fromList [(pLoc, Set.singleton xLoc), (xLoc, Set.singleton yLoc)]
            finalMap `shouldBe` expected

        it "handles assignment from a function call" $ do
            -- Create a context with a pre-defined summary for the called function.
            funcDef <- head <$> mustParse ["int* get_ptr() { int* return_val; return return_val; }"]
            let funcDefs = Map.singleton "get_ptr" funcDef
            let summaryData = PointsToSummaryData { returnPointsTo = Set.singleton (VarLocation "return_val")
                                                  , outputPointsTo = Map.empty }
            let summary = Map.singleton [] summaryData
            let summaries = Map.singleton "get_ptr" summary
            let ctxWithSummary = ctx { ptcSummaries = summaries, ptcFuncDefs = funcDefs, ptcCurrentContext = [], ptcDynamicCallGraph = Map.empty, ptcAnalyzedCfgs = Map.empty, ptcFileMacros = Map.empty }
            stmt <- mustParseStmt ["p = get_ptr();"]
            let (finalMap, _) = analyzeStatementForPointers Map.empty ctxWithSummary "f" Map.empty stmt
            -- `p` should now point to the abstract location returned by the summary.
            finalMap `shouldBe` Map.singleton pLoc (Set.singleton (GlobalVarLocation "return_val"))

        it "handles struct member assignment" $ do
            stmt <- mustParseStmt ["s.field = &x;"]
            let (finalMap, _) = analyzeStatementForPointers Map.empty ctx "f" Map.empty stmt
            let sFieldLoc = FieldLocation (VarLocation "s") "field"
            finalMap `shouldBe` Map.singleton sFieldLoc (Set.singleton xLoc)

        it "handles struct pointer assignment" $ do
            stmt <- mustParseStmt ["p->field = &y;"]
            let stateBefore = Map.singleton pLoc (Set.singleton xLoc) -- p points to x
            let (finalMap, _) = analyzeStatementForPointers Map.empty ctx "f" stateBefore stmt
            let xFieldLoc = FieldLocation xLoc "field"
            -- The analysis should conclude that x.field now points to y
            let expected = Map.union stateBefore (Map.singleton xFieldLoc (Set.singleton yLoc))
            finalMap `shouldBe` expected

        it "handles assignment of a function address to a function pointer" $ do
            stmt <- mustParseStmt ["fp = &my_func;"]
            let (finalMap, _) = analyzeStatementForPointers Map.empty ctx "f" Map.empty stmt
            let fpLoc = VarLocation "fp"
            let myFuncLoc = VarLocation "my_func" -- Functions are treated as locations
            finalMap `shouldBe` Map.singleton fpLoc (Set.singleton myFuncLoc)

        it "handles taking the address of an array element" $ do
            stmt <- mustParseStmt ["p = &a[2];"]
            let (finalMap, _) = analyzeStatementForPointers Map.empty ctx "f" Map.empty stmt
            let aElemLoc = FieldLocation (VarLocation "a") "_index_2"
            finalMap `shouldBe` Map.singleton pLoc (Set.singleton aElemLoc)

        it "handles heap allocation with malloc" $ do
            stmt <- mustParseStmt ["p = malloc(sizeof(int));"]
            let (finalMap, _) = analyzeStatementForPointers Map.empty ctx "f" Map.empty stmt
            -- The test assumes malloc is recognized and assigned a unique heap location.
            -- This will require extending the analysis.
            case Map.lookup pLoc finalMap of
                Just pointsToSet ->
                    case Set.toList pointsToSet of
                        [HeapLocation n] -> n `shouldNotBe` 0
                        other -> expectationFailure $ "p should point to a single heap location, but got " ++ show other
                Nothing -> expectationFailure "p not found in points-to map"

        it "handles pointer arithmetic" $ do
            stmt <- mustParseStmt ["q = p + 1;"]
            let aLoc = VarLocation "a"
            let stateBefore = Map.singleton pLoc (Set.singleton aLoc)
            let (finalMap, _) = analyzeStatementForPointers Map.empty ctx "f" stateBefore stmt
            finalMap `shouldBe` Map.fromList [(pLoc, Set.singleton aLoc), (qLoc, Set.singleton aLoc)]

        it "handles NULL pointer assignment" $ do
            stmt <- mustParseStmt ["p = nullptr;"]
            let stateBefore = Map.singleton pLoc (Set.singleton xLoc)
            let (finalMap, _) = analyzeStatementForPointers Map.empty ctx "f" stateBefore stmt
            finalMap `shouldBe` Map.empty

        it "handles variable declaration with a casted function call" $ do
            ast <- mustParse
                [ "struct Packet_Data { int x; };"
                , "void* malloc(int size);"
                , "void f() {"
                , "  struct Packet_Data *new_d = (struct Packet_Data *)malloc(sizeof(struct Packet_Data));"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "f"
            let newDLoc = VarLocation "new_d"
            Map.lookup newDLoc finalMap `shouldNotBe` Nothing

        it "should support macros with assigns (internal)" $ do
            defineStmt <- head <$> mustParse ["#define ASSIGN_PTR(p, a) do { p = &a; } while (0)"]
            callStmt <- mustParseStmt ["ASSIGN_PTR(p, a);"]
            let (stateAfterDefine, _) = transferPointsToState ctx "f" (PointsToState Map.empty Map.empty) defineStmt
            let (finalMap, _) = analyzeStatementForPointers (ptsMacros stateAfterDefine) ctx "f" (ptsMap stateAfterDefine) callStmt
            let aLoc = VarLocation "a"
            finalMap `shouldBe` Map.singleton pLoc (Set.singleton aLoc)

        it "should support macros with assigns" $ do
            ast <- mustParse
                [ "void f(int* in, int** out) {"
                , "  #define GET_PTR(p) do { *p = in; } while (0)"
                , "  GET_PTR(out);"
                , "  #undef GET_PTR"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "f"
            let outLoc = DerefLocation (VarLocation "out")
            let inLoc = VarLocation "in"
            Map.lookup outLoc finalMap `shouldBe` Just (Set.singleton inLoc)

        it "should support macros with multiple statements and arguments" $ do
            ast <- mustParse
                [ "void f(int* x, int* y) {"
                , "  #define SWAP(a, b) do { int* tmp = a; a = b; b = tmp; } while (0)"
                , "  int *p = x;"
                , "  int *q = y;"
                , "  SWAP(p, q);"
                , "  #undef SWAP"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "f"
            Map.lookup pLoc finalMap `shouldBe` Just (Set.singleton (DerefLocation yLoc))
            Map.lookup qLoc finalMap `shouldBe` Just (Set.singleton (DerefLocation xLoc))

        it "handles assignments to a pointer within a function call" $ do
            ast <- mustParse
                [ "void assign_ptr(int** pp, int* val) {"
                , "  *pp = val;"
                , "}"
                , "void f() {"
                , "  int* p;"
                , "  int x;"
                , "  assign_ptr(&p, &x);"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "f"
            Map.lookup pLoc finalMap `shouldBe` Just (Set.singleton xLoc)

        it "handles assignments to a casted pointer within a function call" $ do
            ast <- mustParse
                [ "void assign_ptr(int** pp, int* val) {"
                , "  *pp = val;"
                , "}"
                , "void f() {"
                , "  int* p;"
                , "  int x;"
                , "  assign_ptr((int**)&p, &x);"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "f"
            Map.lookup pLoc finalMap `shouldBe` Just (Set.singleton xLoc)

        it "handles scoped macro definitions" $ do
            ast <- mustParse
                [ "void f(int* x, int* y) {"
                , "  #define PTR_ASSIGN(a, b) do { a = b; } while (0)"
                , "  int *p;"
                , "  PTR_ASSIGN(p, x);"
                , "  #undef PTR_ASSIGN"
                , "  int *q;"
                , "  PTR_ASSIGN(q, y);"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "f"
            Map.lookup pLoc finalMap `shouldBe` Just (Set.singleton (DerefLocation xLoc))
            Map.lookup qLoc finalMap `shouldBe` Nothing

    describe "Inter-procedural PointsTo Analysis" $ do
        it "propagates a pointer returned from a simple function" $ do
            ast <- mustParse
                [ "static const int x = 3;"
                , "const int* get_x() { return &x; }"
                , "void main() { const int* p = get_x(); }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let pLoc = VarLocation "p"
            let xLoc = GlobalVarLocation "x" -- Assuming static vars are treated as globals
            Map.lookup pLoc finalMap `shouldBe` Just (Set.singleton xLoc)

        it "propagates a pointer through a simple identity function" $ do
            ast <- mustParse
                [ "int* identity(int* p) { return p; }"
                , "void main() { int x; int* p = &x; int* q = identity(p); }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let qLoc = VarLocation "q"
            let xLoc = VarLocation "x"
            Map.lookup qLoc finalMap `shouldBe` Just (Set.singleton xLoc)

        it "propagates a pointer with arithmetic through an identity function" $ do
            ast <- mustParse
                [ "int* identity(int* p) { return p; }"
                , "void main() { int x; int* p = &x; int* q = identity(p + 1); }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let qLoc = VarLocation "q"
            let xLoc = VarLocation "x"
            Map.lookup qLoc finalMap `shouldBe` Just (Set.singleton xLoc)

        it "handles side-effects on pointers passed as arguments" $ do
            ast <- mustParse
                [ "void make_p_point_to_y(int** p, int* y) { *p = y; }"
                , "void main() { int x; int y; int* p = &x; make_p_point_to_y(&p, &y); }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let pLoc = VarLocation "p"
            let yLoc = VarLocation "y"
            -- After the call, p in main should point to y.
            pendingWith "side-effects"
            Map.lookup pLoc finalMap `shouldBe` Just (Set.singleton yLoc)

        it "handles self-referential assignment with pointer member access" $ do
            ast <- mustParse
                [ "struct MySession { int* rate_ptr; };"
                , "int* my_func(int* input) { return input; }"
                , "void do_work(struct MySession *s) {"
                , "  int x;"
                , "  s->rate_ptr = &x;"
                , "  s->rate_ptr = my_func(s->rate_ptr);"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "do_work"
            let sDerefLoc = DerefLocation (VarLocation "s")
            let ratePtrLoc = FieldLocation sDerefLoc "rate_ptr"
            let xLoc = VarLocation "x"
            Map.lookup ratePtrLoc finalMap `shouldBe` Just (Set.singleton xLoc)

        it "handles writing a non-pointer value via a pointer argument" $ do
            ast <- mustParse
                [ "void write_int(int* p, int x) { *p = x; }"
                , "void main() { int x; write_int(&x, 1); }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let xLoc = VarLocation "x"
            -- The analysis should record that `x` was the target of a pointer
            -- assignment. Since an integer was written, its points-to set is empty.
            finalMap `shouldBe` Map.singleton xLoc Set.empty

        it "handles writing a sizeof value via a pointer argument" $ do
            ast <- mustParse
                [ "void write_int(int* p, int x) { *p = x; }"
                , "void main() { int x; write_int(&x, sizeof(int)); }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let xLoc = VarLocation "x"
            -- The analysis should handle sizeof correctly and the points-to map for x should be empty.
            finalMap `shouldBe` Map.singleton xLoc Set.empty

        it "handles writing a sizeof(array) value via a pointer argument" $ do
            ast <- mustParse
                [ "void write_int(int* p, int x) { *p = x; }"
                , "void main() { int arr[10]; int x; write_int(&x, sizeof(arr)); }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let xLoc = VarLocation "x"
            -- The analysis should handle sizeof correctly and the points-to map for x should be empty.
            finalMap `shouldBe` Map.singleton xLoc Set.empty

        it "handles writing a binary op value via a pointer argument" $ do
            ast <- mustParse
                [ "void write_int(int* p, int x) { *p = x; }"
                , "void main() { int x; write_int(&x, 1 + 2); }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let xLoc = VarLocation "x"
            -- The analysis should handle the binary op correctly and the points-to map for x should be empty.
            finalMap `shouldBe` Map.singleton xLoc Set.empty

        it "handles function calls that modify pointer arguments to point to local variables" $ do
            ast <- mustParse
                [ "void update_ptr(int** pp) { int y; *pp = &y; }"
                , "void main() { int* p; update_ptr(&p); }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let pLoc = VarLocation "p"
            let yLoc = GlobalVarLocation "y" -- y is local to update_ptr, but from main's perspective it's a global
            Map.lookup pLoc finalMap `shouldBe` Just (Set.singleton yLoc)

        it "handles mutual recursion correctly" $ do
            ast <- mustParse
                [ "int* g(int* p);"
                , "int* f(int* p) { if (p) { return g(p); } else { return p; } }"
                , "int* g(int* p) { if (p) { return f(p); } else { return p; } }"
                , "void main() { int x; int* p = &x; int* q = f(p); }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let qLoc = VarLocation "q"
            let xLoc = VarLocation "x"
            -- q should point to x after the recursive calls resolve.
            Map.lookup qLoc finalMap `shouldBe` Just (Set.singleton xLoc)

        it "handles returning a pointer to an array element from a function" $ do
            ast <- mustParse
                [ "int* get_element(int arr[], int index) { return &arr[index]; }"
                , "void main() { int my_array[10]; int* element = get_element(my_array, 2); }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let elementLoc = VarLocation "element"
            let arrayLoc = VarLocation "my_array"
            Map.lookup elementLoc finalMap `shouldBe` Just (Set.singleton arrayLoc)

        it "tracks a simple function pointer" $ do
            ast <- mustParse
                [ "typedef void my_cb();"
                , "void my_func() { return; }"
                , "void main() { my_cb *fptr = &my_func; }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let fptrLoc = VarLocation "fptr"
            let funcLoc = FunctionLocation "my_func"
            Map.lookup fptrLoc finalMap `shouldBe` Just (Set.singleton funcLoc)

        it "tracks a function pointer inside a struct" $ do
            ast <- mustParse
                [ "typedef void my_cb();"
                , "struct Function_Table { my_cb *fp; };"
                , "void my_func() { return; }"
                , "void main() { struct Function_Table s; s.fp = &my_func; my_cb *p = s.fp; }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let pLoc = VarLocation "p"
            let funcLoc = FunctionLocation "my_func"
            Map.lookup pLoc finalMap `shouldBe` Just (Set.singleton funcLoc)

        it "tracks a function pointer inside a heap-allocated struct" $ do
            ast <- mustParse
                [ "typedef void my_cb();"
                , "struct Function_Table { my_cb *fp; };"
                , "void my_func() { return; }"
                , "void main() { struct Function_Table* s = malloc(sizeof(struct Function_Table)); s->fp = &my_func; my_cb *p = s->fp; }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let pLoc = VarLocation "p"
            let funcLoc = FunctionLocation "my_func"
            Map.lookup pLoc finalMap `shouldBe` Just (Set.singleton funcLoc)

        it "tracks multiple functions for a single function pointer, including one where we only have a decl, no defn" $ do
            ast <- mustParse
                [ "typedef void my_cb();"
                , "struct Function_Table { my_cb *fp; };"
                , "void my_func1();"
                , "void my_func2() { return; }"
                , "void main() {"
                , "  struct Function_Table s;"
                , "  if (1) { s.fp = &my_func1; } else { s.fp = &my_func2; }"
                , "  my_cb *p = s.fp;"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let pLoc = VarLocation "p"
            let func1Loc = FunctionLocation "my_func1"
            let func2Loc = FunctionLocation "my_func2"
            Map.lookup pLoc finalMap `shouldBe` Just (Set.fromList [func1Loc, func2Loc])

        it "tracks a function pointer set via a function call" $ do
            ast <- mustParse
                [ "typedef void my_cb();"
                , "struct Function_Table { my_cb *fp; };"
                , "void my_func() { return; }"
                , "void set_callback(struct Function_Table* s, my_cb* f) { s->fp = f; }"
                , "void main() {"
                , "  struct Function_Table s;"
                , "  set_callback(&s, &my_func);"
                , "  my_cb *p = s.fp;"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let pLoc = VarLocation "p"
            let funcLoc = FunctionLocation "my_func"
            Map.lookup pLoc finalMap `shouldBe` Just (Set.singleton funcLoc)

        it "tracks a function pointer set via a function call with a struct pointer" $ do
            ast <- mustParse
                [ "typedef void my_cb();"
                , "struct Function_Table { my_cb *fp; };"
                , "void my_func() { return; }"
                , "void set_callback(struct Function_Table* s) { s->fp = &my_func; }"
                , "void main() {"
                , "  struct Function_Table s;"
                , "  set_callback(&s);"
                , "  my_cb *p = s.fp;"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let pLoc = VarLocation "p"
            let funcLoc = FunctionLocation "my_func"
            Map.lookup pLoc finalMap `shouldBe` Just (Set.singleton funcLoc)

        it "resolves a function pointer call through a struct passed by pointer" $ do
            ast <- mustParse
                [ "typedef void my_cb(int x);"
                , "void sink(int x) { return; }"
                , "struct FTable { my_cb* f; };"
                , "void call_it(struct FTable* t, int a) { t->f(a); }"
                , "void main() {"
                , "  struct FTable table;"
                , "  table.f = &sink;"
                , "  call_it(&table, 1);"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let callGraph = ptcCallGraph pointsToContext
            let callees = getCallees callGraph "call_it"
            Map.keys callees `shouldBe` ["sink"]

        it "handles a function call as an argument to another function call" $ do
            ast <- mustParse
                [ "int* h() { int y; int* q = &y; return q; }"
                , "int* g(int* p) { return p; }"
                , "void main() { int x; int* p = &x; int* r = g(h()); }"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let rLoc = VarLocation "r"
            let yLoc = GlobalVarLocation "y" -- y is local to h, but from main's perspective it's a global
            Map.lookup rLoc finalMap `shouldBe` Just (Set.singleton yLoc)

        it "handles assignment with a function call in a binary expression" $ do
            ast <- mustParse
                [ "int mono_time_get(int mono_time) { return mono_time; }"
                , "struct Entry { int store_until; };"
                , "void f(struct Entry *entry, int announce, int timeout) {"
                , "  entry->store_until = mono_time_get(announce) + timeout;"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "f"
            let entryDerefLoc = DerefLocation (VarLocation "entry")
            let storeUntilLoc = FieldLocation entryDerefLoc "store_until"
            Map.lookup storeUntilLoc finalMap `shouldBe` Just Set.empty

        it "handles conditional assignment with #ifdef" $ do
            ast <- mustParse
                [ "void main() {"
                , "  int x;"
                , "  int y;"
                , "  int *p;"
                , "#ifdef SOME_MACRO"
                , "  p = &x;"
                , "#else"
                , "  p = &y;"
                , "#endif /* SOME_MACRO */"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let pLoc = VarLocation "p"
            let xLoc = VarLocation "x"
            let yLoc = VarLocation "y"
            -- The analysis should explore both branches of the #ifdef,
            -- so p can point to either x or y.
            Map.lookup pLoc finalMap `shouldBe` Just (Set.fromList [xLoc, yLoc])

        it "handles aliasing through a union" $ do
            ast <- mustParse
                [ "union MyUnion { int* a; float* b; };"
                , "void main() {"
                , "  int x;"
                , "  union MyUnion u;"
                , "  u.a = &x;"
                , "  float* p = u.b;"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let pLoc = VarLocation "p"
            let xLoc = VarLocation "x"
            -- Because of union aliasing, p should point to x, even though it was read from u.b
            Map.lookup pLoc finalMap `shouldBe` Just (Set.singleton xLoc)

        it "handles conditional assignment to union members" $ do
            ast <- mustParse
                [ "union MyUnion { int* a; int* b; };"
                , "void main(int cond) {"
                , "  int x;"
                , "  int y;"
                , "  union MyUnion u;"
                , "  if (cond) {"
                , "    u.a = &x;"
                , "  } else {"
                , "    u.b = &y;"
                , "  }"
                , "  int* p = u.a;"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let pLoc = VarLocation "p"
            let xLoc = VarLocation "x"
            let yLoc = VarLocation "y"
            -- After the conditional, p (read from u.a) could point to either x or y
            Map.lookup pLoc finalMap `shouldBe` Just (Set.fromList [xLoc, yLoc])

        it "handles assignment into a pointer to a struct member returned by a function" $ do
            ast <- mustParse
                [ "struct Foo_Bar { int value; };"
                , "void set_ptr(int *ptr, int value) { *ptr = value; }"
                , "int *get_ptr(struct Foo_Bar *foo) { return &foo->value; }"
                , "void f(struct Foo_Bar *foo) {"
                , "  set_ptr(get_ptr(foo), 3);"
                , "}"
                ]
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "f"
            let entryDerefLoc = DerefLocation (VarLocation "foo")
            let storeUntilLoc = FieldLocation entryDerefLoc "value"
            Map.lookup storeUntilLoc finalMap `shouldBe` Just Set.empty

        it "handles self-recursion inside a for-loop and if-statement" $ do
            ast <- mustParse
                [ "void recursive_loop(int* p, int count) {"
                , "  for (int i = 0; i < count; ++i) {"
                , "    if (i == 5) {"
                , "      recursive_loop(p, count - 1);"
                , "    }"
                , "  }"
                , "}"
                , "void main() {"
                , "  int x;"
                , "  int* p = &x;"
                , "  recursive_loop(p, 10);"
                , "}"
                ]
            pendingWith "recursion currently has infinite loops in the analysis"
            let tus = [("test.c", ast)]
            let pointsToContext = buildPointsToContext tus (buildCallGraph tus Map.empty) Map.empty
            let finalMap = analyzeFunctionWithSummaries pointsToContext "main"
            let pLoc = VarLocation "p"
            let xLoc = VarLocation "x"
            Map.lookup pLoc finalMap `shouldBe` Just (Set.singleton xLoc)
