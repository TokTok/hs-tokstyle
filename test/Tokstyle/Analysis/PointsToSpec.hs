{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Tokstyle.Analysis.PointsToSpec where

import           Control.Monad                       (foldM, when)
import           Control.Monad.State.Strict          (execState, put, runState)
import           Data.Fix                            (Fix (..))
import           Data.IntMap.Strict                  (IntMap)
import qualified Data.IntMap.Strict                  as IntMap
import           Data.IntSet                         (IntSet)
import qualified Data.IntSet                         as IntSet
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as Map
import           Data.Maybe                          (fromMaybe, mapMaybe)
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import           Debug.Trace                         (traceShow)
import qualified Language.Cimple                     as C
import           Language.Cimple.TraverseAst
import           Test.Hspec                          (Spec, describe, it,
                                                      shouldBe)
import           Tokstyle.Analysis.DataFlow          (CFGNode (..), buildCFG,
                                                      fixpoint, join)
import           Tokstyle.Analysis.PointsTo.Fixpoint (findEntryPointsAndFuncMap,
                                                      runGlobalFixpoint)
import           Tokstyle.Analysis.PointsTo.Types    (GlobalEnv (..),
                                                      IMemLoc (..), MemLoc (..),
                                                      MemLocPool (..),
                                                      PointsToAnalysis,
                                                      PointsToContext (..),
                                                      PointsToFact (..),
                                                      RelevantInputState (..),
                                                      intern)
import           Tokstyle.Analysis.Scope             (ScopedId (..),
                                                      runScopePass)
import           Tokstyle.Analysis.VTable            (resolveVTables)
import           Tokstyle.Common.TypeSystem          (collect)
import           Tokstyle.LinterSpec                 (mustParse)

findFunc :: Text -> [C.Node (C.Lexeme ScopedId)] -> C.Node (C.Lexeme ScopedId)
findFunc name ast =
    let
        finder (node@(Fix (C.FunctionDefn _ (Fix (C.FunctionPrototype _ (C.L _ _ ScopedId{sidName}) _)) _))) =
            when (sidName == name) (put $ Just node)
        finder _ = return ()
        actions = astActions { doNode = \_ n act -> finder n >> act }
    in
        fromMaybe (error $ "Function not found: " ++ show name) $
        execState (traverseAst actions ast) Nothing

getParams :: C.Node (C.Lexeme ScopedId) -> [ScopedId]
getParams (Fix (C.FunctionDefn _ (Fix (C.FunctionPrototype _ _ params)) _)) =
    mapMaybe getParamId params
  where
    getParamId (Fix (C.VarDecl _ (C.L _ _ sid) _)) = Just sid
    getParamId _                                   = Nothing
getParams _ = []



runPointsToAnalysis :: [Text] -> IO ([C.Node (C.Lexeme ScopedId)], PointsToFact, MemLocPool)
runPointsToAnalysis code = do
    parsed <-mustParse code
    let typeSystem = collect [("test.c", parsed)]
    let (scopedAst, _) = runScopePass parsed
    let vtableMap = resolveVTables scopedAst typeSystem
    let (entryPoints, funcMap) = findEntryPointsAndFuncMap scopedAst

    -- For our test cases, we expect a single entry point.
    when (length entryPoints /= 1) $
        error $ "Expected 1 entry point for the test, but found " ++ show (length entryPoints) ++ " (" ++ show (map sidName entryPoints) ++ ")"

    let entryPointId = head entryPoints

    let dummyId = ScopedId 0 "" C.Global
    let ctx = PointsToContext "test.c" typeSystem vtableMap (GlobalEnv Map.empty) funcMap dummyId Map.empty :: PointsToContext ScopedId
    -- NOTE: This is a simplified harness that doesn't create a proper call-site
    -- for the function under test. For functions with parameters, we initialize
    -- them to point to UnknownLoc to simulate being called from an unknown context.
    let (_, _, cfgCache, pool) = runGlobalFixpoint ctx scopedAst
    -- For entry points, we need to find the RelevantInputState it was analyzed with.
    let entryPointKeys = filter (\(fid, _) -> fid == entryPointId) (Map.keys cfgCache)
    when (null entryPointKeys) $ error "Entry point not found in cache"
    -- Assuming only one analysis context for the entry point in these simple tests.
    let (_, entryPointRIS) = head entryPointKeys
    let (cfgs, _) = fromMaybe (error "CFGs for entry point not found in cache") (Map.lookup (entryPointId, entryPointRIS) cfgCache)
    -- Merge exit facts from all CFGs for this entry point (e.g. if there were #ifdefs)
    let (finalFact, _) = runState (foldM (join ctx) (cfgOutFacts $ last (Map.elems (head cfgs))) (map (\cfg -> cfgOutFacts $ last (Map.elems cfg)) (tail cfgs))) pool
    return (scopedAst, finalFact, pool)

findSid :: Text -> [C.Node (C.Lexeme ScopedId)] -> ScopedId
findSid name asts =
    let
        finder s@ScopedId{sidName} = when (sidName == name) (put $ Just s)
        actions = astActions { doLexeme = \_ (C.L _ _ sid) act -> finder sid >> act }
    in
        fromMaybe (error $ "ScopedId not found for: " ++ show name) $
        execState (mapM_ (traverseAst actions) asts) Nothing

runEntryPointAnalysis :: [Text] -> IO [Text]
runEntryPointAnalysis code = do
    parsed <- mustParse code
    let (scopedAst, _) = runScopePass parsed
    let (entryPoints, _) = findEntryPointsAndFuncMap scopedAst
    return $ Set.toList $ Set.fromList $ map sidName entryPoints

uninternSet :: MemLocPool -> IntSet -> Set MemLoc
uninternSet pool iset = Set.fromList $ map (\i -> IntMap.findWithDefault UnknownLoc i (idToMemLoc pool)) (IntSet.toList iset)

uninternFact :: MemLocPool -> PointsToFact -> (Map ScopedId (Set MemLoc), Map MemLoc (Set MemLoc), Set MemLoc)
uninternFact pool (PointsToFact vm mm uw) =
    ( Map.map (uninternSet pool) vm
    , Map.fromList $ map (\(k, v) -> (IntMap.findWithDefault UnknownLoc k (idToMemLoc pool), uninternSet pool v)) (IntMap.toList mm)
    , uninternSet pool uw
    )

spec :: Spec
spec = do
    describe "Entry Point Detection" $ do
        it "should identify un-called functions as entry points" $ do
            entryPoints <- runEntryPointAnalysis
                [ "void called_func() { /* empty */ }"
                , "void entry_func() { called_func(); }"
                ]
            entryPoints `shouldBe` ["entry_func"]

        it "should not consider address-taken functions as entry points" $ do
            entryPoints <- runEntryPointAnalysis
                [ "typedef void referenced_func_cb(void);"
                , "void referenced_func() { /* empty */ }"
                , "void entry_func() { referenced_func_cb *ptr = referenced_func; }"
                ]
            entryPoints `shouldBe` ["entry_func"]

        it "should not identify mutually recursive functions as entry points" $ do
            entryPoints <- runEntryPointAnalysis
                [ "void func_a();"
                , "void func_b() { func_a(); }"
                , "void func_a() { func_b(); }"
                ]
            entryPoints `shouldBe` []

        it "should identify a completely unused function as an entry point" $ do
            entryPoints <- runEntryPointAnalysis
                [ "void dead_func() { /* empty */ }"
                ]
            entryPoints `shouldBe` ["dead_func"]

        it "should identify functions called via cast as referenced" $ do
            entryPoints <- runEntryPointAnalysis
                [ "void called_func() { /* empty */ }"
                , "typedef void func_cb(void);"
                , "void entry_func() { ((func_cb*)called_func)(); }"
                ]
            entryPoints `shouldBe` ["entry_func"]

    describe "Points-To Analysis" $ do
        it "should handle simple address-of assignments" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void test() {"
                , "  int a;"
                , "  int* p = &a;"
                , "}"
                ]
            let p_id = findSid "p" ast
            let a_id = findSid "a" ast
            let expectedVarMap = Map.singleton p_id (Set.singleton (StackLoc a_id))
            uninternFact pool finalFact `shouldBe` (expectedVarMap, Map.empty, Set.empty)

        it "should handle pointer assignments" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void test() {"
                , "  int a;"
                , "  int* p = &a;"
                , "  int* q = p;"
                , "}"
                ]
            let p_id = findSid "p" ast
            let q_id = findSid "q" ast
            let a_id = findSid "a" ast
            let expectedVarMap = Map.fromList
                    [ (p_id, Set.singleton (StackLoc a_id))
                    , (q_id, Set.singleton (StackLoc a_id))
                    ]
            uninternFact pool finalFact `shouldBe` (expectedVarMap, Map.empty, Set.empty)

        it "should handle simple conditional assignments" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void test(int cond) {"
                , "  int a;"
                , "  int b;"
                , "  int* p;"
                , "  if (cond) {"
                , "    p = &a;"
                , "  } else {"
                , "    p = &b;"
                , "  }"
                , "}"
                ]
            let p_id = findSid "p" ast
            let a_id = findSid "a" ast
            let b_id = findSid "b" ast
            let expectedVarMap = Map.singleton p_id (Set.fromList [StackLoc a_id, StackLoc b_id])
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton p_id) `shouldBe` expectedVarMap

        it "should handle simple store operations" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void test() {"
                , "  int a;"
                , "  int* p;"
                , "  int** q = &p;"
                , "  *q = &a;"
                , "}"
                ]
            let p_id = findSid "p" ast
            let a_id = findSid "a" ast
            let q_id = findSid "q" ast
            let expectedVarMap = Map.fromList
                    [ (q_id, Set.singleton (StackLoc p_id))
                    , (p_id, Set.singleton (StackLoc a_id))
                    ]
            let expectedMemMap = Map.singleton (StackLoc p_id) (Set.singleton (StackLoc a_id))
            uninternFact pool finalFact `shouldBe` (expectedVarMap, expectedMemMap, Set.empty)

        it "should handle simple load operations" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void test() {"
                , "  int a;"
                , "  int* p = &a;"
                , "  int** q = &p;"
                , "  int* r = *q;"
                , "}"
                ]
            let p_id = findSid "p" ast
            let q_id = findSid "q" ast
            let r_id = findSid "r" ast
            let a_id = findSid "a" ast
            let expectedVarMap = Map.fromList
                    [ (p_id, Set.singleton (StackLoc a_id))
                    , (q_id, Set.singleton (StackLoc p_id))
                    , (r_id, Set.singleton (StackLoc a_id))
                    ]
            uninternFact pool finalFact `shouldBe` (expectedVarMap, Map.empty, Set.empty)

        it "should handle NULL pointers" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void test() {"
                , "  int* p = nullptr;"
                , "}"
                ]
            let p_id = findSid "p" ast
            let expectedVarMap = Map.singleton p_id (Set.singleton NullLoc)
            uninternFact pool finalFact `shouldBe` (expectedVarMap, Map.empty, Set.empty)

        it "should be field-sensitive" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "struct Struct { int x; int y; };"
                , "void test() {"
                , "  struct Struct s;"
                , "  int* p = &s.x;"
                , "  int* q = &s.y;"
                , "}"
                ]
            let s_id = findSid "s" ast
            let p_id = findSid "p" ast
            let q_id = findSid "q" ast
            let s_loc = StackLoc s_id
            let expectedVarMap = Map.fromList
                    [ (p_id, Set.singleton (FieldLoc s_loc "x"))
                    , (q_id, Set.singleton (FieldLoc s_loc "y"))
                    ]
            uninternFact pool finalFact `shouldBe` (expectedVarMap, Map.empty, Set.empty)

        it "should handle heap allocation" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void* malloc(unsigned long size);"
                , "void test() {"
                , "  int* p = (int*)malloc(sizeof(int));"
                , "}"
                ]
            let p_id = findSid "p" ast
            let expectedLoc = HeapLoc "test.c:3:18"
            let expectedVarMap = Map.singleton p_id (Set.singleton expectedLoc)
            uninternFact pool finalFact `shouldBe` (expectedVarMap, Map.empty, Set.empty)

        it "should handle store and load via heap" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void* malloc(unsigned long size);"
                , "void test() {"
                , "  int a;"
                , "  int* q = &a;"
                , "  int** p = (int**)malloc(sizeof(int*));"
                , "  *p = q;"
                , "  int* r = *p;"
                , "}"
                ]
            let p_id = findSid "p" ast
            let q_id = findSid "q" ast
            let r_id = findSid "r" ast
            let a_id = findSid "a" ast

            let heap_loc = HeapLoc "test.c:5:20"

            let expectedVarMap = Map.fromList
                  [ (q_id, Set.singleton (StackLoc a_id))
                  , (p_id, Set.singleton heap_loc)
                  , (r_id, Set.singleton (StackLoc a_id))
                  ]
            let expectedMemMap = Map.fromList
                  [ (heap_loc, Set.singleton (StackLoc a_id)) ]

            uninternFact pool finalFact `shouldBe` (expectedVarMap, expectedMemMap, Set.empty)

        it "should track stores via external parameters" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void test(int** p) {"
                , "  int a;"
                , "  int* q = &a;"
                , "  *p = q;"
                , "}"
                ]
            let a_id = findSid "a" ast
            let q_id = findSid "q" ast
            let p_id = findSid "p" ast

            -- The parameter 'p' should point to an abstract external location.
            let p_points_to = Set.singleton (ExternalParamLoc "test" "p")
            -- The variable 'q' should point to the stack location of 'a'.
            let q_points_to = Set.singleton (StackLoc a_id)
            -- The store '*p = q' should result in the abstract external location
            -- now pointing to what 'q' points to.
            let expectedMemMap = Map.singleton (ExternalParamLoc "test" "p") q_points_to

            let expectedVarMap = Map.fromList
                    [ (q_id, q_points_to)
                    , (p_id, p_points_to)
                    ]

            uninternFact pool finalFact `shouldBe` (expectedVarMap, expectedMemMap, Set.empty)

        it "should propagate UnknownLoc on load" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void test(int** p) {"
                , "  int* q = *p;"
                , "}"
                ]
            let q_id = findSid "q" ast
            let expectedVarMap = Map.singleton q_id (Set.singleton UnknownLoc)
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton q_id) `shouldBe` expectedVarMap

        it "should handle conditional null assignment" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void test(int cond) {"
                , "  int a;"
                , "  int* p = &a;"
                , "  if (cond) {"
                , "    p = nullptr;"
                , "  }"
                , "}"
                ]
            let p_id = findSid "p" ast
            let a_id = findSid "a" ast
            let expectedVarMap = Map.singleton p_id (Set.fromList [StackLoc a_id, NullLoc])
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton p_id) `shouldBe` expectedVarMap

        it "should handle storing into a struct field via a pointer" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "struct Struct { int* x; };"
                , "void test() {"
                , "  struct Struct s;"
                , "  int a;"
                , "  s.x = &a;"
                , "}"
                ]
            let s_id = findSid "s" ast
            let a_id = findSid "a" ast
            let s_loc = StackLoc s_id
            let field_loc = FieldLoc s_loc "x"
            let expectedMemMap = Map.singleton field_loc (Set.singleton (StackLoc a_id))
            uninternFact pool finalFact `shouldBe` (Map.empty, expectedMemMap, Set.empty)

        it "should handle loading from a struct field via a pointer" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "struct Struct { int* x; };"
                , "void test() {"
                , "  struct Struct s;"
                , "  int a;"
                , "  s.x = &a;"
                , "  int* p = s.x;"
                , "}"
                ]
            let s_id = findSid "s" ast
            let a_id = findSid "a" ast
            let p_id = findSid "p" ast
            let s_loc = StackLoc s_id
            let field_loc = FieldLoc s_loc "x"
            let expectedMemMap = Map.singleton field_loc (Set.singleton (StackLoc a_id))
            let expectedVarMap = Map.singleton p_id (Set.singleton (StackLoc a_id))
            uninternFact pool finalFact `shouldBe` (expectedVarMap, expectedMemMap, Set.empty)

        it "should merge points-to sets in a loop" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void test(int cond) {"
                , "  int a; int b;"
                , "  int* p = &a;"
                , "  while (cond) {"
                , "    p = &b;"
                , "  }"
                , "}"
                ]
            let p_id = findSid "p" ast
            let a_id = findSid "a" ast
            let b_id = findSid "b" ast
            let expectedVarMap = Map.singleton p_id (Set.fromList [StackLoc a_id, StackLoc b_id])
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton p_id) `shouldBe` expectedVarMap

        it "should handle multi-level pointer stores" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                -- Our parser doesn't support int*** but we can work around it
                -- with a typedef.
                [ "typedef int *Int_Ptr;"
                , "void test() {"
                , "  int a;"
                , "  int* p1;"
                , "  int** p2 = &p1;"
                , "  Int_Ptr** p3 = &p2;"
                , "  **p3 = &a;"
                , "}"
                ]
            let a_id = findSid "a" ast
            let p1_id = findSid "p1" ast
            let p2_id = findSid "p2" ast
            let p3_id = findSid "p3" ast
            let expectedVarMap = Map.fromList
                    [ (p3_id, Set.singleton (StackLoc p2_id))
                    , (p2_id, Set.singleton (StackLoc p1_id))
                    , (p1_id, Set.singleton (StackLoc a_id))
                    ]
            let expectedMemMap = Map.singleton (StackLoc p1_id) (Set.singleton (StackLoc a_id))
            uninternFact pool finalFact `shouldBe` (expectedVarMap, expectedMemMap, Set.empty)

        it "should handle multi-level pointer loads" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                -- Our parser doesn't support int*** but we can work around it
                -- with a typedef.
                [ "typedef int *Int_Ptr;"
                , "void test() {"
                , "  int a;"
                , "  int* p1 = &a;"
                , "  int** p2 = &p1;"
                , "  Int_Ptr** p3 = &p2;"
                , "  int* q = **p3;"
                , "}"
                ]
            let a_id = findSid "a" ast
            let p1_id = findSid "p1" ast
            let p2_id = findSid "p2" ast
            let p3_id = findSid "p3" ast
            let q_id = findSid "q" ast
            let expectedVarMap = Map.fromList
                    [ (p1_id, Set.singleton (StackLoc a_id))
                    , (p2_id, Set.singleton (StackLoc p1_id))
                    , (p3_id, Set.singleton (StackLoc p2_id))
                    , (q_id, Set.singleton (StackLoc a_id))
                    ]
            uninternFact pool finalFact `shouldBe` (expectedVarMap, Map.empty, Set.empty)

        it "should be sound in the presence of #ifdef function definitions" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void* malloc(unsigned long size);"
                , "#ifdef FLAG_A"
                , "void set_ptr_ifdef(int** p) {"
                , "  *p = (int*)malloc(sizeof(int));"
                , "}"
                , "#else"
                , "void set_ptr_ifdef(int** p) {"
                , "  /* This version does nothing */"
                , "}"
                , "#endif /* FLAG_A */"
                , "void test() {"
                , "  int* my_ptr = nullptr;"
                , "  set_ptr_ifdef(&my_ptr);"
                , "}"
                ]
            let my_ptr_id = findSid "my_ptr" ast
            -- A sound analysis must consider both definitions of set_ptr_ifdef.
            -- One definition assigns a heap location to *p, the other does nothing.
            -- Therefore, my_ptr could either point to the new heap location or remain NULL.
            let expectedHeapLoc = HeapLoc "test.c:4:14" -- Location of malloc
            let expectedVarMap = Map.singleton my_ptr_id (Set.fromList [NullLoc, expectedHeapLoc])
            -- This test will FAIL with the current implementation because it only sees
            -- one of the definitions (the last one), and will report that my_ptr can
            -- only be NullLoc. When the analysis is made sound, this test should pass.
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton my_ptr_id) `shouldBe` expectedVarMap

        it "should be sound with multiple function definitions" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void* malloc(unsigned long size);"
                , "void set_ptr_A(int** p) {"
                , "  *p = (int*)malloc(sizeof(int));"
                , "}"
                , "void set_ptr_B(int** p) {"
                , "  /* This version does nothing */"
                , "}"
                , "void test() {"
                , "  int* my_ptr = nullptr;"
                , "  set_ptr_A(&my_ptr);"
                , "  set_ptr_B(&my_ptr);"
                , "}"
                ]
            let my_ptr_id = findSid "my_ptr" ast
            let expectedHeapLoc = HeapLoc "test.c:3:14"
            let expectedVarMap = Map.singleton my_ptr_id (Set.singleton expectedHeapLoc)
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton my_ptr_id) `shouldBe` expectedVarMap

        it "should be fully context-sensitive" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void set_ptr(int** p, int* val) {"
                , "  *p = val;"
                , "}"
                , "void test() {"
                , "  int a; int b;"
                , "  int* p1;"
                , "  int* p2;"
                , "  set_ptr(&p1, &a);"
                , "  set_ptr(&p2, &b);"
                , "}"
                ]
            let p1_id = findSid "p1" ast
            let p2_id = findSid "p2" ast
            let a_id = findSid "a" ast
            let b_id = findSid "b" ast
            let expectedVarMap = Map.fromList
                    [ (p1_id, Set.singleton (StackLoc a_id))
                    , (p2_id, Set.singleton (StackLoc b_id))
                    ]
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.fromList [p1_id, p2_id]) `shouldBe` expectedVarMap

        it "should handle memcpy between structs with pointers" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void* memcpy(void* dest, const void* src, unsigned long n);"
                , "struct Struct { int* ptr; };"
                , "void test() {"
                , "  struct Struct s1; struct Struct s2;"
                , "  int a;"
                , "  s1.ptr = &a;"
                , "  s2.ptr = nullptr;"
                , "  memcpy(&s2, &s1, sizeof(struct Struct));"
                , "}"
                ]
            let s1_id = findSid "s1" ast
            let s2_id = findSid "s2" ast
            let a_id = findSid "a" ast
            let s1_loc = StackLoc s1_id
            let s2_loc = StackLoc s2_id
            let s1_ptr_loc = FieldLoc s1_loc "ptr"
            let s2_ptr_loc = FieldLoc s2_loc "ptr"
            let a_loc = StackLoc a_id
            let expectedMemMap = Map.fromList
                    [ (s1_ptr_loc, Set.singleton a_loc)
                    , (s2_ptr_loc, Set.singleton a_loc)
                    ]
            uninternFact pool finalFact `shouldBe` (Map.empty, expectedMemMap, Set.empty)

        it "should handle v-table based memory allocation" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void *calloc(unsigned int nmemb, unsigned int size);"
                , "typedef void mem_calloc_cb(void *obj, unsigned int nmemb, unsigned int size);"
                , "typedef struct Memory_Funcs { mem_calloc_cb *calloc; } Memory_Funcs;"
                , "typedef struct Memory { const Memory_Funcs *funcs; void *obj; } Memory;"
                , "void *sys_calloc(void *obj, unsigned int nmemb, unsigned int size) { return calloc(nmemb, size); }"
                , "static const Memory_Funcs os_memory_funcs = { sys_calloc };"
                , "static const Memory os_memory_obj = { &os_memory_funcs };"
                , "const Memory *os_memory(void) { return &os_memory_obj; }"
                , "void *mem_alloc(const Memory *mem, unsigned int size) {"
                , "  return mem->funcs->calloc(mem->obj, 1, size);"
                , "}"
                , "void test() {"
                , "  const Memory* mem = os_memory();"
                , "  int* p = (int*)mem_alloc(mem, sizeof(int));"
                , "}"
                ]
            let p_id = findSid "p" ast
            let expectedLoc = HeapLoc "test.c:5:77" -- This is the location of the actual `calloc` call inside `sys_calloc`.
            let expectedVarMap = Map.singleton p_id (Set.singleton expectedLoc)
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton p_id) `shouldBe` expectedVarMap

        it "should handle nested structs and field access" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "struct Inner { int* y; };"
                , "struct Outer { struct Inner* x; };"
                , "void* malloc(unsigned long size);"
                , "void test() {"
                , "  int a;"
                , "  struct Outer* o = (struct Outer*)malloc(sizeof(struct Outer));"
                , "  o->x = (struct Inner*)malloc(sizeof(struct Inner));"
                , "  o->x->y = &a;"
                , "  int* p = o->x->y;"
                , "}"
                ]
            let p_id = findSid "p" ast
            let a_id = findSid "a" ast
            let o_heap_loc = HeapLoc "test.c:6:36"
            let inner_heap_loc = HeapLoc "test.c:7:25"
            let field_x_loc = FieldLoc o_heap_loc "x"
            let field_y_loc = FieldLoc inner_heap_loc "y"
            let a_loc = StackLoc a_id

            let expectedMemMap = Map.fromList
                    [ (field_x_loc, Set.singleton inner_heap_loc)
                    , (field_y_loc, Set.singleton a_loc)
                    ]
            let expectedVarMap = Map.singleton p_id (Set.singleton a_loc)
            let (vm, mm, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton p_id) `shouldBe` expectedVarMap
            (mm `Map.restrictKeys` Set.fromList [field_x_loc, field_y_loc]) `shouldBe` expectedMemMap

        it "should use allocation-site abstraction for loops" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void* malloc(unsigned long size);"
                , "void test() {"
                , "  int* p;"
                , "  for (int i = 0; i < 10; ++i) {"
                , "    p = (int*)malloc(sizeof(int));"
                , "  }"
                , "}"
                ]
            let p_id = findSid "p" ast
            let heap_loc = HeapLoc "test.c:5:15"
            let expectedVarMap = Map.singleton p_id (Set.singleton heap_loc)
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton p_id) `shouldBe` expectedVarMap

        it "should handle indirect calls via function pointers" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void* malloc(unsigned long size);"
                , "void func_A(int** p) { *p = (int*)malloc(sizeof(int)); }"
                , "void func_B(int** p) { *p = nullptr; }"
                , "typedef void func_ptr_cb(int** p);"
                , "void test(int cond) {"
                , "  func_ptr_cb* fp;"
                , "  if (cond) { fp = func_A; } else { fp = func_B; }"
                , "  int* my_ptr = nullptr;"
                , "  fp(&my_ptr);"
                , "}"
                ]
            let my_ptr_id = findSid "my_ptr" ast
            let expectedHeapLoc = HeapLoc "test.c:2:35" -- Location of malloc in func_A
            let expectedVarMap = Map.singleton my_ptr_id (Set.fromList [NullLoc, expectedHeapLoc])
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton my_ptr_id) `shouldBe` expectedVarMap

        it "should be context-sensitive with return values" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "int* id(int* p) { return p; }"
                , "void test() {"
                , "  int a;"
                , "  int b;"
                , "  int* p1 = &a;"
                , "  int* p2 = &b;"
                , "  int* r1 = id(p1);"
                , "  int* r2 = id(p2);"
                , "}"
                ]
            let r1_id = findSid "r1" ast
            let r2_id = findSid "r2" ast
            let a_id = findSid "a" ast
            let b_id = findSid "b" ast
            let expectedVarMap = Map.fromList
                    [ (r1_id, Set.singleton (StackLoc a_id))
                    , (r2_id, Set.singleton (StackLoc b_id))
                    ]
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.fromList [r1_id, r2_id]) `shouldBe` expectedVarMap

        it "should handle deep call chain for parameter modification" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void* malloc(unsigned long size);"
                , "void allocate_deep(int** p) { *p = (int*)malloc(sizeof(int)); }"
                , "void allocate_mid(int** p) { allocate_deep(p); }"
                , "void test() {"
                , "  int* q = nullptr;"
                , "  allocate_mid(&q);"
                , "}"
                ]
            let q_id = findSid "q" ast
            let expectedHeapLoc = HeapLoc "test.c:2:42"
            let expectedVarMap = Map.singleton q_id (Set.singleton expectedHeapLoc)
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton q_id) `shouldBe` expectedVarMap

        it "should handle indirect calls with a no-op path" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void* malloc(unsigned long size);"
                , "void func_A(int** p) { *p = (int*)malloc(sizeof(int)); }"
                , "void func_B(int** p) { /* Does nothing */ }"
                , "typedef void func_ptr_cb(int** p);"
                , "void test_noop_path(int cond) {"
                , "  func_ptr_cb* fp;"
                , "  if (cond) { fp = func_A; } else { fp = func_B; }"
                , "  int* my_ptr = nullptr;"
                , "  fp(&my_ptr);"
                , "}"
                ]
            let my_ptr_id = findSid "my_ptr" ast
            let expectedHeapLoc = HeapLoc "test.c:2:35"
            let expectedVarMap = Map.singleton my_ptr_id (Set.fromList [NullLoc, expectedHeapLoc])
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton my_ptr_id) `shouldBe` expectedVarMap

        it "should handle merging return values from indirect calls" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void* malloc(unsigned long size);"
                , "int* func_A() { return (int*)malloc(sizeof(int)); }"
                , "int* func_B() { return nullptr; }"
                , "typedef int* func_ptr_ret_cb(void);"
                , "void test_return_values(int cond) {"
                , "  func_ptr_ret_cb* fp;"
                , "  if (cond) { fp = func_A; } else { fp = func_B; }"
                , "  int* my_ptr = fp();"
                , "}"
                ]
            let my_ptr_id = findSid "my_ptr" ast
            let expectedHeapLoc = HeapLoc "test.c:2:30"
            let expectedVarMap = Map.singleton my_ptr_id (Set.fromList [NullLoc, expectedHeapLoc])
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton my_ptr_id) `shouldBe` expectedVarMap

        it "should handle indirect calls via a function pointer in a struct" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void* malloc(unsigned long size);"
                , "void func_A(int** p) { *p = (int*)malloc(sizeof(int)); }"
                , "void func_B(int** p) { *p = nullptr; }"
                , "typedef void func_ptr_cb(int** p);"
                , "struct Dispatcher { func_ptr_cb* action; };"
                , "void test_struct_field_call(int cond) {"
                , "  struct Dispatcher d;"
                , "  if (cond) { d.action = func_A; } else { d.action = func_B; }"
                , "  int* my_ptr = nullptr;"
                , "  d.action(&my_ptr);"
                , "}"
                ]
            let my_ptr_id = findSid "my_ptr" ast
            let expectedHeapLoc = HeapLoc "test.c:2:35"
            let expectedVarMap = Map.singleton my_ptr_id (Set.fromList [NullLoc, expectedHeapLoc])
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton my_ptr_id) `shouldBe` expectedVarMap

        it "should handle aliasing of union fields" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "union My_Union { int* x; int* y; };"
                , "void test() {"
                , "  union My_Union u;"
                , "  int a;"
                , "  int b;"
                , "  u.x = &a;"
                , "  u.y = &b; // This overwrites the memory location of u.x"
                , "  int* p = u.x; // Should point to &b now"
                , "}"
                ]
            let u_id = findSid "u" ast
            let b_id = findSid "b" ast
            let p_id = findSid "p" ast
            let u_loc = StackLoc u_id
            let field_x_loc = FieldLoc u_loc "x"
            let field_y_loc = FieldLoc u_loc "y"
            let b_loc = StackLoc b_id

            -- A sound analysis must know that u.x and u.y alias.
            -- After `u.y = &b`, a load from `u.x` should yield `&b`.
            let expected_b_locs = Set.singleton b_loc
            let expectedMemMap = Map.fromList
                    [ (field_x_loc, expected_b_locs)
                    , (field_y_loc, expected_b_locs)
                    ]
            let expectedVarMap = Map.singleton p_id expected_b_locs
            uninternFact pool finalFact `shouldBe` (expectedVarMap, expectedMemMap, Set.empty)

        it "should handle global pointers initialized with address-of" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "typedef const int* Const_Int_Ptr;"
                , "static const int g_a = 0;"
                , "static const Const_Int_Ptr g_p = &g_a;"
                , "void test() {"
                , "  const int* p = g_p;"
                , "}"
                ]
            let p_id = findSid "p" ast
            let g_a_id = findSid "g_a" ast

            -- The analysis should initialize g_p to point to the global 'g_a'.
            -- Then inside test, p should be assigned the points-to set of g_p.
            let expectedVarMap = Map.singleton p_id (Set.singleton (GlobalVarLoc g_a_id))
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton p_id) `shouldBe` expectedVarMap

        it "should be sound for intra-procedural #ifdef" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void test() {"
                , "  int a;"
                , "  int b;"
                , "  int* p = &a;"
                , "#ifdef SOME_FLAG"
                , "  p = &b;"
                , "#endif /* SOME_FLAG */"
                , "}"
                ]
            let p_id = findSid "p" ast
            let a_id = findSid "a" ast
            let b_id = findSid "b" ast
            -- A sound analysis must consider both paths of the #ifdef.
            let expectedVarMap = Map.singleton p_id (Set.fromList [StackLoc a_id, StackLoc b_id])
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton p_id) `shouldBe` expectedVarMap

        it "should track pointers through struct fields for v-table resolution" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void *calloc(unsigned int nmemb, unsigned int size);"
                , "typedef void *mem_calloc_cb(void *obj, unsigned int nmemb, unsigned int size);"
                , "typedef struct Memory_Funcs { mem_calloc_cb *calloc; } Memory_Funcs;"
                , "typedef struct Memory { const Memory_Funcs *funcs; void *obj; } Memory;"
                , "void *sys_calloc(void *obj, unsigned int nmemb, unsigned int size) { return calloc(nmemb, size); }"
                , "static const Memory_Funcs os_memory_funcs = { sys_calloc };"
                , "static const Memory os_memory_obj = { &os_memory_funcs, 0 };"
                , "const Memory *os_memory(void) { return &os_memory_obj; }"
                , "void *mem_valloc(const Memory *mem, unsigned int nmemb, unsigned int size) {"
                , "  return mem->funcs->calloc(mem->obj, nmemb, size);"
                , "}"
                , "struct DHT { const Memory *mem; int* data; };"
                , "void do_allocation(struct DHT* dht) {"
                , "  dht->data = (int*)mem_valloc(dht->mem, 1, sizeof(int));"
                , "}"
                , "void test() {"
                , "  struct DHT d;"
                , "  d.mem = os_memory();"
                , "  do_allocation(&d);"
                , "}"
                ]
            let d_id = findSid "d" ast
            let d_loc = StackLoc d_id
            let data_field_loc = FieldLoc d_loc "data"
            -- This is the allocation site of the call to `calloc` inside `sys_calloc`.
            -- The analysis should be able to resolve the entire chain of calls and pointers.
            let expectedHeapLoc = HeapLoc "test.c:5:77"
            let (_, finalMemMap, _) = uninternFact pool finalFact
            let resultPointsToSet = fromMaybe Set.empty (Map.lookup data_field_loc finalMemMap)
            resultPointsToSet `shouldBe` Set.singleton expectedHeapLoc

        it "should soundly handle pointers escaping their local scope" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "int* get_local_ptr() {"
                , "  int x;"
                , "  return &x;"
                , "}"
                , "void test_escape() {"
                , "  int* p = get_local_ptr();"
                , "}"
                ]
            let p_id = findSid "p" ast
            let x_id = findSid "x" ast
            let expectedVarMap = Map.singleton p_id (Set.singleton (StackLoc x_id))
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton p_id) `shouldBe` expectedVarMap

        it "should handle storing to a nested struct field" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "struct Inner { int* p; };"
                , "struct Outer { struct Inner i; };"
                , "void test() {"
                , "  struct Outer o;"
                , "  int a;"
                , "  o.i.p = &a;"
                , "}"
                ]
            let o_id = findSid "o" ast
            let a_id = findSid "a" ast
            let o_loc = StackLoc o_id
            let i_loc = FieldLoc o_loc "i"
            let p_loc = FieldLoc i_loc "p"
            let a_loc = StackLoc a_id
            let expectedMemMap = Map.singleton p_loc (Set.singleton a_loc)
            uninternFact pool finalFact `shouldBe` (Map.empty, expectedMemMap, Set.empty)

        it "should transfer heap locs out of functions when returning local vars" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "typedef struct My_Struct { int a; } My_Struct;"
                , "void* calloc(unsigned int nmemb, unsigned int size);"
                , "My_Struct *new_my_struct() {"
                , "  My_Struct *out_key = (My_Struct *)calloc(1, sizeof(My_Struct));"
                , "  return out_key;"
                , "}"
                , "void test() {"
                , "  My_Struct* k = new_my_struct();"
                , "}"
                ]
            let k_id = findSid "k" ast
            let expectedHeapLoc = HeapLoc "test.c:4:37"
            -- We should actually be returning nullptr or heap from calloc, but
            -- our current external summary only returns heap (i.e. it never
            -- fails). We should fix that at some point for full null safety.
            let expectedVarMap = Map.singleton k_id (Set.singleton expectedHeapLoc)
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton k_id) `shouldBe` expectedVarMap

        it "should return UnknownLoc for unsummarized external function calls" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "typedef struct Tox_Pass_Key Tox_Pass_Key;"
                , "Tox_Pass_Key *tox_pass_key_derive_with_salt(); // External, unsummarized"
                , "void test() {"
                , "  Tox_Pass_Key *key = tox_pass_key_derive_with_salt();"
                , "}"
                ]
            let key_id = findSid "key" ast
            let expectedVarMap = Map.singleton key_id (Set.singleton UnknownLoc)
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.singleton key_id) `shouldBe` expectedVarMap

        it "should converge on a program with a data-dependent feedback loop" $ do
            (ast, finalFact, pool) <- runPointsToAnalysis
                [ "void update_ptr(int** p, int* v) {"
                , "  *p = v;"
                , "}"
                , "void intermediate(int** p, int* v) {"
                , "  update_ptr(p, v);"
                , "}"
                , "void test() {"
                , "  int a; int b;"
                , "  int* p1 = &a;"
                , "  int* p2 = &b;"
                , "  intermediate(&p1, p2);"
                , "  intermediate(&p2, p1);"
                , "}"
                ]
            let p1_id = findSid "p1" ast
            let p2_id = findSid "p2" ast
            let b_id = findSid "b" ast

            -- After `intermediate(&p1, p2)`, p1 points to b.
            -- After `intermediate(&p2, p1)`, p1 still points to b, so p2 is made to point to b.
            let expectedVarMap = Map.fromList
                    [ (p1_id, Set.singleton (StackLoc b_id))
                    , (p2_id, Set.singleton (StackLoc b_id))
                    ]
            let (vm, _, _) = uninternFact pool finalFact
            (vm `Map.restrictKeys` Set.fromList [p1_id, p2_id]) `shouldBe` expectedVarMap
