{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Analysis.VTableSpec where

import           Control.Monad              (msum)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Language.Cimple            as C
import           Test.Hspec                 (Spec, describe, it, shouldBe)
import           Tokstyle.Analysis.Scope    (ScopeState (..), ScopedId (..),
                                             runScopePass)
import           Tokstyle.Analysis.VTable   (VTableMap, resolveVTables)
import           Tokstyle.Common.TypeSystem (collect)
import           Tokstyle.LinterSpec        (mustParse)

-- Helper to run all preprocessing steps
runSetup :: [Text] -> IO (ScopeState, [C.Node (C.Lexeme ScopedId)], VTableMap)
runSetup code = do
    parsed <- mustParse code
    let typeSystem = collect [("test.c", parsed)]
    let (scopedAst, scopeState) = runScopePass parsed
    let vtableMap = resolveVTables scopedAst typeSystem
    return (scopeState, scopedAst, vtableMap)

findSidInState :: Text -> ScopeState -> ScopedId
findSidInState name st = fromMaybe (error $ "SID not found for: " ++ show name) $
    msum $ map (Map.lookup name) (ssTable st)

spec :: Spec
spec = do
    describe "VTable Resolution" $ do
        it "should resolve the os_memory_funcs v-table correctly" $ do
            (scopeState, _, vtableMap) <- runSetup
                [ "typedef void *mem_malloc_cb(void *obj, unsigned int size);"
                , "typedef void *mem_calloc_cb(void *obj, unsigned int nmemb, unsigned int size);"
                , "typedef void *mem_realloc_cb(void *obj, void *ptr, unsigned int size);"
                , "typedef void mem_free_cb(void *obj, void *ptr);"
                , "typedef struct Memory_Funcs {"
                , "    mem_malloc_cb *malloc;"
                , "    mem_calloc_cb *calloc;"
                , "    mem_realloc_cb *realloc;"
                , "    mem_free_cb *free;"
                , "} Memory_Funcs;"
                , "static void *sys_malloc(void *obj, unsigned int size) { return 0; }"
                , "static void *sys_calloc(void *obj, unsigned int nmemb, unsigned int size) { return 0; }"
                , "static void *sys_realloc(void *obj, void *ptr, unsigned int size) { return 0; }"
                , "static void sys_free(void *obj, void *ptr) { return; }"
                , "static const Memory_Funcs os_memory_funcs = {"
                , "    (mem_malloc_cb *)sys_malloc,"
                , "    sys_calloc,"
                , "    sys_realloc,"
                , "    sys_free,"
                , "};"
                ]

            let osMemFuncsSid = findSidInState "os_memory_funcs" scopeState
            let sysMallocSid  = findSidInState "sys_malloc" scopeState
            let sysCallocSid  = findSidInState "sys_calloc" scopeState
            let sysReallocSid = findSidInState "sys_realloc" scopeState
            let sysFreeSid    = findSidInState "sys_free" scopeState

            let expectedMap = Map.singleton osMemFuncsSid $ Map.fromList
                    [ ("malloc", sysMallocSid)
                    , ("calloc", sysCallocSid)
                    , ("realloc", sysReallocSid)
                    , ("free", sysFreeSid)
                    ]

            vtableMap `shouldBe` expectedMap

        it "should ignore structs with non-function-pointer fields" $ do
            (_, _, vtableMap) <- runSetup
                [ "typedef void fun_cb(void);"
                , "struct NotAVTable {"
                , "  int x;"
                , "  fun_cb *f;"
                , "};"
                , "void my_func() {return;}"
                , "const struct NotAVTable my_var = { 1, my_func };"
                ]
            vtableMap `shouldBe` Map.empty

        it "should ignore local variables" $ do
            (_, _, vtableMap) <- runSetup
                [ "typedef void fun_cb(void);"
                , "typedef struct VTable { fun_cb *f; } VTable;"
                , "void my_func() {return;}"
                , "void main() {"
                , "  const VTable my_local_vtable = { my_func };"
                , "}"
                ]
            vtableMap `shouldBe` Map.empty
