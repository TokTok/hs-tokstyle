{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Analysis.CallGraphSpec where

import           Data.Fix                    (Fix (..))
import           Data.List.NonEmpty          (NonEmpty (..))
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import qualified Language.Cimple             as C
import           Test.Hspec
import           Tokstyle.Analysis.CallGraph
import           Tokstyle.Analysis.Types     (AbstractLocation (..),
                                              CallSite (..), CallType (..),
                                              FunctionName, PointsToMap,
                                              getCallers, lookupOrError)
import           Tokstyle.LinterSpec         (mustParse)

-- Helper to create a CallSite set for a direct call. We don't care about the
-- exact NodeId in most tests, so we just check that there is one.
directCallSite :: Set CallSite
directCallSite = Set.singleton (CallSite 0 DirectCall)

isDirectCall :: Set CallSite -> Bool
isDirectCall sites = not (Set.null sites) && all ((== DirectCall) . csCallType) sites

isIndirectCall :: Set CallSite -> Bool
isIndirectCall sites = not (Set.null sites) && all ((== IndirectCall) . csCallType) sites

spec :: Spec
spec = do
    describe "CallGraph Construction" $ do
        it "should build a graph for a simple direct call" $ do
            ast <- mustParse
                [ "void g() { return; }"
                , "void f() { g(); }"
                ]
            let graph = buildCallGraph [("test.c", ast)] Map.empty
            let fCallees = getCallees graph "f"
            Map.size fCallees `shouldBe` 1
            isDirectCall (fCallees Map.! "g") `shouldBe` True

        it "should handle a function with no calls" $ do
            ast <- mustParse
                [ "void f() { int x = 1; }"
                ]
            let graph = buildCallGraph [("test.c", ast)] Map.empty
            graph `shouldBe` Map.empty

        it "should handle multiple distinct calls" $ do
            ast <- mustParse
                [ "void g() { return; }"
                , "void h() { return; }"
                , "void f() { g(); h(); }"
                ]
            let graph = buildCallGraph [("test.c", ast)] Map.empty
            let fCallees = getCallees graph "f"
            Map.size fCallees `shouldBe` 2
            isDirectCall (fCallees Map.! "g") `shouldBe` True
            isDirectCall (fCallees Map.! "h") `shouldBe` True

        it "should create two distinct CallSites for multiple calls to the same function" $ do
            ast <- mustParse
                [ "void g() { return; }"
                , "void f() { g(); g(); }"
                ]
            let graph = buildCallGraph [("test.c", ast)] Map.empty
            let fCallees = getCallees graph "f"
            Map.size fCallees `shouldBe` 1
            -- The set of call sites for 'g' should contain two distinct CallSite objects
            -- because they originate from different AST nodes.
            Set.size (fCallees Map.! "g") `shouldBe` 2

        it "should identify an indirect call" $ do
            ast <- mustParse
                [ "typedef void empty_cb(const char* message);"
                , "void f(empty_cb *func) { func(); }"
                ]
            let graph = buildCallGraph [("test.c", ast)] Map.empty
            let fCallees = getCallees graph "f"
            Map.size fCallees `shouldBe` 1
            isIndirectCall (fCallees Map.! "func") `shouldBe` True

        it "should build a graph for nested calls" $ do
            ast <- mustParse
                [ "void h() { return; }"
                , "void g() { h(); }"
                , "void f() { g(); }"
                ]
            let graph = buildCallGraph [("test.c", ast)] Map.empty
            let fCallees = getCallees graph "f"
            let gCallees = getCallees graph "g"
            isDirectCall (fCallees Map.! "g") `shouldBe` True
            isDirectCall (gCallees Map.! "h") `shouldBe` True

        it "should handle direct recursion" $ do
            ast <- mustParse
                [ "void f() { f(); }"
                ]
            let graph = buildCallGraph [("test.c", ast)] Map.empty
            let fCallees = getCallees graph "f"
            isDirectCall (fCallees Map.! "f") `shouldBe` True

        it "should handle mutual recursion" $ do
            ast <- mustParse
                [ "void g();"
                , "void f() { g(); }"
                , "void g() { f(); }"
                ]
            let graph = buildCallGraph [("test.c", ast)] Map.empty
            let fCallees = getCallees graph "f"
            let gCallees = getCallees graph "g"
            isDirectCall (fCallees Map.! "g") `shouldBe` True
            isDirectCall (gCallees Map.! "f") `shouldBe` True

        it "should capture a non-zero NodeId for a call site" $ do
            ast <- mustParse
                [ "void g() { return; }"
                , "void f() { g(); }"
                ]
            let graph = buildCallGraph [("test.c", ast)] Map.empty
            let fCallees = getCallees graph "f"
            let gCallSites = lookupOrError "CallGraphSpec" fCallees "g"
            Set.size gCallSites `shouldBe` 1
            let actualNodeId = csNodeId (Set.elemAt 0 gCallSites)
            actualNodeId `shouldNotBe` 0

    describe "Inter-procedural CallGraph Construction" $ do
        it "should resolve an indirect call via a simple function pointer" $ do
            ast <- mustParse
                [ "typedef void empty_cb(const char* message);"
                , "void g() { return; }"
                , "void f(empty_cb *func) { func(); }"
                ]
            -- Simulate that points-to analysis has determined `func` points to `g`
            let pointsToMap = Map.singleton (VarLocation "func") (Set.singleton (FunctionLocation "g"))
            let graph = buildCallGraph [("test.c", ast)] pointsToMap
            let fCallees = getCallees graph "f"
            isIndirectCall (fCallees Map.! "g") `shouldBe` True

        it "should resolve an indirect call via a struct function pointer" $ do
            ast <- mustParse
                [ "typedef void sender_cb(const char* message);"
                , "typedef struct Dispatcher { sender_cb *sender_callback; } Dispatcher;"
                , "void network_send(const char* message);"
                , "void process(Dispatcher* d) { d->sender_callback(\"hello\"); }"
                ]
            -- Simulate that d->sender_callback points to network_send
            let pointsToMap = Map.singleton
                    (FieldLocation (DerefLocation (VarLocation "d")) "sender_callback")
                    (Set.singleton (FunctionLocation "network_send"))
            let graph = buildCallGraph [("test.c", ast)] pointsToMap
            let pCallees = getCallees graph "process"
            isIndirectCall (pCallees Map.! "network_send") `shouldBe` True

        it "should create edges to all potential targets of a function pointer" $ do
            ast <- mustParse
                [ "typedef void empty_cb(const char* message);"
                , "void g() { return; }"
                , "void h() { return; }"
                , "void f(empty_cb *func) { func(); }"
                ]
            -- Simulate that func can point to either g or h
            let pointsToMap = Map.singleton (VarLocation "func") (Set.fromList [FunctionLocation "g", FunctionLocation "h"])
            let graph = buildCallGraph [("test.c", ast)] pointsToMap
            let fCallees = getCallees graph "f"
            Map.size fCallees `shouldBe` 2
            isIndirectCall (fCallees Map.! "g") `shouldBe` True
            isIndirectCall (fCallees Map.! "h") `shouldBe` True

    describe "CallGraph Queries" $ do
        it "getCallees should return correct functions" $ do
            ast <- mustParse
                [ "void h() { return; }"
                , "void g() { h(); }"
                , "void f() { g(); h(); }"
                ]
            let graph = buildCallGraph [("test.c", ast)] Map.empty

            let fCallees = getCallees graph "f"
            let gCallees = getCallees graph "g"
            let hCallees = getCallees graph "h"

            Map.size fCallees `shouldBe` 2
            isDirectCall (fCallees Map.! "g") `shouldBe` True
            isDirectCall (fCallees Map.! "h") `shouldBe` True
            isDirectCall (gCallees Map.! "h") `shouldBe` True
            hCallees `shouldBe` Map.empty

        it "getCallers should return correct functions" $ do
            ast <- mustParse
                [ "void h() { return; }"
                , "void g() { h(); }"
                , "void f() { g(); h(); }"
                ]
            let graph = buildCallGraph [("test.c", ast)] Map.empty

            let fCallers = getCallers graph "f"
            let gCallers = getCallers graph "g"
            let hCallers = getCallers graph "h"

            fCallers `shouldBe` Map.empty
            isDirectCall (gCallers Map.! "f") `shouldBe` True
            Map.size hCallers `shouldBe` 2
            isDirectCall (hCallers Map.! "f") `shouldBe` True
            isDirectCall (hCallers Map.! "g") `shouldBe` True
