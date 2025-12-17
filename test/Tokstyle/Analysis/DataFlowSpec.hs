{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Tokstyle.Analysis.DataFlowSpec where

import           Control.Monad              (foldM)
import           Control.Monad.Identity     (Identity (..), runIdentity)
import           Data.Fix                   (Fix (..))
import           Data.List                  (find)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Language.Cimple            (NodeF (..))
import qualified Language.Cimple            as C
import           Language.Cimple.Pretty     (showNode, showNodePlain)
import           Test.Hspec                 (Spec, describe, expectationFailure,
                                             it, pendingWith, shouldBe)
import           Text.Groom                 (groom)
import           Tokstyle.Analysis.DataFlow
import           Tokstyle.LinterSpec        (mustParse)

-- | A simple "Reaching Definitions" analysis.
data ReachingDefs = ReachingDefs (Map Text (Set Text))
    deriving (Eq, Show)

data Empty l = Empty

empty :: Empty Text
empty = Empty

instance DataFlow Identity Empty Text ReachingDefs () where
    emptyFacts _ = return $ ReachingDefs Map.empty
    join _ (ReachingDefs a) (ReachingDefs b) = return $ ReachingDefs (Map.unionWith Set.union a b)
    transfer _ _ _ (ReachingDefs facts) (Fix (C.ExprStmt (Fix (C.AssignExpr (Fix (C.VarExpr (C.L _ _ name))) _ rhs)))) =
        return (ReachingDefs $ Map.insert name (evalExpr rhs facts) facts, Set.empty)
    transfer _ _ _ (ReachingDefs facts) (Fix (C.VarDeclStmt (Fix (C.VarDecl _ (C.L _ _ name) _)) (Just rhs))) =
        return (ReachingDefs $ Map.insert name (evalExpr rhs facts) facts, Set.empty)
    transfer _ _ _ (ReachingDefs facts) (Fix (C.VarDeclStmt (Fix (C.VarDecl _ (C.L _ _ name) _)) Nothing)) =
        return (ReachingDefs $ Map.insert name (Set.singleton "uninitialized") facts, Set.empty)
    transfer _ _ _ facts _ = return (facts, Set.empty)

evalExpr :: C.Node (C.Lexeme Text) -> Map Text (Set Text) -> Set Text
evalExpr (Fix (C.VarExpr (C.L _ _ name))) facts = fromMaybe (Set.singleton "uninitialized") (Map.lookup name facts)
evalExpr (Fix (C.BinaryExpr lhs _ rhs)) facts = Set.union (evalExpr lhs facts) (evalExpr rhs facts)
evalExpr (Fix (C.AssignExpr _ _ rhs)) facts = evalExpr rhs facts
evalExpr (Fix (C.ParenExpr e)) facts = evalExpr e facts
evalExpr (Fix (C.CastExpr _ e)) facts = evalExpr e facts
evalExpr (Fix (C.TernaryExpr _ t e)) facts = Set.union (evalExpr t facts) (evalExpr e facts)
evalExpr (Fix (C.LiteralExpr _ (C.L _ _ val))) _ = Set.singleton val
evalExpr _ _ = Set.singleton "literal"

data StatementCoverage = StatementCoverage (Set Text)
    deriving (Eq, Show)

instance DataFlow Identity Empty Text StatementCoverage () where
    emptyFacts _ = return $ StatementCoverage Set.empty
    join _ (StatementCoverage a) (StatementCoverage b) = return $ StatementCoverage (Set.union a b)
    transfer _ _ _ (StatementCoverage facts) stmt =
        if "__tokstyle_assume" `Text.isPrefixOf` showNodePlain stmt
        then return (StatementCoverage facts, Set.empty)
        else return (StatementCoverage $ Set.insert (showNodePlain stmt) facts, Set.empty)

-- | Find the unique exit node of a CFG.
findExitNodeId :: CFG Text a -> Int
findExitNodeId cfg =
    case filter (null . cfgSuccs) (Map.elems cfg) of
        [node] -> cfgNodeId node
        nodes  -> error $ "Expected 1 exit node, but found " ++ show (length nodes)

-- | Find a node in the CFG by a statement it contains.
findNodeIdByStmt :: (C.Node (C.Lexeme Text) -> Bool) -> CFG Text a -> Int
findNodeIdByStmt predicate cfg =
    case find (\n -> any predicate (cfgStmts n)) (Map.elems cfg) of
        Just node -> cfgNodeId node
        Nothing   -> error "findNodeIdByStmt: could not find node with matching statement"

-- | Find the 'else' branch of an 'if' statement that contains a given statement in its 'then' branch.
findElseNodeIdOfIfContainingStmt :: (C.Node (C.Lexeme Text) -> Bool) -> CFG Text a -> Int
findElseNodeIdOfIfContainingStmt predicate cfg =
    let thenNodeId = findNodeIdByStmt predicate cfg
        thenNode = fromJust (Map.lookup thenNodeId cfg)
        ifNodeId = case cfgPreds thenNode of
            [p] -> p
            _   -> error "Expected one predecessor for then-branch"
        ifNode = fromJust (Map.lookup ifNodeId cfg)
        elseNodeId = case cfgSuccs ifNode of
            [s1, s2] -> if s1 == thenNodeId then s2 else s1
            _        -> error "Expected two successors for if-node"
    in elseNodeId



spec :: Spec
spec = do
    describe "Reaching Definitions" $ do
        it "should calculate the reaching definitions for a simple function" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  int y = 2;"
                , "  x = y;"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["2"]), ("y", Set.fromList ["2"])])

        it "should calculate the reaching definitions for an if statement" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  if (x > 0) {"
                , "    x = 2;"
                , "  } else {"
                , "    x = 3;"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["2", "3"])])

        it "should calculate the reaching definitions for a while loop" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  while (x < 10) {"
                , "    x = x + 1;"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["1"])])

        it "should calculate the reaching definitions for a function with a return statement" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  return;"
                , "  x = 2;"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["1"])])

        it "should calculate the reaching definitions for nested if statements" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  if (x > 0) {"
                , "    if (x > 1) {"
                , "      x = 2;"
                , "    } else {"
                , "      x = 3;"
                , "  }"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["1", "2", "3"])])

        it "should calculate the reaching definitions for a while loop with a nested if statement" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  while (x < 10) {"
                , "    if (x < 5) {"
                , "      x = x + 1;"
                , "    } else {"
                , "      x = x + 2;"
                , "    }"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let isAssignXPlus2 = \case
                    Fix (C.ExprStmt (Fix (C.AssignExpr (Fix (C.VarExpr (C.L _ _ "x"))) C.AopEq (Fix (C.BinaryExpr (Fix (C.VarExpr (C.L _ _ x'))) C.BopPlus (Fix (C.LiteralExpr C.Int (C.L _ _ "2")))))))) | ("x"::Text) == x' -> True
                    _ -> False
            let finalNodeId = findNodeIdByStmt isAssignXPlus2 finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["1", "2"])])

        it "should calculate the reaching definitions for a for loop" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 0;"
                , "  for (int i = 0; i < 10; ++i) {"
                , "    x = x + 1;"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["0", "1"]), ("i", Set.fromList ["0"])])

        it "should calculate the reaching definitions for a do-while loop" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  do {"
                , "    x = x + 1;"
                , "  } while (x < 10);"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let isAssignXPlus1 = \case
                    Fix (C.ExprStmt (Fix (C.AssignExpr (Fix (C.VarExpr (C.L _ _ "x"))) C.AopEq (Fix (C.BinaryExpr (Fix (C.VarExpr (C.L _ _ x'))) C.BopPlus (Fix (C.LiteralExpr C.Int (C.L _ _ "1")))))))) | ("x"::Text) == x' -> True
                    _ -> False
            let finalNodeId = findNodeIdByStmt isAssignXPlus1 finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["1"])])

        it "should calculate the reaching definitions for a for loop with a break statement" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 0;"
                , "  for (int i = 0; i < 10; ++i) {"
                , "    if (i == 5) {"
                , "      break;"
                , "    }"
                , "    x = x + 1;"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let isBreak = \case Fix C.Break -> True; _ -> False
            let finalNodeId = findElseNodeIdOfIfContainingStmt isBreak finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["0", "1"]), ("i", Set.fromList ["0"])])

        it "should calculate the reaching definitions for a variable assigned to another variable" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  int y = x;"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["1"]), ("y", Set.fromList ["1"])])

        it "should calculate the reaching definitions for a switch statement" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  int y = 1;"
                , "  switch (x) {"
                , "    case 1: {"
                , "      y = 2;"
                , "      break;"
                , "    }"
                , "    case 2: {"
                , "      y = 3;"
                , "      break;"
                , "    }"
                , "    default: {"
                , "      y = 4;"
                , "      break;"
                , "    }"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["1"]), ("y", Set.fromList ["2", "3", "4"])])

        it "should calculate the reaching definitions for a ternary operator" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  int y = (x > 0) ? 2 : 3;"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["1"]), ("y", Set.fromList ["2", "3"])])

        it "should calculate the reaching definitions for a goto statement" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  goto HANDLE_ERROR;"
                , "  x = 2;"
                , "HANDLE_ERROR:"
                , "  x = 3;"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["3"])])

        it "should calculate the reaching definitions for a while loop with a continue statement" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 0;"
                , "  int i = 0;"
                , "  while (i < 10) {"
                , "    i = i + 1;"
                , "    if (i % 2 == 0) {"
                , "      continue;"
                , "    }"
                , "    x = i;"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["0", "1"]), ("i", Set.fromList ["0", "1"])])

        it "should calculate the reaching definitions for a switch statement with fall-through" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  int y = 1;"
                , "  switch (x) {"
                , "    case 1: {"
                , "      y = 2;"
                , "    }"
                , "    case 2: {"
                , "      y = 3;"
                , "      break;"
                , "    }"
                , "    default: {"
                , "      y = 4;"
                , "      break;"
                , "    }"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["1"]), ("y", Set.fromList ["3", "4"])])

        it "should calculate the reaching definitions for a while loop with a break statement" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 0;"
                , "  while (x < 10) {"
                , "    if (x == 5) {"
                , "      break;"
                , "    }"
                , "    x = x + 1;"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["0", "1"])])

        it "should calculate the reaching definitions for a switch statement with only a default case" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  int y = 1;"
                , "  switch (x) {"
                , "    default: {"
                , "      y = 4;"
                , "      break;"
                , "    }"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["1"]), ("y", Set.fromList ["4"])])

        it "should calculate the reaching definitions for a switch statement with all cases falling through" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  int y = 1;"
                , "  switch (x) {"
                , "    case 1: {"
                , "      y = 2;"
                , "    }"
                , "    case 2: {"
                , "      y = 3;"
                , "    }"
                , "    default: {"
                , "      y = 4;"
                , "    }"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["1"]), ("y", Set.fromList ["4"])])

        it "should correctly handle unreachable code after a return statement" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  return;"
                , "  x = 2;"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let isAssignX2 = \case
                    Fix (C.ExprStmt (Fix (C.AssignExpr (Fix (C.VarExpr (C.L _ _ "x"))) _ (Fix (C.LiteralExpr C.Int (C.L _ _ "2")))))) -> True
                    _ -> False
            let nodeExists = any (\n -> any isAssignX2 (cfgStmts n)) (Map.elems finalCfg)
            nodeExists `shouldBe` False

        it "should handle a switch statement with no default case" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  int y = 1;"
                , "  switch (x) {"
                , "    case 1: {"
                , "      y = 2;"
                , "      break;"
                , "    }"
                , "    case 2: {"
                , "      y = 3;"
                , "      break;"
                , "    }"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["1"]), ("y", Set.fromList ["1", "2", "3"])])

        it "should handle a switch statement where a case falls through to default" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  int y = 1;"
                , "  switch (x) {"
                , "    case 1: {"
                , "      y = 2;"
                , "    }"
                , "    default: {"
                , "      y = 4;"
                , "      break;"
                , "    }"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["1"]), ("y", Set.fromList ["4"])])

    it "should include preprocessor directives in the CFG" $ do
        ast <- mustParse
            [ "void f() {"
            , "  #define MY_MACRO(x) do { int abc = 0; } while (0)" -- the do {} must not be empty.
            , "  int y = 1;"
            , "  #undef MY_MACRO"
            , "}"
            ]
        let funcBody = head ast
        let stmts = case unFix funcBody of C.FunctionDefn _ _ (Fix (C.CompoundStmt s)) -> s; _ -> []
        let cfg = runIdentity $ buildCFG empty funcBody (runIdentity $ emptyFacts empty) :: CFG Text StatementCoverage
        let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
        let (StatementCoverage finalFacts) = runIdentity $ foldM (join empty) (runIdentity $ emptyFacts empty) (map cfgOutFacts (Map.elems finalCfg))

        let findStmtRecursive predicate _stmts' =
                let find' _ [] = Nothing
                    find' p (s:ss) =
                        if p s then Just s
                        else case unFix s of
                            C.PreprocScopedDefine def body undef ->
                                case find' p [def] of
                                    Just found -> Just found
                                    Nothing -> case find' p body of
                                        Just found' -> Just found'
                                        Nothing     -> find' p [undef]
                            _ -> find' p ss
                in fromMaybe (error "Could not find statement in parsed AST") (find' predicate stmts)

        let defineStmt = findStmtRecursive (\s -> case unFix s of C.PreprocDefineMacro {} -> True; _ -> False) stmts
        let undefStmt = findStmtRecursive (\s -> case unFix s of C.PreprocUndef {} -> True; _ -> False) stmts

        Set.member (showNodePlain defineStmt) finalFacts `shouldBe` True
        Set.member (showNodePlain undefStmt) finalFacts `shouldBe` True

    it "should calculate the reaching definitions for a backward goto statement creating a loop" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 0;"
                , "LOOP_START:"
                , "  x = x + 1;"
                , "  if (x < 5) {"
                , "    goto LOOP_START;"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let isAssignXPlus1 = \case
                    Fix (C.ExprStmt (Fix (C.AssignExpr (Fix (C.VarExpr (C.L _ _ "x"))) C.AopEq (Fix (C.BinaryExpr (Fix (C.VarExpr (C.L _ _ x'))) C.BopPlus (Fix (C.LiteralExpr C.Int (C.L _ _ "1")))))))) | ("x"::Text) == x' -> True
                    _ -> False
            let assignNodeId = findNodeIdByStmt isAssignXPlus1 finalCfg
            let assignNode = fromJust (Map.lookup assignNodeId finalCfg)
            -- The input to the `x = x + 1` node should contain definitions from both the initial assignment and the previous iteration.
            let (ReachingDefs inFacts) = cfgInFacts assignNode
            inFacts `shouldBe` Map.fromList [("x", Set.fromList ["0", "1"])]

    it "should handle multiple case labels for a single block" $ do
        ast <- mustParse
            [ "void f() {"
            , "  int y = 0;"
            , "  int x = 1;"
            , "  switch (x) {"
            , "    case 1:"
            , "    case 2: {"
            , "      y = 10;"
            , "      break;"
            , "    }"
            , "    case 3: {"
            , "      y = 20;"
            , "      break;"
            , "    }"
            , "  }"
            , "}"
            ]
        let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
        let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
        let finalNodeId = findExitNodeId finalCfg
        let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
        finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["1"]), ("y", Set.fromList ["0", "10", "20"])])

    it "should handle a switch statement inside a loop" $ do
        ast <- mustParse
            [ "void f() {"
            , "  int x = 0;"
            , "  int y = 0;"
            , "  while (x < 10) {"
            , "    x = x + 1;"
            , "    switch (x) {"
            , "      case 5: {"
            , "        y = 5;"
            , "        break;"
            , "      }"
            , "      default: {"
            , "        y = 1;"
            , "        break;"
            , "      }"
            , "    }"
            , "  }"
            , "}"
            ]
        let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
        let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
        let finalNodeId = findExitNodeId finalCfg
        let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
        finalFacts `shouldBe` ReachingDefs (Map.fromList [("x", Set.fromList ["0", "1"]), ("y", Set.fromList ["0", "1", "5"])])

    describe "Fixpoint Solver" $ do
        it "should solve a simple linear CFG" $ do
            let
                node0 = CFGNode 0 [] [1] [Fix (C.VarDeclStmt (Fix (C.VarDecl (Fix (C.TyStd (C.L (C.AlexPn 0 0 0) C.IdStdType "int"))) (C.L (C.AlexPn 0 0 0) C.IdVar "x") [])) Nothing)] (runIdentity $ emptyFacts empty) (runIdentity $ emptyFacts empty)
                node1 = CFGNode 1 [0] [2] [Fix (C.VarDeclStmt (Fix (C.VarDecl (Fix (C.TyStd (C.L (C.AlexPn 0 0 0) C.IdStdType "int"))) (C.L (C.AlexPn 0 0 0) C.IdVar "y") [])) Nothing)] (runIdentity $ emptyFacts empty) (runIdentity $ emptyFacts empty)
                node2 = CFGNode 2 [1] [] [] (runIdentity $ emptyFacts empty) (runIdentity $ emptyFacts empty)
                cfg = Map.fromList [(0, node0), (1, node1), (2, node2)] :: CFG Text ReachingDefs
                (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
                finalFacts0 = cfgOutFacts (fromJust (Map.lookup 0 finalCfg))
                finalFacts1 = cfgOutFacts (fromJust (Map.lookup 1 finalCfg))
            finalFacts0 `shouldBe` ReachingDefs (Map.fromList [("x", Set.singleton "uninitialized")])
            finalFacts1 `shouldBe` ReachingDefs (Map.fromList [("x", Set.singleton "uninitialized"), ("y", Set.singleton "uninitialized")])

        it "should solve a diamond-shaped CFG" $ do
            let
                node0 = CFGNode 0 [] [1, 2] [Fix (C.VarDeclStmt (Fix (C.VarDecl (Fix (C.TyStd (C.L (C.AlexPn 0 0 0) C.IdStdType "int"))) (C.L (C.AlexPn 0 0 0) C.IdVar "x") [])) Nothing)] (runIdentity $ emptyFacts empty) (runIdentity $ emptyFacts empty)
                node1 = CFGNode 1 [0] [3] [Fix (C.VarDeclStmt (Fix (C.VarDecl (Fix (C.TyStd (C.L (C.AlexPn 0 0 0) C.IdStdType "int"))) (C.L (C.AlexPn 0 0 0) C.IdVar "y") [])) Nothing)] (runIdentity $ emptyFacts empty) (runIdentity $ emptyFacts empty)
                node2 = CFGNode 2 [0] [3] [Fix (C.VarDeclStmt (Fix (C.VarDecl (Fix (C.TyStd (C.L (C.AlexPn 0 0 0) C.IdStdType "int"))) (C.L (C.AlexPn 0 0 0) C.IdVar "z") [])) Nothing)] (runIdentity $ emptyFacts empty) (runIdentity $ emptyFacts empty)
                node3 = CFGNode 3 [1, 2] [4] [] (runIdentity $ emptyFacts empty) (runIdentity $ emptyFacts empty)
                node4 = CFGNode 4 [3] [] [] (runIdentity $ emptyFacts empty) (runIdentity $ emptyFacts empty)
                cfg = Map.fromList [(0, node0), (1, node1), (2, node2), (3, node3), (4, node4)] :: CFG Text ReachingDefs
                (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
                finalFacts3 = cfgOutFacts (fromJust (Map.lookup 3 finalCfg))
            finalFacts3 `shouldBe` ReachingDefs (Map.fromList [("x", Set.singleton "uninitialized"), ("y", Set.singleton "uninitialized"), ("z", Set.singleton "uninitialized")])

        it "should propagate definitions through a binary expression" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int a = 1;"
                , "  int b = 2;"
                , "  int c = a + b;"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList
                [ ("a", Set.fromList ["1"])
                , ("b", Set.fromList ["2"])
                , ("c", Set.fromList ["1", "2"])
                ])

    it "should propagate facts between statements in the same basic block" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  int y = x;"
                , "  int z = y;"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let finalFacts = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            finalFacts `shouldBe` ReachingDefs (Map.fromList
                [ ("x", Set.fromList ["1"])
                , ("y", Set.fromList ["1"])
                , ("z", Set.fromList ["1"])
                ])

    describe "CFG Construction" $ do
        it "should initialize new CFG nodes with empty facts" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  if (x > 0) {"
                , "    int y = 2;"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let isDeclY = \case
                    Fix (C.VarDeclStmt (Fix (C.VarDecl _ (C.L _ _ "y") _)) _) -> True
                    _ -> False
            let thenNodeId = findNodeIdByStmt isDeclY cfg
            let thenNode = fromJust (Map.lookup thenNodeId cfg)
            cfgInFacts thenNode `shouldBe` runIdentity (emptyFacts empty)

        it "should join definitions from if/else branches" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x;"
                , "  if (1) {"
                , "    x = 1;"
                , "  } else {"
                , "    x = 2;"
                , "  }"
                , "}"
                ]
            let cfg = runIdentity $ buildCFG empty (head ast) (runIdentity $ emptyFacts empty) :: CFG Text ReachingDefs
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg

            let isAssignX1 = \case
                    Fix (C.ExprStmt (Fix (C.AssignExpr (Fix (C.VarExpr (C.L _ _ "x"))) _ (Fix (C.LiteralExpr C.Int (C.L _ _ "1")))))) -> True
                    _ -> False
            let isAssignX2 = \case
                    Fix (C.ExprStmt (Fix (C.AssignExpr (Fix (C.VarExpr (C.L _ _ "x"))) _ (Fix (C.LiteralExpr C.Int (C.L _ _ "2")))))) -> True
                    _ -> False

            let thenNodeId = findNodeIdByStmt isAssignX1 finalCfg
            let elseNodeId = findNodeIdByStmt isAssignX2 finalCfg
            let thenNode = fromJust (Map.lookup thenNodeId finalCfg)
            let elseNode = fromJust (Map.lookup elseNodeId finalCfg)

            case (cfgSuccs thenNode, cfgSuccs elseNode) of
                ([mergeNodeIdThen], [mergeNodeIdElse]) | mergeNodeIdThen == mergeNodeIdElse -> do
                    let mergeNode = fromJust (Map.lookup mergeNodeIdThen finalCfg)
                    let (ReachingDefs inFacts) = cfgInFacts mergeNode
                    Map.lookup "x" inFacts `shouldBe` Just (Set.fromList ["1", "2"])
                _ -> error "Could not find a unique merge node for the if/else branches"

        it "should solve a CFG with a loop" $ do
            let
                node0 = CFGNode 0 [] [1] [Fix (C.VarDeclStmt (Fix (C.VarDecl (Fix (C.TyStd (C.L (C.AlexPn 0 0 0) C.IdStdType "int"))) (C.L (C.AlexPn 0 0 0) C.IdVar "x") [])) Nothing)] (runIdentity $ emptyFacts empty) (runIdentity $ emptyFacts empty)
                node1 = CFGNode 1 [0, 2] [2, 3] [] (runIdentity $ emptyFacts empty) (runIdentity $ emptyFacts empty)
                node2 = CFGNode 2 [1] [1] [Fix (C.VarDeclStmt (Fix (C.VarDecl (Fix (C.TyStd (C.L (C.AlexPn 0 0 0) C.IdStdType "int"))) (C.L (C.AlexPn 0 0 0) C.IdVar "y") [])) Nothing)] (runIdentity $ emptyFacts empty) (runIdentity $ emptyFacts empty)
                node3 = CFGNode 3 [1] [] [] (runIdentity $ emptyFacts empty) (runIdentity $ emptyFacts empty)
                cfg = Map.fromList [(0, node0), (1, node1), (2, node2), (3, node3)] :: CFG Text ReachingDefs
                (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
                finalFacts1 = cfgOutFacts (fromJust (Map.lookup 1 finalCfg))
                finalFacts3 = cfgInFacts (fromJust (Map.lookup 3 finalCfg))
            finalFacts1 `shouldBe` ReachingDefs (Map.fromList [("x", Set.singleton "uninitialized"), ("y", Set.singleton "uninitialized")])
            finalFacts3 `shouldBe` ReachingDefs (Map.fromList [("x", Set.singleton "uninitialized"), ("y", Set.singleton "uninitialized")])

    describe "StatementCoverage Analysis" $ do
        it "should cover all reachable statements in a simple function" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  int y = 2;"
                , "  x = y;"
                , "}"
                ]
            let funcBody = head ast
            let stmts = case unFix funcBody of C.FunctionDefn _ _ (Fix (C.CompoundStmt s)) -> s; _ -> []
            let cfg = runIdentity $ buildCFG empty funcBody (runIdentity $ emptyFacts empty) :: CFG Text StatementCoverage
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let (StatementCoverage finalFacts) = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            let expectedStmts = Set.fromList $ map showNodePlain stmts
            finalFacts `shouldBe` expectedStmts

        it "should cover all reachable statements in a function with an if-else" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  if (x > 0) {"
                , "    x = 2;"
                , "  } else {"
                , "    x = 3;"
                , "  }"
                , "  int y = 4;"
                , "}"
                ]
            let funcBody = head ast
            let stmts = case unFix funcBody of C.FunctionDefn _ _ (Fix (C.CompoundStmt s)) -> s; _ -> []
            let cfg = runIdentity $ buildCFG empty funcBody (runIdentity $ emptyFacts empty) :: CFG Text StatementCoverage
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let (StatementCoverage finalFacts) = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            let expectedStmts = Set.fromList $ map showNodePlain (filter (\s -> case unFix s of C.IfStmt {} -> False; _ -> True) stmts)
            let cond = case stmts !! 1 of Fix (C.IfStmt c (Fix (C.CompoundStmt [s1])) (Just (Fix (C.CompoundStmt [s2])))) -> [showNodePlain c, showNodePlain s1, showNodePlain s2]; _ -> []
            finalFacts `shouldBe` Set.union expectedStmts (Set.fromList cond)

        it "should cover all reachable statements in a while loop" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  while (x < 10) {"
                , "    x = x + 1;"
                , "  }"
                , "}"
                ]
            let funcBody = head ast
            let stmts = case unFix funcBody of C.FunctionDefn _ _ (Fix (C.CompoundStmt s)) -> s; _ -> []
            let cfg = runIdentity $ buildCFG empty funcBody (runIdentity $ emptyFacts empty) :: CFG Text StatementCoverage
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let finalNodeId = findExitNodeId finalCfg
            let (StatementCoverage finalFacts) = cfgOutFacts (fromJust (Map.lookup finalNodeId finalCfg))
            let expectedStmts = Set.fromList $ map showNodePlain (filter (\s -> case unFix s of C.WhileStmt {} -> False; _ -> True) stmts)
            let whileStmt = fromJust $ find (\s -> case unFix s of C.WhileStmt {} -> True; _ -> False) stmts
            let (cond, bodyStmts) = case unFix whileStmt of C.WhileStmt c (Fix (C.CompoundStmt b)) -> (c, b); _ -> error "unexpected while loop structure"
            let expected = Set.union expectedStmts (Set.insert (showNodePlain cond) (Set.fromList (map showNodePlain bodyStmts)))
            finalFacts `shouldBe` expected

        it "should not cover unreachable statements" $ do
            ast <- mustParse
                [ "void f() {"
                , "  int x = 1;"
                , "  return;"
                , "  x = 2;"
                , "}"
                ]
            let funcBody = head ast
            let stmts = case unFix funcBody of C.FunctionDefn _ _ (Fix (C.CompoundStmt s)) -> s; _ -> []
            let cfg = runIdentity $ buildCFG empty funcBody (runIdentity $ emptyFacts empty) :: CFG Text StatementCoverage
            let (finalCfg, _) = runIdentity $ fixpoint empty "f" cfg
            let (StatementCoverage finalFacts) = runIdentity $ foldM (join empty) (runIdentity $ emptyFacts empty) (map cfgOutFacts (Map.elems finalCfg))
            let isAssignX2 = \case Fix (C.ExprStmt (Fix (C.AssignExpr (Fix (C.VarExpr (C.L _ _ "x"))) _ (Fix (C.LiteralExpr C.Int (C.L _ _ "2")))))) -> True; _ -> False
            let unreachableStmt = showNodePlain (head (filter isAssignX2 stmts))
            Set.member unreachableStmt finalFacts `shouldBe` False
