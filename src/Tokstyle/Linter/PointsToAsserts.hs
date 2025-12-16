{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.PointsToAsserts (descr) where

import           Control.Monad                       (foldM, guard)
import           Control.Monad.State.Strict          (evalState, get, runState)
import           Data.Fix
import           Data.IntMap.Strict                  (IntMap)
import qualified Data.IntMap.Strict                  as IntMap
import           Data.IntSet                         (IntSet)
import qualified Data.IntSet                         as IntSet
import           Data.List                           (foldl')
import qualified Data.Map.Strict                     as Map
import           Data.Maybe                          (fromMaybe)
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import qualified Debug.Trace                         as Debug
import qualified Language.Cimple                     as C
import qualified Language.Cimple.Diagnostics         as Diagnostics
import           Tokstyle.Analysis.DataFlow          (CFGNode (..), transfer)
import           Tokstyle.Analysis.PointsTo          (evalExpr)
import           Tokstyle.Analysis.PointsTo.Fixpoint (findEntryPointsAndFuncMap,
                                                      findVarTypes,
                                                      runGlobalFixpoint)
import           Tokstyle.Analysis.PointsTo.Types
import           Tokstyle.Analysis.Scope             (ScopedId (..),
                                                      runScopePass)
import           Tokstyle.Analysis.VTable            (resolveVTables)
import           Tokstyle.Common.TypeSystem          (collect)

debugging :: Bool
debugging = False

dtrace :: String -> a -> a
dtrace msg x = if debugging then Debug.trace msg x else x

analyse :: [(FilePath, [C.Node (C.Lexeme Text)])] -> [Text]
analyse sources =
    let
        flatAst = concatMap snd sources
        (scopedAsts, _) = runScopePass flatAst
        typeSystem = collect (("test.c", flatAst) : map (\(fp, ast) -> (fp, ast)) sources)
        vtableMap = resolveVTables scopedAsts typeSystem
        (_, funcMap) = findEntryPointsAndFuncMap scopedAsts

        filePath = fst (head sources)
        dummyId = ScopedId 0 "" C.Global
        ctx = PointsToContext filePath typeSystem vtableMap (GlobalEnv Map.empty) funcMap dummyId Map.empty
        (gEnv, _, cfgCache, pool) = runGlobalFixpoint ctx scopedAsts

        (GlobalEnv globalEnvMap) = gEnv
        allFuncContextPairs = Map.keys globalEnvMap

        lintFunction (funcId, relevantState) =
            case Map.lookup (funcId, relevantState) cfgCache of
                Just (cfgs, _) ->
                    let funcDef = head (fromMaybe [] (Map.lookup funcId funcMap))
                        varTypes = findVarTypes funcDef
                        lintCtx = ctx { pcGlobalEnv = gEnv, pcCurrentFunc = funcId, pcVarTypes = varTypes }
                    in concatMap (\cfg -> concatMap (checkNode lintCtx pool) (Map.elems cfg)) cfgs
                Nothing -> []

        lintResults = concatMap lintFunction allFuncContextPairs
    in
        Set.toList (Set.fromList lintResults)

checkNode :: PointsToContext ScopedId -> MemLocPool -> CFGNode ScopedId PointsToFact -> [Text]
checkNode ctx pool node =
    let
        folder (facts, diags) stmt = do
            (nextFacts, _) <- transfer ctx (pcCurrentFunc ctx) (cfgNodeId node) facts stmt
            newDiags <- checkStmt ctx (cfgNodeId node) facts stmt
            return (nextFacts, diags ++ newDiags)
        ((_, allDiags), _) = runState (foldM folder (cfgInFacts node, []) (cfgStmts node)) pool
    in
        allDiags

checkStmt :: PointsToContext ScopedId -> Int -> PointsToFact -> C.Node (C.Lexeme ScopedId) -> PointsToAnalysis [Text]
checkStmt ctx nodeId facts stmt =
    case unFix stmt of
        C.ExprStmt (Fix (C.FunctionCall (Fix (C.VarExpr lexeme@(C.L _ _ sid))) [arg])) | sidName sid == "assert" ->
            checkAssert ctx nodeId facts lexeme arg
        _ -> return []

checkAssert :: PointsToContext ScopedId -> Int -> PointsToFact -> C.Lexeme ScopedId -> C.Node (C.Lexeme ScopedId) -> PointsToAnalysis [Text]
checkAssert ctx nodeId facts assertLexeme assertArg =
    case evalAssertExpr assertArg of
        Just (ptrArg, predicate, desc) ->
            let errMsg = "does not satisfy assertion: must be " <> desc
            in checkPredicate ctx nodeId facts assertLexeme ptrArg predicate errMsg
        Nothing -> return []

checkPredicate :: PointsToContext ScopedId -> Int -> PointsToFact -> C.Lexeme ScopedId -> C.Node (C.Lexeme ScopedId) -> (MemLoc -> Bool) -> Text -> PointsToAnalysis [Text]
checkPredicate ctx nodeId facts assertLexeme ptrArg predicate errMsg = do
    ptrLocsInt <- evalExpr facts ctx nodeId ptrArg
    pool <- get
    let ptrLocs = map (\i -> IntMap.findWithDefault UnknownLoc i (idToMemLoc pool)) (IntSet.toList ptrLocsInt)
    let predicateResults = map predicate ptrLocs
    if not (null ptrLocs) && all (==True) predicateResults
       then return []
       else return
           [ Diagnostics.sloc (pcFilePath ctx) assertLexeme <>
             ": Static analysis check failed: Pointer '" <> sidName (C.lexemeText (fromMaybe (C.L (C.AlexPn 0 0 0) C.IdVar (ScopedId 0 "" C.Local)) (getLexeme ptrArg))) <>
             "' " <> errMsg <> ". It can point to " <> summarizeMemLocs (Set.fromList ptrLocs) <> "."
           ]

stripParens :: C.Node a -> C.Node a
stripParens (Fix (C.ParenExpr inner)) = stripParens inner
stripParens node                      = node

evalAssertExpr :: C.Node (C.Lexeme ScopedId) -> Maybe (C.Node (C.Lexeme ScopedId), MemLoc -> Bool, Text)
evalAssertExpr expr = dtrace ("[evalAssertExpr] " ++ show (fmap (const ()) (unFix expr))) $
    case unFix expr of
    C.ParenExpr inner -> evalAssertExpr inner
    C.UnaryExpr C.UopNot inner -> do
        (ptrArg, p, desc) <- evalAssertExpr inner
        let newDesc = case unFix (stripParens inner) of
                C.BinaryExpr {} -> "not (" <> desc <> ")"
                _               -> "not " <> desc
        Just (ptrArg, not . p, newDesc)
    C.FunctionCall (Fix (C.VarExpr (C.L _ _ ScopedId{sidName}))) [ptrArg] ->
        case sidName of
            "mem_is_heap"           -> Just (ptrArg, \case { HeapLoc _ -> True; _ -> False }, "heap")
            "mem_is_stack"          -> Just (ptrArg, \case { StackLoc _ -> True; _ -> False }, "stack")
            "mem_is_not_null"       -> Just (ptrArg, \case { NullLoc -> False; _ -> True }, "not null")
            "mem_is_external_param" -> Just (ptrArg, \case { ExternalParamLoc {} -> True; _ -> False }, "external param")
            _                       -> Nothing
    C.BinaryExpr lhs op rhs -> do
        (ptrArg1, p1, desc1) <- evalAssertExpr lhs
        (ptrArg2, p2, desc2) <- evalAssertExpr rhs
        guard (fmap C.lexemeText (getLexeme ptrArg1) == fmap C.lexemeText (getLexeme ptrArg2))
        let pDesc1 = paren op lhs desc1
        let pDesc2 = paren op rhs desc2
        case op of
            C.BopOr  -> Just (ptrArg1, \loc -> p1 loc || p2 loc, pDesc1 <> " or " <> pDesc2)
            C.BopAnd -> Just (ptrArg1, \loc -> p1 loc && p2 loc, pDesc1 <> " and " <> pDesc2)
            _        -> Nothing
    _ -> Nothing

opPrec :: C.BinaryOp -> Int
opPrec C.BopOr  = 1
opPrec C.BopAnd = 2
opPrec _        = 99

paren :: C.BinaryOp -> C.Node a -> Text -> Text
paren outerOp node txt = case unFix node of
    C.ParenExpr inner        -> paren outerOp inner txt
    C.BinaryExpr _ innerOp _ -> if opPrec innerOp < opPrec outerOp
        then "(" <> txt <> ")"
        else txt
    _ -> txt

summarizeMemLocs :: Set MemLoc -> Text
summarizeMemLocs locs =
    let simplifiedNames = Set.map (Text.pack . takeWhile (/= ' ') . show) locs
    in "{" <> Text.intercalate ", " (Set.toList simplifiedNames) <> "}"

getLexeme :: C.Node (C.Lexeme l) -> Maybe (C.Lexeme l)
getLexeme (Fix (C.VarExpr l))          = Just l
getLexeme (Fix (C.MemberAccess _ l))   = Just l
getLexeme (Fix (C.PointerAccess _ l))  = Just l
getLexeme (Fix (C.ArrayAccess base _)) = getLexeme base
getLexeme _                            = Nothing

descr :: ([(FilePath, [C.Node (C.Lexeme Text)])] -> [Text], (Text, Text))
descr = (analyse, ("points-to-asserts", Text.unlines
    [ "Checks for static analysis assertions in the code."
    , ""
    , "**Reason:** Allows developers to enforce invariants about pointer properties"
    , "(e.g., whether a pointer refers to heap or stack memory) that are"
    , "verified at compile time by the points-to analysis."
    , ""
    , "Supported checks:"
    , "  - `mem_is_heap(p)`: asserts that `p` points to a heap location."
    , "  - `mem_is_stack(p)`: asserts that `p` points to a stack location."
    , "  - `mem_is_not_null(p)`: asserts that `p` is not null."
    , "  - `mem_is_external_param(p)`: asserts that `p` points to an external parameter."
    , ""
    , "Checks can be combined with `&&` and `||`, and negated with `!`."
    , "For example: `assert(!(mem_is_heap(p) || mem_is_stack(p)))`"
    ]))
