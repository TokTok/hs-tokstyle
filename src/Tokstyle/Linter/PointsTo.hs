{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.PointsTo (descr) where

import           Control.Monad                       (foldM)
import           Control.Monad.State.Strict
import           Data.Fix
import           Data.IntMap.Strict                  (IntMap)
import qualified Data.IntMap.Strict                  as IntMap
import           Data.IntSet                         (IntSet)
import qualified Data.IntSet                         as IntSet
import           Data.List                           (foldl', splitAt)
import qualified Data.Map                            as Map
import           Data.Maybe                          (fromMaybe)
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import qualified Language.Cimple                     as C
import qualified Language.Cimple.Diagnostics         as Diagnostics
import           Language.Cimple.TraverseAst
import           Tokstyle.Analysis.DataFlow          (CFGNode (..), buildCFG,
                                                      fixpoint, transfer)
import           Tokstyle.Analysis.PointsTo          (evalExpr)
import           Tokstyle.Analysis.PointsTo.Fixpoint (findEntryPointsAndFuncMap,
                                                      runGlobalFixpoint)
import           Tokstyle.Analysis.PointsTo.Types
import           Tokstyle.Analysis.Scope             (ScopedId (..),
                                                      runScopePass)
import           Tokstyle.Analysis.VTable            (resolveVTables)
import           Tokstyle.Common.TypeSystem          (collect)

analyse :: [(FilePath, [C.Node (C.Lexeme Text)])] -> [Text]
analyse sources =
    let
        -- 1. Setup
        flatAst = concatMap snd sources
        (scopedAsts, _) = runScopePass flatAst
        typeSystem = collect (("test.c", flatAst) : map (\(fp, ast) -> (fp, ast)) sources)
        vtableMap = resolveVTables scopedAsts typeSystem
        (_, funcMap) = findEntryPointsAndFuncMap scopedAsts

        -- 2. Run global points-to analysis
        filePath = fst (head sources)
        dummyId = ScopedId 0 "" C.Global
        ctx = PointsToContext filePath typeSystem vtableMap (GlobalEnv Map.empty) funcMap dummyId Map.empty
        (gEnv, _, cfgCache, pool) = runGlobalFixpoint ctx scopedAsts

        -- 3. Lint each function using the analysis results
        (GlobalEnv globalEnvMap) = gEnv
        allFuncContextPairs = Map.keys globalEnvMap

        lintFunction (funcId, relevantState) =
            case Map.lookup (funcId, relevantState) cfgCache of
                Just (cfgs, _) ->
                    let lintCtx = ctx { pcGlobalEnv = gEnv, pcCurrentFunc = funcId }
                    in concatMap (\cfg -> concatMap (checkNode lintCtx pool) (Map.elems cfg)) cfgs
                Nothing -> []

        lintResults = concatMap lintFunction allFuncContextPairs
    in
        lintResults

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
        C.VarDeclStmt _ (Just rhs)              -> checkRhs rhs
        C.ExprStmt (Fix (C.AssignExpr _ _ rhs)) -> checkRhs rhs
        C.Return (Just expr)                    -> checkRhs expr
        _                                       -> return []
  where
    checkRhs rhs =
        let
            -- Helper to find the actual function call's callee expression
            findCallee :: C.Node (C.Lexeme ScopedId) -> Maybe (C.Node (C.Lexeme ScopedId))
            findCallee (Fix (C.FunctionCall callee _)) = Just callee
            findCallee (Fix (C.CastExpr _ inner))      = findCallee inner
            findCallee _                               = Nothing
        in
            case findCallee rhs of
                Just (Fix (C.VarExpr lexeme@(C.L _ _ sid))) -> do
                    returnValueLocs <- evalExpr facts ctx nodeId rhs
                    pool <- get
                    unknownLoc <- intern UnknownLoc
                    let isUnresolved = IntSet.member (unIMemLoc unknownLoc) returnValueLocs
                    if isUnresolved then
                        return [Diagnostics.sloc (pcFilePath ctx) lexeme <> ": The return value of function '" <> sidName sid <> "' is used here, but its value could not be determined by the analysis."]
                    else
                        return []
                _ -> return [] -- Not a direct call or a call we want to check

descr :: ([(FilePath, [C.Node (C.Lexeme Text)])] -> [Text], (Text, Text))
descr = (analyse, ("points-to", Text.unlines
    [ "Checks for use of return values from unsummarized external functions."
    , ""
    , "**Reason:** Calling an external function that is not summarized and using its"
    , "return value can lead to a loss of precision in the points-to analysis,"
    , "potentially hiding bugs. It's better to provide a summary for the function."
    ]))
