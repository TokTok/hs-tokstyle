{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.SecurityRank (descr) where

import           Data.Foldable                              (foldl')
import           Data.List                                  (nub)
import           Data.Map.Strict                            (Map)
import qualified Data.Map.Strict                            as Map
import           Data.Maybe                                 (fromMaybe)
import           Data.Set                                   (Set)
import qualified Data.Set                                   as Set
import           Data.Text                                  (Text)
import qualified Data.Text                                  as Text
import           Debug.Trace                                (trace)
import qualified Language.Cimple                            as C
import           Tokstyle.Analysis.CallGraph                (buildCallGraph)
import           Tokstyle.Analysis.PointsTo                 (PointsToContext (..),
                                                             PointsToSummary,
                                                             buildPointsToContext)
import           Tokstyle.Analysis.SecurityRank             (SecurityRankContext (..),
                                                             SecurityRankState (..),
                                                             SecurityRankSummary,
                                                             analyzeFunction,
                                                             buildPointsToSummaryFromAnnotation,
                                                             buildSecurityRankSummaryFromAnnotation,
                                                             findFunctionDecls,
                                                             findFunctionDefs,
                                                             findStructDefs,
                                                             getFuncNameFromDef,
                                                             runInterproceduralAnalysis)
import           Tokstyle.Analysis.SecurityRank.Annotations (parseAllAnnotations)
import           Tokstyle.Analysis.SecurityRank.Types       (SecurityRankSummaryData (..),
                                                             emptySecurityRankSummaryData)
import           Tokstyle.Analysis.Types                    (FunctionName,
                                                             lookupOrError)

-- | The main analysis function for the linter.
analyse :: [(FilePath, [C.Node (C.Lexeme Text)])] -> [Text]
analyse tus =
    trace ("Analyzing files: " ++ show (map fst tus)) $
    let
        -- 1. Parse annotations first. This is the ground truth for sources/sinks.
        annotations = parseAllAnnotations tus
        funcDecls = findFunctionDecls tus
        funcDefs = findFunctionDefs tus
        structDefs = findStructDefs (concatMap snd tus)

        -- 2. Build initial summaries from annotations for all declared functions.
        initialPointsToSummaries = Map.mapWithKey (buildPointsToSummaryFromAnnotation annotations) (fmap snd funcDecls)
        initialSecurityRankSummaries = Map.mapWithKey (buildSecurityRankSummaryFromAnnotation annotations) (fmap snd funcDecls)

        -- 3. Run the unified interprocedural analysis.
        finalSecurityRankContext = runInterproceduralAnalysis annotations initialPointsToSummaries funcDecls funcDefs structDefs initialSecurityRankSummaries

        -- 4. Collect all diagnostics from the final context-sensitive analysis.
        allDiagnostics = collectAllDiagnostics finalSecurityRankContext
    in
        allDiagnostics

-- | Runs the analysis on all functions and contexts to collect diagnostics.
collectAllDiagnostics :: SecurityRankContext Text -> [Text]
collectAllDiagnostics finalCtx =
    let
        allSummaries = srcSummaries finalCtx
        -- Only take diagnostics from the summary for the empty context, as this
        -- represents the final, user-facing analysis of a function. Diagnostics
        -- from other contexts are intermediate and would create noise.
        allDiagnostics = concatMap (srsdDiagnostics . fromMaybe emptySecurityRankSummaryData . Map.lookup []) (Map.elems allSummaries)
    in
        map Text.pack (nub allDiagnostics)


-- | The linter description record.
descr :: ([(FilePath, [C.Node (C.Lexeme Text)])] -> [Text], (Text, Text))
descr = (analyse, ("security-rank", Text.unlines
    [ "Performs a global taint analysis based on @security_rank annotations."
    , ""
    , "**Reason:** to prevent data with a lower security rank from flowing into"
    , "a sink that requires a higher security rank."
    ]))
