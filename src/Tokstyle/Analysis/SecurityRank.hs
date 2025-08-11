{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module Tokstyle.Analysis.SecurityRank
    ( SecurityRankContext(..)
    , SecurityRankState(..)
    , SecurityRankSummary
    , SecurityRankSummaryData(..)
    , runInterproceduralAnalysis
    , analyzeFunction
    , findFunctionDecls
    , findFunctionDefs
    , findStructDefs
    , getFuncNameFromDef
    , buildSecurityRankSummaryFromAnnotation
    , buildPointsToSummaryFromAnnotation
    ) where

import           Control.Monad                              (foldM, forM_, when)
import           Control.Monad.State.Strict                 (State, StateT,
                                                             execState,
                                                             execStateT, get,
                                                             lift, modify, put,
                                                             runState,
                                                             runStateT)
import           Data.Fix                                   (Fix (..))
import           Data.Foldable                              (asum)
import           Data.List                                  (find, findIndex,
                                                             foldl', nub)
import           Data.Map.Strict                            (Map)
import qualified Data.Map.Strict                            as Map
import           Data.Maybe                                 (fromMaybe,
                                                             mapMaybe)
import           Data.Set                                   (Set)
import qualified Data.Set                                   as Set
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           Debug.Trace                                (trace, traceM)
import qualified Language.Cimple                            as C
import           Language.Cimple.TraverseAst                (AstActions (..),
                                                             astActions,
                                                             traverseAst)
import           Text.Groom                                 (groom)
import           Text.Read                                  (readMaybe)
import           Tokstyle.Analysis.Context                  (kLimit,
                                                             pushContext)
import           Tokstyle.Analysis.DataFlow
import qualified Tokstyle.Analysis.PointsTo                 as PointsTo
import           Tokstyle.Analysis.SecurityRank.Annotations (AnnotationMap)
import           Tokstyle.Analysis.SecurityRank.Lattice
import           Tokstyle.Analysis.SecurityRank.Types       (SecurityRankSummary,
                                                             SecurityRankSummaryData (..),
                                                             emptySecurityRankSummaryData)
import           Tokstyle.Analysis.Types                    (AbstractLocation (..),
                                                             CallGraph, Context,
                                                             FunctionName,
                                                             NodeId,
                                                             PointsToMap,
                                                             PointsToSummaryData (..),
                                                             getCallers,
                                                             toAbstractLocation)
import           Tokstyle.Worklist

fakeTestSource :: FilePath
fakeTestSource = "test.c"

debugging :: Bool
debugging = False

dtrace :: String -> a -> a
dtrace msg x = if debugging then trace msg x else x

dtraceM :: Monad m => String -> m ()
dtraceM msg = if debugging then traceM msg else return ()

getFuncNameFromLoc :: AbstractLocation -> Maybe FunctionName
getFuncNameFromLoc (VarLocation name)      = Just name
getFuncNameFromLoc (FunctionLocation name) = Just name
getFuncNameFromLoc _                       = Nothing

-- | The context for the security rank analysis.
data SecurityRankContext l = SecurityRankContext
    { srcCurrentFile      :: FilePath
    , srcAnnotations      :: AnnotationMap
    , srcSummaries        :: Map FunctionName SecurityRankSummary
    , srcFuncDecls        :: Map FunctionName (FilePath, C.Node (C.Lexeme l))
    , srcFuncDefs         :: Map FunctionName (FilePath, C.Node (C.Lexeme l))
    , srcStructDefs       :: Map Text (C.Node (C.Lexeme l))
    , srcCallGraph        :: CallGraph
    , srcPointsToContext  :: PointsTo.PointsToContext l
    , srcCurrentContext   :: Context
    , srcDynamicCallGraph :: Map (FunctionName, Context) (Set (FunctionName, Context))
    , srcAnalyzedCfgs     :: Map (FunctionName, Context) (CFG l SecurityRankState)
    }

-- | The state that flows through the intra-procedural analysis.
data SecurityRankState = SecurityRankState
    { srsTaintState  :: TaintState
    , srsFptrSigs    :: Map AbstractLocation SecurityRankSummaryData
    , srsDiagnostics :: [Diagnostic]
    } deriving (Eq, Show)

instance DataFlow SecurityRankContext Text SecurityRankState where
    emptyFacts _ = SecurityRankState Map.empty Map.empty []
    join _ s1@(SecurityRankState a f1 d1) s2@(SecurityRankState b f2 d2) =
        let result = SecurityRankState (Map.unionWith mergeRank a b) (Map.unionWith joinSummaries f1 f2) (nub (d1 ++ d2))
        in dtrace ("\n--- JOIN ---\n" <> "S1: " <> groom s1 <> "\nS2: " <> groom s2 <> "\nRESULT: " <> groom result) result
    transfer = transferRank


type Diagnostic = String

-- | The main entry point for the inter-procedural security rank analysis.
-- It computes summaries for all functions until a fixpoint is reached.
runInterproceduralAnalysis :: AnnotationMap
                           -> Map FunctionName PointsTo.PointsToSummary
                           -> Map FunctionName (FilePath, C.Node (C.Lexeme Text))
                           -> Map FunctionName (FilePath, C.Node (C.Lexeme Text))
                           -> Map Text (C.Node (C.Lexeme Text))
                           -> Map FunctionName SecurityRankSummary
                           -> SecurityRankContext Text
runInterproceduralAnalysis annotations initialPointsToSummaries funcDecls funcDefs structDefs initialSecurityRankSummaries =
    let
        worklist = fromList $ map (, []) (Map.keys funcDecls)
        initialPtsCtx = PointsTo.PointsToContext
            { PointsTo.ptcCallGraph = Map.empty
            , PointsTo.ptcSummaries = initialPointsToSummaries
            , PointsTo.ptcFuncDefs = fmap snd funcDefs
            , PointsTo.ptcFuncDecls = fmap snd funcDecls
            , PointsTo.ptcStructDefs = structDefs
            , PointsTo.ptcVarTypes = Map.empty
            , PointsTo.ptcCurrentContext = []
            , PointsTo.ptcDynamicCallGraph = Map.empty
            , PointsTo.ptcAnalyzedCfgs = Map.empty
            , PointsTo.ptcLocalVars = Set.empty
            , PointsTo.ptcFileMacros = Map.empty
            }
        initialSrCtx = SecurityRankContext
            { srcCurrentFile = ""
            , srcAnnotations = annotations
            , srcSummaries = initialSecurityRankSummaries
            , srcFuncDecls = funcDecls
            , srcFuncDefs = funcDefs
            , srcStructDefs = structDefs
            , srcCallGraph = Map.empty
            , srcPointsToContext = initialPtsCtx
            , srcCurrentContext = []
            , srcDynamicCallGraph = Map.empty
            , srcAnalyzedCfgs = Map.empty
            }
    in
        fixpointSummaries initialSrCtx worklist


-- | The fixed-point iteration loop for computing function summaries.
fixpointSummaries :: SecurityRankContext Text -> Worklist (FunctionName, Context) -> SecurityRankContext Text
fixpointSummaries ctx worklist =
    dtrace ("\n--- FIXPOINT ITERATION ---\nWORKLIST: " <> groom worklist) $
    case pop worklist of
        Just ((funcName, context), worklist') ->
            dtrace ("SecurityRank.fixpointSummaries: Analyzing " ++ T.unpack funcName ++ " in context " ++ groom context) $
            if Map.notMember funcName (srcFuncDefs ctx) then
                fixpointSummaries ctx worklist'
            else
                let
                    -- First, run the PointsTo analysis for the current function and context.
                    ptsCtx = srcPointsToContext ctx
                    ptsWorklist = fromList [(funcName, context)]
                    ptsCtx' = PointsTo.fixpointSummaries ptsCtx ptsWorklist

                    -- Now, run the SecurityRank analysis with the updated points-to context.
                    (filePath, funcDef) = fromMaybe (error $ "Function def not found: " ++ T.unpack funcName) (Map.lookup funcName (srcFuncDefs ctx))
                    ctxForFunc = ctx { srcCurrentFile = filePath, srcCurrentContext = context, srcPointsToContext = ptsCtx' }

                    (finalState, finalCfg, newCallees) = analyzeFunction ctxForFunc funcDef

                    newSummaryData = generateSummaryFromState ctxForFunc finalState finalCfg funcDef
                    oldSummaries = fromMaybe Map.empty (Map.lookup funcName (srcSummaries ctx))
                    oldSummaryData = fromMaybe emptySecurityRankSummaryData (Map.lookup context oldSummaries)

                    mergedSummaryData = joinSummaries oldSummaryData newSummaryData
                    summaries' = Map.insert funcName (Map.insert context mergedSummaryData oldSummaries) (srcSummaries ctx)

                    dynamicCallGraph' = Map.insert (funcName, context) newCallees (srcDynamicCallGraph ctx)
                    worklistWithCallees = pushList (Set.toList newCallees) worklist'

                    traceMsg = "Comparing summaries for " <> T.unpack funcName <> " in context " <> groom context <>
                               "\n  OLD: " <> groom oldSummaryData <>
                               "\n  NEW_RAW: " <> groom newSummaryData <>
                               "\n  MERGED: " <> groom mergedSummaryData
                    worklist'' = if dtrace traceMsg mergedSummaryData /= oldSummaryData
                                 then
                                     let dependents = Map.foldlWithKey' (\acc (caller, callerCtx) callees ->
                                                  if Set.member (funcName, context) callees
                                                  then Set.insert (caller, callerCtx) acc
                                                  else acc
                                              ) Set.empty dynamicCallGraph'
                                     in dtrace ("Summary changed for " <> T.unpack funcName <> ", adding dependents: " <> groom dependents) $ pushList (Set.toList dependents) worklistWithCallees
                                 else worklistWithCallees
                    analyzedCfgs' = Map.insert (funcName, context) finalCfg (srcAnalyzedCfgs ctx)
                    ctx' = ctx { srcSummaries = summaries', srcDynamicCallGraph = dynamicCallGraph', srcAnalyzedCfgs = analyzedCfgs', srcPointsToContext = ptsCtx' }
                in
                    fixpointSummaries ctx' worklist''
        Nothing ->
            dtrace ("SecurityRank.fixpointSummaries: done") $
            ctx



-- | Runs an intra-procedural analysis on a single function.
analyzeFunction :: SecurityRankContext Text -> C.Node (C.Lexeme Text) -> (SecurityRankState, CFG Text SecurityRankState, Set (FunctionName, Context))
analyzeFunction ctx funcDef =
    let
        funcName = getFuncNameFromDef funcDef
    in
        if Map.notMember funcName (srcFuncDefs ctx) then
            (emptyFacts ctx, Map.empty, Set.empty)
        else
            let
                initialFacts = createInitialFacts ctx funcDef
                cfg = buildCFG ctx funcDef initialFacts
                (finalCfg, newWork) = fixpoint ctx funcName cfg
                finalState = foldl' (join ctx) (emptyFacts ctx) (Map.elems (fmap cfgOutFacts finalCfg))
            in
                (finalState, finalCfg, newWork)

findCallersOf :: SecurityRankContext Text -> FunctionName -> Context -> [(FunctionName, Context, C.Node (C.Lexeme Text))]
findCallersOf ctx calleeName calleeContext =
    let
        dynamicCallGraph = srcDynamicCallGraph ctx
        callSiteNodeId = head calleeContext

        findCallingStmt :: CFG Text SecurityRankState -> Maybe (C.Node (C.Lexeme Text))
        findCallingStmt cfg =
            let
                allStmts = concatMap cfgStmts (Map.elems cfg)
                findNodeInStmt :: C.Node (C.Lexeme Text) -> Maybe (C.Node (C.Lexeme Text))
                findNodeInStmt topStmt =
                    let
                        collector = astActions {
                            doNode = \_ (node :: C.Node (C.Lexeme Text)) act -> do
                                get >>= \case
                                    Just _ -> return ()
                                    Nothing -> do
                                        when (C.getNodeId node == callSiteNodeId) $ put (Just topStmt)
                                        act
                        }
                    in
                        execState (traverseAst collector [(fakeTestSource, [topStmt])]) Nothing
            in
                asum (map findNodeInStmt allStmts)

        callers = Map.foldlWithKey' (\acc (caller, callerCtx) callees ->
            if Set.member (calleeName, calleeContext) callees
            then case Map.lookup (caller, callerCtx) (srcAnalyzedCfgs ctx) of
                    Just cfg -> case findCallingStmt cfg of
                        Just stmt -> (caller, callerCtx, stmt) : acc
                        Nothing   -> acc
                    Nothing -> acc
            else acc
          ) [] dynamicCallGraph
    in
        callers

computeFactsFromCallSite :: SecurityRankContext Text -> FunctionName -> (FunctionName, Context, C.Node (C.Lexeme Text)) -> TaintState
computeFactsFromCallSite ctx calleeName (callerName, callerContext, callStmt) =
    let
        callerCfg = fromMaybe (error "caller CFG not found for SecurityRank") $ Map.lookup (callerName, callerContext) (srcAnalyzedCfgs ctx)
        callNode = fromMaybe (error "call node not found in CFG for SecurityRank") $ find (\n -> callStmt `elem` cfgStmts n) (Map.elems callerCfg)
        stmtsBeforeCall = takeWhile (/= callStmt) (cfgStmts callNode)
        callerState = srsTaintState $ fst $ foldl'
            (\ (accFacts, _) stmt -> transferRank ctx callerName accFacts stmt)
            (cfgInFacts callNode, Set.empty)
            stmtsBeforeCall

        -- Recompute the PointsToMap at the call site
        pctx = (srcPointsToContext ctx) { PointsTo.ptcCurrentContext = callerContext }
        callerPtsCfg = dtrace ("computeFactsFromCallSite: Looking up Pts CFG for (" ++ T.unpack callerName ++ ", " ++ groom callerContext ++ ") in " ++ show (Map.keys (PointsTo.ptcAnalyzedCfgs pctx))) $ fromMaybe (error $ "caller Pts CFG not found for (" ++ T.unpack callerName ++ ", " ++ groom callerContext ++ ")") $ Map.lookup (callerName, callerContext) (PointsTo.ptcAnalyzedCfgs pctx)
        ptsCallNode = fromMaybe (error "call node not found in Pts CFG") $ find (\n -> callStmt `elem` cfgStmts n) (Map.elems callerPtsCfg)

        callerPtsMap = PointsTo.ptsMap $ fst $ foldl'
            (\(accFacts, _) stmt -> PointsTo.transferPointsToState pctx callerName accFacts stmt)
            (cfgInFacts ptsCallNode, Set.empty)
            stmtsBeforeCall

        (_, args) = case unFix callStmt of
            C.ExprStmt (Fix (C.FunctionCall e a)) -> (e, a)
            C.FunctionCall e a -> (e, a)
            C.AssignExpr _ _ (Fix (C.FunctionCall e a)) -> (e, a)
            C.VarDeclStmt _ (Just (Fix (C.FunctionCall e a))) -> (e, a)
            _ -> error $ "Unhandled call statement structure in SecurityRank: " ++ groom callStmt

        (_, calleeDef) = fromMaybe (error $ "callee def not found: " ++ T.unpack calleeName) (Map.lookup calleeName (srcFuncDefs ctx))
        paramNames = getParamNamesFromDef calleeDef

        paramFacts = zipWith (\pName arg ->
            let rank = evalRank ctx callerName callerState callerPtsMap arg
            in (VarLocation pName, rank)
          ) paramNames args
    in
        dtrace ("computeFactsFromCallSite for " <> T.unpack calleeName <> " called by " <> T.unpack callerName <> " in context " <> groom callerContext <>
                "\n  CALLER_TAINT_STATE: " <> groom callerState <>
                "\n  CALLER_PTS_MAP: " <> groom callerPtsMap <>
                "\n  PARAM_FACTS: " <> groom paramFacts) $
        Map.fromList paramFacts



createInitialFacts :: SecurityRankContext Text -> C.Node (C.Lexeme Text) -> SecurityRankState
createInitialFacts ctx funcDef =
    let
        funcName = getFuncNameFromDef funcDef
        calleeContext = srcCurrentContext ctx
        funcAnns = fromMaybe Map.empty (Map.lookup funcName (srcAnnotations ctx))
        paramTypes = getParamTypesFromDef funcDef
        paramNames = getParamNamesFromDef funcDef

        -- Taint from annotations on parameters and struct fields.
        paramTaints = Map.fromListWith mergeRank $ concatMap (\(pName, pType) ->
            dtrace ("  Processing param: " <> T.unpack pName <> " with type " <> groom pType) $
            let
                paramAnnTaint = case Map.lookup ("source:" <> pName) funcAnns of
                    Just rank ->
                        let loc = case unFix pType of
                                C.TyPointer {} -> DerefLocation (VarLocation pName)
                                _              -> VarLocation pName
                        in [(loc, rank)]
                    Nothing   -> []

                structFieldTaint = case unFix pType of
                    C.TyPointer (Fix (C.TyStruct (C.L _ _ structName))) ->
                        let structDef = fromMaybe (error $ "Struct def not found: " ++ T.unpack structName) (Map.lookup structName (srcStructDefs ctx))
                            memberNames = getStructMemberNames structDef
                            baseLoc = DerefLocation (VarLocation pName)
                        in dtrace ("    Struct " <> T.unpack structName <> " has members: " <> groom memberNames) $
                           mapMaybe (\mName ->
                               let memberAnnKey = structName <> "." <> mName
                                   memberAnns = fromMaybe Map.empty (Map.lookup memberAnnKey (srcAnnotations ctx))
                               in case Map.lookup "source" memberAnns of
                                   Just rank -> Just (FieldLocation baseLoc mName, rank)
                                   Nothing   -> Nothing
                           ) memberNames
                    _ -> []
            in paramAnnTaint ++ structFieldTaint
          ) (zip paramNames paramTypes)

        initialTaintState = if null calleeContext
            then paramTaints
            else
                let
                    callers = findCallersOf ctx funcName calleeContext
                    factList = map (computeFactsFromCallSite ctx funcName) callers
                    taintFromCallers = foldl' (Map.unionWith mergeRank) Map.empty factList
                in
                    Map.unionWith mergeRank paramTaints taintFromCallers
    in
        SecurityRankState initialTaintState Map.empty []




-- | Generates a function summary from the final state of its analysis.
generateSummaryFromState :: SecurityRankContext Text -> SecurityRankState -> CFG Text SecurityRankState -> C.Node (C.Lexeme Text) -> SecurityRankSummaryData
generateSummaryFromState ctx finalState finalCfg funcNode =
    let
        funcName = getFuncNameFromDef funcNode
        paramNames = getParamNamesFromDef funcNode
        taintState = srsTaintState finalState

        -- 1. Handle output parameters by checking the final taint state
        paramOutRanks = Map.fromList $ mapMaybe (\pName ->
            let pLoc = DerefLocation (VarLocation pName)
            in case Map.lookup pLoc taintState of
                Just rank | rank < Safe -> Just (pLoc, rank)
                _                       -> Nothing
            ) paramNames

        -- 2. Handle return value
        returnStmts = findReturnStmts funcNode
        returnedRanks = map (evalReturnFromCFG funcName finalCfg) returnStmts
        finalReturnRank = foldl' mergeRank Safe returnedRanks
        returnOutRank = if finalReturnRank < Safe
                        then Map.singleton (ReturnLocation funcName) finalReturnRank
                        else Map.empty

        -- 3. Get summary info from annotations
        funcAnns = fromMaybe Map.empty (Map.lookup funcName (srcAnnotations ctx))
        (annotatedOutputRanks, annotatedSinks) = Map.foldlWithKey' (buildRanksFromAnnotations funcName paramNames) (Map.empty, Map.empty) funcAnns

        -- 4. Combine analysis results with annotation results
        finalOutputRanks = Map.unionWith min (Map.union paramOutRanks returnOutRank) annotatedOutputRanks
        sinksFromAnalysis = findSinksInFunctionBody ctx finalCfg funcNode
        finalSinks = Map.unionWith max sinksFromAnalysis annotatedSinks

        diagnostics = nub (srsDiagnostics finalState)
        summary = SecurityRankSummaryData finalOutputRanks finalSinks diagnostics
    in
        dtrace ("SecurityRank.generateSummaryFromState (" <> T.unpack funcName <> ")\n  SUMMARY: " <> groom summary) summary
  where
    buildRanksFromAnnotations funcName paramNames (outs, sinks) key rank =
        case T.splitOn ":" key of
            ["source", "return"] ->
                (Map.insert (ReturnLocation funcName) rank outs, sinks)
            ["source", paramName] ->
                (Map.insert (DerefLocation (VarLocation paramName)) rank outs, sinks)
            ["sink", paramName] ->
                case findIndex (== paramName) paramNames of
                    Just i  -> (outs, Map.insert i rank sinks)
                    Nothing -> (outs, sinks)
            _ -> (outs, sinks)

    findReturnStmts :: C.Node (C.Lexeme Text) -> [C.Node (C.Lexeme Text)]
    findReturnStmts node = execState (traverseAst collector [(fakeTestSource, [node])]) []
      where
        collector :: AstActions (State [C.Node (C.Lexeme Text)]) Text
        collector = astActions
            { doNode = \_ n act -> do
                case unFix n of
                    C.Return (Just _) -> modify (n :)
                    _                 -> pure ()
                act
            }

    evalReturnFromCFG :: FunctionName -> CFG Text SecurityRankState -> C.Node (C.Lexeme Text) -> SecurityRank
    evalReturnFromCFG funcName cfg stmt@(Fix (C.Return (Just expr))) =
        case find (\node -> stmt `elem` cfgStmts node) (Map.elems cfg) of
            Just node ->
                let
                    pctx = srcPointsToContext ctx
                    -- Find the points-to CFG for the current function and context
                    ptsCfg = fromMaybe (fromMaybe (error $ "Points-to CFG not found for func " <> T.unpack funcName) (Map.lookup (funcName, []) (PointsTo.ptcAnalyzedCfgs pctx)))
                           (Map.lookup (funcName, srcCurrentContext ctx) (PointsTo.ptcAnalyzedCfgs pctx))
                    -- Find the corresponding node in the points-to CFG
                    ptNode = fromMaybe (error "Statement not found in points-to CFG for summary gen") $
                             find (\n -> stmt `elem` cfgStmts n) (Map.elems ptsCfg)
                    -- Use the facts *after* the statement for the rank evaluation
                    ptsMap = PointsTo.ptsMap $ cfgOutFacts ptNode
                in
                    evalRank ctx funcName (srsTaintState (cfgOutFacts node)) ptsMap expr
            Nothing   -> Safe -- Should not happen in a well-formed CFG
    evalReturnFromCFG _ _ _ = Safe



_getVarNameFromLoc' :: AbstractLocation -> Text
_getVarNameFromLoc' (VarLocation n) = n
_getVarNameFromLoc' _               = ""

getFuncNameFromCall :: PointsTo.PointsToContext Text -> PointsTo.MacroDefinitionMap -> FunctionName -> PointsToMap -> C.Node (C.Lexeme Text) -> [FunctionName]
getFuncNameFromCall pctx macroCtx funcName ptsMap callExpr =
    let (callees, _) = PointsTo.evalPointsToSet macroCtx pctx (PointsTo.ptcLocalVars pctx) funcName ptsMap callExpr
    in mapMaybe getFuncNameFromLoc (Set.toList callees)

findSinksInFunctionBody :: SecurityRankContext Text -> CFG Text SecurityRankState -> C.Node (C.Lexeme Text) -> Map Int SecurityRank
findSinksInFunctionBody ctx cfg funcDef =
    let
        funcName = getFuncNameFromDef funcDef
        paramNames = getParamNamesFromDef funcDef
        pctx' = srcPointsToContext ctx
        context = srcCurrentContext ctx

        -- Get the points-to CFG for the current function and context
        ptsCfg = fromMaybe (fromMaybe (error $ "Points-to CFG not found for func " <> T.unpack funcName) (Map.lookup (funcName, []) (PointsTo.ptcAnalyzedCfgs pctx')))
               (Map.lookup (funcName, context) (PointsTo.ptcAnalyzedCfgs pctx'))

        allCalls = concatMap (findCallsInNode . snd) (Map.toList cfg)

        findCallsInNode :: CFGNode Text SecurityRankState -> [C.Node (C.Lexeme Text)]
        findCallsInNode node = filter isCall (cfgStmts node)
          where isCall (Fix (C.FunctionCall _ _))                    = True
                isCall (Fix (C.ExprStmt (Fix (C.FunctionCall _ _)))) = True
                isCall _                                             = False

        getPointsToMapAtStmt :: C.Node (C.Lexeme Text) -> (PointsToMap, PointsTo.MacroDefinitionMap)
        getPointsToMapAtStmt stmt =
            let
                ptNode = fromMaybe (error "Statement not found in points-to CFG for sink finding") $
                         find (\n -> stmt `elem` cfgStmts n) (Map.elems ptsCfg)
                ptNodeState = cfgInFacts ptNode
            in
                (PointsTo.ptsMap ptNodeState, PointsTo.ptsMacros ptNodeState)

        processCall :: C.Node (C.Lexeme Text) -> Map Int SecurityRank
        processCall callStmt =
            let
                (callExpr, args) = case unFix callStmt of
                    C.FunctionCall e a -> (e, a)
                    C.ExprStmt (Fix (C.FunctionCall e a)) -> (e, a)
                    _ -> error "Unhandled call statement"

                _pctx'' = srcPointsToContext ctx
                (ptsMap, macroMap) = getPointsToMapAtStmt callStmt
                calleeNames = getFuncNameFromCall pctx' macroMap funcName ptsMap callExpr

                inferredSinks = foldl' (\accSinks calleeName ->
                    let
                        calleeSummaryMap = fromMaybe Map.empty (Map.lookup calleeName (srcSummaries ctx))
                        -- Important: Use the callee's context-free summary to propagate general sink properties
                        summaryData = fromMaybe emptySecurityRankSummaryData (Map.lookup [] calleeSummaryMap)
                        calleeSinks = srsSinks summaryData

                        newSinks = Map.foldlWithKey' (\acc calleeParamIndex sinkRank ->
                            let
                                argExpr = args !! calleeParamIndex
                            in
                                case unFix argExpr of
                                    C.VarExpr (C.L _ _ argName) ->
                                        case findIndex (== argName) paramNames of
                                            Just callerParamIndex -> Map.insertWith max callerParamIndex sinkRank acc
                                            Nothing -> acc
                                    _ -> acc
                          ) Map.empty calleeSinks
                    in
                        Map.unionWith max accSinks newSinks
                  ) Map.empty calleeNames
            in
                inferredSinks
    in
        foldl' (Map.unionWith max) Map.empty (map processCall allCalls)

-- | Joins two summaries, taking the min rank for each location.
joinSummaries :: SecurityRankSummaryData -> SecurityRankSummaryData -> SecurityRankSummaryData
joinSummaries (SecurityRankSummaryData a b d1) (SecurityRankSummaryData c d d2) =
    SecurityRankSummaryData (Map.unionWith min a c) (Map.unionWith min b d) (nub (d1 ++ d2))

buildSecurityRankSummaryFromAnnotation :: AnnotationMap -> FunctionName -> C.Node (C.Lexeme Text) -> SecurityRankSummary
buildSecurityRankSummaryFromAnnotation annotations funcName funcNode =
    let
        funcAnns = fromMaybe Map.empty (Map.lookup funcName annotations)
        paramNames = getParamNamesFromDef funcNode
        (outputRanks, sinks) = Map.foldlWithKey' (buildRanks paramNames) (Map.empty, Map.empty) funcAnns
    in
        Map.singleton [] (SecurityRankSummaryData outputRanks sinks [])
  where
    buildRanks paramNames (outs, sinks) key rank =
        case T.splitOn ":" key of
            ["source", "return"] ->
                (Map.insert (ReturnLocation funcName) rank outs, sinks)
            ["source", paramName] ->
                (Map.insert (DerefLocation (VarLocation paramName)) rank outs, sinks)
            ["sink", paramName] ->
                case findIndex (== paramName) paramNames of
                    Just i  -> (outs, Map.insert i rank sinks)
                    Nothing -> (outs, sinks)
            ["sink", _callee, "return"] ->
                (outs, sinks) -- This case seems incorrect, ignore for now.
            _ -> (outs, sinks)

-- | The transfer function for a single statement.
transferRank :: SecurityRankContext Text -> FunctionName -> SecurityRankState -> C.Node (C.Lexeme Text) -> (SecurityRankState, Set (FunctionName, Context))
transferRank ctx funcName currentState stmt = runState (execStateT (transferRank' ctx funcName stmt) currentState) Set.empty

transferRank' :: SecurityRankContext Text -> FunctionName -> C.Node (C.Lexeme Text) -> StateT SecurityRankState (State (Set (FunctionName, Context))) ()
transferRank' ctx funcName stmt = do
    st <- get
    let tracePrefix = "SecurityRank.transferRank' (" <> T.unpack funcName <> ", " <> groom (srcCurrentContext ctx) <> ")"
    dtraceM $ tracePrefix <> "\n  STMT: " <> groom stmt <> "\n  STATE BEFORE: " <> groom (srsTaintState st)
    go (unFix stmt)
  where
    nodeId :: NodeId
    nodeId = C.getNodeId stmt
    pctx = (srcPointsToContext ctx) { PointsTo.ptcCurrentContext = srcCurrentContext ctx }
    ptsCfg =
        let analyzedCfgs = PointsTo.ptcAnalyzedCfgs pctx
        in fromMaybe (fromMaybe (error $ "Points-to CFG not found for function " <> T.unpack funcName <> " in context " <> groom (srcCurrentContext ctx)) (Map.lookup (funcName, []) analyzedCfgs))
           (Map.lookup (funcName, srcCurrentContext ctx) analyzedCfgs)
    ptNode = fromMaybe (error "Statement not found in points-to CFG") $
             find (\n -> stmt `elem` cfgStmts n) (Map.elems ptsCfg)
    (ptsMapBefore, ptsMacrosBefore) =
        let
            blockStmts = cfgStmts ptNode
            inFacts = cfgInFacts ptNode
            mStmtIndex = findIndex (== stmt) blockStmts
        in
            case mStmtIndex of
                Just stmtIndex ->
                    let
                        stmtsBefore = take stmtIndex blockStmts
                        finalState = fst $ foldl' (\(accFacts, _) s ->
                            let (newFacts, _) = PointsTo.transferPointsToState pctx funcName accFacts s
                            in (newFacts, Set.empty))
                            (inFacts, Set.empty) stmtsBefore
                    in
                        (PointsTo.ptsMap finalState, PointsTo.ptsMacros finalState)
                Nothing ->
                    error $ "BUG: Statement not found in its own basic block: " ++ groom stmt
    ptsMap = ptsMapBefore

    go node = case node of
        C.ExprStmt expr -> go (unFix expr)

        C.AssignExpr lhs C.AopEq rhs@(Fix (C.UnaryExpr C.UopAddress (Fix (C.VarExpr (C.L _ _ rhsFuncName)))))
            | Map.member rhsFuncName (srcFuncDefs ctx) || Map.member rhsFuncName (srcFuncDecls ctx) -> do
                st <- get
                dtraceM $ "  FUNC_PTR_ASSIGN: " <> T.unpack rhsFuncName
                let lhsLoc = toAbstractLocation lhs
                let summaries = srcSummaries ctx
                let rhsSummary = fromMaybe Map.empty (Map.lookup rhsFuncName summaries)
                let rhsSummaryData = fromMaybe emptySecurityRankSummaryData (Map.lookup [] rhsSummary) -- Simplified: assume empty context
                dtraceM $ "  RHS_SUMMARY: " <> groom rhsSummaryData
                case Map.lookup lhsLoc (srsFptrSigs st) of
                    Nothing -> do
                        dtraceM "  FIRST_ASSIGN"
                        modify $ \s -> s { srsFptrSigs = Map.insert lhsLoc rhsSummaryData (srsFptrSigs s) }
                    Just expectedSummary -> do
                        dtraceM $ "  SUBSEQUENT_ASSIGN, EXPECTED: " <> groom expectedSummary
                        let expectedSinks = srsSinks expectedSummary
                        let rhsSinks = srsSinks rhsSummaryData
                        let allIndices = Set.toList (Set.union (Map.keysSet expectedSinks) (Map.keysSet rhsSinks))

                        forM_ allIndices $ \i -> do
                            let expectedRank = fromMaybe Safe (Map.lookup i expectedSinks)
                            let rhsRank = fromMaybe Safe (Map.lookup i rhsSinks)
                            when (rhsRank < expectedRank) $ do
                                let location = C.sloc (srcCurrentFile ctx) stmt
                                    diag = T.unpack location ++ ": security risk: incompatible function signature assigned to function pointer; expected sink rank " ++ show expectedRank ++ " for argument " ++ show i ++ ", but got rank " ++ show rhsRank
                                modify $ \s -> s { srsDiagnostics = srsDiagnostics s ++ [diag] }

                        let newSummary = joinSummaries expectedSummary rhsSummaryData
                        modify $ \s -> s { srsFptrSigs = Map.insert lhsLoc newSummary (srsFptrSigs s) }
                let rhsRank = evalRank ctx funcName (srsTaintState st) ptsMap rhs
                modify $ \s -> s { srsTaintState = Map.insert lhsLoc rhsRank (srsTaintState s) }

        C.AssignExpr lhs@(Fix (C.ArrayAccess _ _)) C.AopEq rhs -> do
            st <- get
            let lhsLoc = toAbstractLocation lhs
            let rhsRank = evalRank ctx funcName (srsTaintState st) ptsMap rhs
            dtraceM $ "  ASSIGN to ArrayAccess. LHS_LOC=" <> groom lhsLoc <> ", RHS_RANK=" <> groom rhsRank
            modify $ \s -> s { srsTaintState = Map.insert lhsLoc rhsRank (srsTaintState s) }


        C.AssignExpr lhs _ rhs@(Fix (C.FunctionCall callExpr args)) -> do
            st <- get
            let lhsLoc = toAbstractLocation lhs

            -- Process sinks and output parameters for the call
            let (calleePointsTo, _) = PointsTo.evalPointsToSet ptsMacrosBefore pctx (PointsTo.ptcLocalVars pctx) funcName ptsMap callExpr
            let calleeNames = mapMaybe getFuncNameFromLoc (Set.toList calleePointsTo)
            let newContext = pushContext kLimit nodeId (srcCurrentContext ctx)

            -- Use foldM to correctly thread the state when processing multiple callees
            stAfterCallees <- foldM (\currentState calleeName -> do
                lift $ modify (Set.insert (calleeName, newContext))
                let calleeSummary = fromMaybe Map.empty (Map.lookup calleeName (srcSummaries ctx))
                let summaryForContext = Map.lookup newContext calleeSummary
                let summaryForEmpty = Map.lookup [] calleeSummary
                let summaryData = fromMaybe (fromMaybe emptySecurityRankSummaryData summaryForEmpty) summaryForContext

                -- Check sinks using the state from *before* this specific call
                let diags = mapMaybe (checkSinkViolation st funcName callExpr args ptsMapBefore) (Map.toList (srsSinks summaryData))

                -- Apply output taints from the summary
                let newTaintState = applySummaryTaints currentState args calleeName summaryData
                return $ currentState { srsTaintState = newTaintState, srsDiagnostics = srsDiagnostics currentState ++ diags }
              ) st calleeNames

            -- Now that the state is updated, evaluate the rank of the RHS (the return value)
            let rhsRank = evalRank ctx funcName (srsTaintState stAfterCallees) ptsMapBefore rhs
            dtraceM $ "  ASSIGN from FunctionCall. LHS_LOC=" <> groom lhsLoc <> ", RHS_RANK=" <> groom rhsRank

            -- And finally, update the state with the assignment
            put $ stAfterCallees { srsTaintState = Map.insert lhsLoc rhsRank (srsTaintState stAfterCallees) }

        C.AssignExpr lhs _ rhs -> do
            st <- get
            let lhsLoc = toAbstractLocation lhs
            let rhsRank = evalRank ctx funcName (srsTaintState st) ptsMapBefore rhs
            dtraceM $ "  ASSIGN generic. LHS_LOC=" <> groom lhsLoc <> ", RHS_RANK=" <> groom rhsRank
            let newState =
                    case lhs of
                        Fix (C.MemberAccess base _) ->
                            let baseType = Nothing -- Placeholder for type lookup
                            in case baseType of
                                Just (C.TyUnion _) ->
                                    let unionMembers = ["tainted_member", "other_member"] -- Placeholder
                                    in foldl' (\s member -> Map.insert (FieldLocation (toAbstractLocation base) member) rhsRank s)
                                              (srsTaintState st)
                                              unionMembers
                                _ -> Map.insert lhsLoc rhsRank (srsTaintState st)
                        _ -> Map.insert lhsLoc rhsRank (srsTaintState st)
            modify $ \s -> s { srsTaintState = newState }

        C.VarDeclStmt (Fix (C.VarDecl _ (C.L _ _ varName) _)) (Just initializer) -> do
            st <- get
            let varLoc = VarLocation varName

            -- Special handling for function calls to propagate taint from summaries
            let finalRank = case unFix initializer of
                    C.FunctionCall callExpr args ->
                        let
                            (calleePointsTo, _) = PointsTo.evalPointsToSet ptsMacrosBefore pctx (PointsTo.ptcLocalVars pctx) funcName ptsMapBefore callExpr
                            calleeNames = mapMaybe getFuncNameFromLoc (Set.toList calleePointsTo)
                            newContext = pushContext kLimit (C.getNodeId initializer) (srcCurrentContext ctx)
                        in
                            foldl' (\currentRank calleeName ->
                                let
                                    calleeSummaryMap = fromMaybe Map.empty (Map.lookup calleeName (srcSummaries ctx))
                                    summaryForContext = Map.lookup newContext calleeSummaryMap
                                    summaryForEmpty = Map.lookup [] calleeSummaryMap
                                    summaryData = fromMaybe (fromMaybe emptySecurityRankSummaryData summaryForEmpty) summaryForContext
                                    (_, calleeDecl) = fromMaybe (error $ "Function not found: " ++ T.unpack calleeName) (Map.lookup calleeName (srcFuncDecls ctx))
                                    paramNames = getParamNamesFromDef calleeDecl
                                    argRanks = map (evalRank ctx funcName (srsTaintState st) ptsMapBefore) args
                                    substMap = Map.fromList $ zip (map VarLocation paramNames) argRanks
                                    substitute loc = fromMaybe Safe (Map.lookup loc substMap)

                                    summaryRank = foldl' (\acc (outLoc, outRank) ->
                                        case outLoc of
                                            ReturnLocation _ -> min acc outRank
                                            DerefLocation (VarLocation pName) ->
                                                min acc (substitute (VarLocation pName))
                                            _ -> acc
                                        ) Safe (Map.toList (srsOutputRanks summaryData))
                                in
                                    min currentRank summaryRank
                            ) Safe calleeNames
                    _ -> evalRank ctx funcName (srsTaintState st) ptsMapBefore initializer

            dtraceM $ "  VAR_DECL. VAR_LOC=" <> groom varLoc <> ", RHS_RANK=" <> groom finalRank
            modify $ \s -> s { srsTaintState = Map.insert varLoc finalRank (srsTaintState s) }

        C.FunctionCall callExpr args -> do
            st <- get
            dtraceM $ "  FUNCTION_CALL: STMT: " <> groom stmt
            dtraceM $ "  CONTEXT: " <> groom (srcCurrentContext ctx) <> "\n  PTS_CFG InFacts: " <> groom (cfgInFacts ptNode) <> "\n  PTS_MAP before evalPointsToSet: " <> groom ptsMapBefore
            let (calleePointsTo, _) = PointsTo.evalPointsToSet ptsMacrosBefore pctx (PointsTo.ptcLocalVars pctx) funcName ptsMapBefore callExpr
            let calleeNames = mapMaybe getFuncNameFromLoc (Set.toList calleePointsTo)
            dtraceM $ "  FUNCTION_CALL: " <> groom callExpr <> " -> " <> groom calleeNames
            let newContext = pushContext kLimit nodeId (srcCurrentContext ctx)

            -- Use foldM to correctly thread the state when processing multiple callees
            stAfterCallees <- foldM (\currentState calleeName -> do
                lift $ modify (Set.insert (calleeName, newContext))
                let calleeSummary = fromMaybe Map.empty (Map.lookup calleeName (srcSummaries ctx))
                let summaryForContext = Map.lookup newContext calleeSummary
                let summaryForEmpty = Map.lookup [] calleeSummary
                let summaryData = fromMaybe (fromMaybe emptySecurityRankSummaryData summaryForEmpty) summaryForContext
                dtraceM $ "  SUMMARY for " <> T.unpack calleeName <> ": " <> groom summaryData

                -- Check sinks using the state from *before* this specific call
                let diags = mapMaybe (checkSinkViolation st funcName callExpr args ptsMapBefore) (Map.toList (srsSinks summaryData))

                -- Apply output taints from the summary
                let newTaintState = applySummaryTaints currentState args calleeName summaryData
                dtraceM $ "  TAINT_STATE after summary: " <> groom newTaintState
                return $ currentState { srsTaintState = newTaintState, srsDiagnostics = srsDiagnostics currentState ++ diags }
              ) st calleeNames
            put stAfterCallees

        C.Return (Just expr) -> do
            st <- get
            let returnRank = evalRank ctx funcName (srsTaintState st) ptsMapBefore expr
            let funcAnns = fromMaybe Map.empty (Map.lookup funcName (srcAnnotations ctx))
            case Map.lookup "sink:return" funcAnns of
                Just expectedRank ->
                    when (returnRank < expectedRank) $ do
                        let location = C.sloc (srcCurrentFile ctx) stmt
                            diag = T.unpack location ++ ": security risk: tainted data of rank " ++ show returnRank ++ " returned from function with sink rank " ++ show expectedRank
                        dtraceM $ "  RETURN violation. " <> diag
                        modify $ \s -> s { srsDiagnostics = srsDiagnostics s ++ [diag] }
                Nothing -> return ()

        _ -> do
            return ()

    checkSinkViolation st _ callExpr args _ (paramIndex, sinkRank) =
        let arg = args !! paramIndex
            actualRank = evalRank ctx funcName (srsTaintState st) ptsMap arg
            location = C.sloc (srcCurrentFile ctx) stmt
            diag = "checkSinkViolation: callExpr=" <> groom callExpr <> " arg=" <> groom arg <> " actualRank=" <> groom actualRank <> " sinkRank=" <> groom sinkRank
            res = if actualRank < sinkRank
                  then Just $ T.unpack location ++ ": security risk: tainted data of rank " ++ show actualRank ++ " sent to sink of rank " ++ show sinkRank
                  else Nothing
        in dtrace (diag <> "\n  RESULT: " <> groom res) res

    applySummaryTaints st args calleeName summary =
        let (_, calleeDecl) = fromMaybe (error $ "Function not found: " ++ T.unpack calleeName) (Map.lookup calleeName (srcFuncDecls ctx))
            paramNames = getParamNamesFromDef calleeDecl
        in Map.foldlWithKey' (applyTaintForLoc st args paramNames) (srsTaintState st) (srsOutputRanks summary)

    applyTaintForLoc _ args paramNames acc outLoc rank =
        case outLoc of
            DerefLocation (VarLocation paramName) ->
                case findIndex (== paramName) paramNames of
                    Just i ->
                        let arg = args !! i
                            argLoc = toAbstractLocation arg
                        in Map.insert argLoc rank acc
                    Nothing -> acc
            VarLocation paramName ->
                case findIndex (== paramName) paramNames of
                    Just i -> Map.insert (toAbstractLocation (args !! i)) rank acc
                    Nothing -> acc
            _ -> acc

-- | Evaluates an expression to determine its security rank.
evalRank :: SecurityRankContext Text -> FunctionName -> TaintState -> PointsToMap -> C.Node (C.Lexeme Text) -> SecurityRank
evalRank ctx funcName currentState ptsMap node = go node
  where
    go n@(Fix expr) =
        let
            pctx = (srcPointsToContext ctx) { PointsTo.ptcCurrentContext = srcCurrentContext ctx }
        in case expr of
            C.VarExpr (C.L _ _ name) ->
                let
                    loc = VarLocation name
                    ownRank = fromMaybe Safe (Map.lookup loc currentState)
                    pointsToSet = fromMaybe Set.empty (Map.lookup loc ptsMap)
                    pointedToRanks = Set.map (\l -> fromMaybe Safe (Map.lookup l currentState)) pointsToSet
                    minPointedToRank = if Set.null pointedToRanks then Safe else Set.findMin pointedToRanks
                in
                    min ownRank minPointedToRank
            C.MemberAccess baseExpr (C.L _ _ memberName) ->
                let
                    baseLoc = toAbstractLocation baseExpr
                    baseRank = fromMaybe Safe (Map.lookup baseLoc currentState)
                    fieldRank = fromMaybe Safe (Map.lookup (FieldLocation baseLoc memberName) currentState)
                in
                    min baseRank fieldRank
            C.PointerAccess baseExpr (C.L _ _ memberName) ->
                let
                    (baseLocs, _) = PointsTo.evalPointsToSet (error "no macro context") pctx (PointsTo.ptcLocalVars pctx) funcName ptsMap baseExpr
                    baseRanks = Set.map (\l -> fromMaybe Safe (Map.lookup l currentState)) baseLocs
                    minBaseRank = if Set.null baseRanks then Safe else Set.findMin baseRanks
                    fieldRanks = Set.map (\l -> fromMaybe Safe (Map.lookup (FieldLocation l memberName) currentState)) baseLocs
                    minFieldRank = if Set.null fieldRanks then Safe else Set.findMin fieldRanks
                in
                    min minBaseRank minFieldRank
            C.ArrayAccess baseExpr _ ->
                let loc = toAbstractLocation n
                in fromMaybe (go baseExpr) (Map.lookup loc currentState)
            C.UnaryExpr C.UopDeref ptrExpr ->
                let
                    ptrLoc = toAbstractLocation ptrExpr
                    ptrRank = fromMaybe Safe (Map.lookup ptrLoc currentState)
                    pointsToSet = fromMaybe Set.empty (Map.lookup ptrLoc ptsMap)
                    pointedToRanks = Set.map (\loc -> fromMaybe Safe (Map.lookup loc currentState)) pointsToSet
                    minPointedToRank = if Set.null pointedToRanks then Safe else Set.findMin pointedToRanks
                in
                    min ptrRank minPointedToRank
            C.UnaryExpr C.UopAddress inner ->
                let innerLoc = toAbstractLocation inner
                in fromMaybe Safe (Map.lookup innerLoc currentState)
            C.FunctionCall callExpr args ->
                let
                    (calleePointsTo, _) = PointsTo.evalPointsToSet (error "no macro context") pctx (PointsTo.ptcLocalVars pctx) funcName ptsMap callExpr
                    calleeNames = mapMaybe getFuncNameFromLoc (Set.toList calleePointsTo)
                    newContext = pushContext kLimit (C.getNodeId n) (srcCurrentContext ctx)

                    applySummaryForCallee :: SecurityRank -> FunctionName -> SecurityRank
                    applySummaryForCallee currentRank calleeName =
                        let
                            calleeSummaryMap = fromMaybe Map.empty (Map.lookup calleeName (srcSummaries ctx))
                            summaryForContext = Map.lookup newContext calleeSummaryMap
                            summaryForEmpty = Map.lookup [] calleeSummaryMap
                            summaryData = fromMaybe (fromMaybe emptySecurityRankSummaryData summaryForEmpty) summaryForContext

                            -- Get the parameter names for the callee
                            (_, calleeDecl) = fromMaybe (error $ "Function not found: " ++ T.unpack calleeName) (Map.lookup calleeName (srcFuncDecls ctx))
                            paramNames = getParamNamesFromDef calleeDecl

                            -- Evaluate the rank of each argument at the call site
                            argRanks = map (evalRank ctx funcName currentState ptsMap) args

                            -- Create a substitution map from parameter locations to argument ranks
                            substMap = Map.fromList $ zip (map VarLocation paramNames) argRanks

                            -- Substitute parameter locations in the summary's output ranks
                            substitute loc = fromMaybe Safe (Map.lookup loc substMap)

                            finalRank = foldl' (\acc (outLoc, outRank) ->
                                case outLoc of
                                    ReturnLocation _ -> min acc outRank
                                    DerefLocation (VarLocation pName) ->
                                        min acc (substitute (VarLocation pName))
                                    -- Propagate taint from arguments to return value if the function just returns it
                                    VarLocation pName -> min acc (substitute (VarLocation pName))
                                    _ -> acc
                                ) Safe (Map.toList (srsOutputRanks summaryData))

                        in
                            min currentRank finalRank

                in
                    dtrace ("evalRank FunctionCall: " <> groom n <> " -> " <> groom calleeNames <> " => " <> groom (foldl' applySummaryForCallee Safe calleeNames)) $
                    foldl' applySummaryForCallee Safe calleeNames
            C.LiteralExpr {} -> Safe
            C.ParenExpr inner -> go inner
            C.CastExpr _ inner -> go inner
            C.BinaryExpr lhs _ rhs -> min (go lhs) (go rhs)
            C.TernaryExpr _ trueExpr falseExpr -> min (go trueExpr) (go falseExpr)
            _ -> Safe

-- | Finds all function declarations and definitions.
findFunctionDecls :: [(FilePath, [C.Node (C.Lexeme Text)])] -> Map FunctionName (FilePath, C.Node (C.Lexeme Text))
findFunctionDecls tus = execState (traverseAst collector tus) Map.empty
  where
    collector = astActions
        { doNode = \fp node act -> do
            case unFix node of
                C.FunctionDefn _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)) _ ->
                    modify (Map.insert name (fp, node))
                C.FunctionDecl _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)) ->
                    modify (Map.insert name (fp, node))
                _ -> return ()
            act
        }

-- | Finds all function definitions.
findFunctionDefs :: [(FilePath, [C.Node (C.Lexeme Text)])] -> Map FunctionName (FilePath, C.Node (C.Lexeme Text))
findFunctionDefs tus = execState (traverseAst collector tus) Map.empty
  where
    collector = astActions
        { doNode = \fp node act -> do
            case unFix node of
                C.FunctionDefn _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)) _ ->
                    modify (Map.insert name (fp, node))
                _ -> return ()
            act
        }

findStructDefs :: [C.Node (C.Lexeme Text)] -> Map Text (C.Node (C.Lexeme Text))
findStructDefs nodes = execState (traverseAst collector nodes) Map.empty
  where
    collector = astActions
        { doNode = \_ node act -> do
            case unFix node of
                C.Struct (C.L _ _ name) _ ->
                    modify (Map.insert name node)
                C.Typedef (Fix (C.Struct (C.L _ _ name) _)) _ ->
                    modify (Map.insert name node)
                _ -> return ()
            act
        }

getFuncNameFromDef :: C.Node (C.Lexeme Text) -> FunctionName
getFuncNameFromDef (Fix (C.FunctionDefn _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)) _)) = name
getFuncNameFromDef (Fix (C.FunctionDecl _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)))) = name
getFuncNameFromDef _ = error "Node is not a function definition or declaration"

getParamNamesFromDef :: C.Node (C.Lexeme Text) -> [Text]
getParamNamesFromDef (Fix (C.FunctionDefn _ (Fix (C.FunctionPrototype _ _ params)) _)) =
    mapMaybe getParamName params
getParamNamesFromDef (Fix (C.FunctionDecl _ (Fix (C.FunctionPrototype _ _ params)))) =
    mapMaybe getParamName params
getParamNamesFromDef _ = []

getParamName :: C.Node (C.Lexeme Text) -> Maybe Text
getParamName (Fix (C.VarDecl _ (C.L _ _ name) _)) = Just name
getParamName _                                    = Nothing

getParamTypesFromDef :: C.Node (C.Lexeme Text) -> [C.Node (C.Lexeme Text)]
getParamTypesFromDef (Fix (C.FunctionDefn _ (Fix (C.FunctionPrototype _ _ params)) _)) =
    mapMaybe getParamType params
getParamTypesFromDef (Fix (C.FunctionDecl _ (Fix (C.FunctionPrototype _ _ params)))) =
    mapMaybe getParamType params
getParamTypesFromDef _ = []

getParamType :: C.Node (C.Lexeme Text) -> Maybe (C.Node (C.Lexeme Text))
getParamType (Fix (C.VarDecl ty _ _)) = Just ty
getParamType _                        = Nothing

getStructMemberNames :: C.Node (C.Lexeme Text) -> [Text]
getStructMemberNames (Fix (C.Struct _ decls)) =
    mapMaybe getMemberName decls
getStructMemberNames (Fix (C.Typedef (Fix (C.Struct _ decls)) _)) =
    mapMaybe getMemberName decls
getStructMemberNames _ = []

getMemberName :: C.Node (C.Lexeme Text) -> Maybe Text
getMemberName (Fix (C.MemberDecl (Fix (C.VarDecl _ (C.L _ _ name) _)) _)) = Just name
getMemberName (Fix (C.Commented _ (Fix (C.MemberDecl (Fix (C.VarDecl _ (C.L _ _ name) _)) _)))) = Just name
getMemberName _ = Nothing

buildPointsToSummaryFromAnnotation :: AnnotationMap -> FunctionName -> C.Node (C.Lexeme Text) -> PointsTo.PointsToSummary
buildPointsToSummaryFromAnnotation annotations funcName _ =
    let
        funcAnns = fromMaybe Map.empty (Map.lookup funcName annotations)
        outputPointsTo' = Map.foldlWithKey' buildOutputPointsTo Map.empty funcAnns
    in
        Map.singleton [] (PointsToSummaryData Set.empty outputPointsTo')
  where
    buildOutputPointsTo acc key _ =
        case T.splitOn ":" key of
            ["source", paramName] ->
                Map.insert (DerefLocation (VarLocation paramName)) (Set.singleton (HeapLocation (hash funcName))) acc
            _ -> acc
    hash = fromIntegral . T.length -- Simple hash for demonstration
