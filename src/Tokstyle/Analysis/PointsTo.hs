{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | This module implements an inter-procedural, context-sensitive, summary-based
-- points-to analysis for C code.
--
-- The analysis determines the set of abstract memory locations that each pointer
-- variable could point to at any given program point. This is a foundational
-- analysis required for many subsequent static analyses, such as taint tracking.
--
-- The core algorithm works as follows:
-- 1.  **AST Traversal**: It first traverses the Abstract Syntax Tree (AST) of the
--     entire program to find all function definitions and declarations.
-- 2.  **Worklist Algorithm**: It uses a worklist algorithm to iteratively analyze
--     functions until a fixed point is reached. The worklist contains pairs of
--     (FunctionName, Context), ensuring that functions are re-analyzed if their
--     calling context changes.
-- 3.  **Intra-procedural Analysis**: For each function, it performs a standard
--     forward dataflow analysis over its Control Flow Graph (CFG). The state
--     (or "facts") at each point is a `PointsToMap`, which maps abstract
--     locations to the set of locations they point to.
-- 4.  **Function Summaries**: After analyzing a function, it generates a
--     `PointsToSummary`. This summary captures the function's effect on pointers,
--     including what it returns and what side effects it has on pointers passed
--     as arguments. This is the key to scalability, as it avoids re-analyzing
--     a function's body at every call site.
-- 5.  **Context Sensitivity**: The analysis is context-sensitive, meaning it can
--     distinguish between different call sites of the same function. The `Context`
--     is a list of node IDs (hashes) representing the call stack. This allows for
--     more precise summaries and handling of recursion.
-- 6.  **Fixed-Point Iteration**: The worklist algorithm continues until the
--     summaries for all functions stabilize (i.e., no longer change). When a
--     function's summary changes, all of its callers are added back to the
--     worklist to be re-analyzed with the updated information. This process
--     guarantees that the analysis correctly handles complex interactions,
--     including mutual recursion.
module Tokstyle.Analysis.PointsTo
    ( PointsToMap
    , PointsToContext(..)
    , PointsToState(..)
    , PointsToSummary
    , PointsToSummaryData(..)
    , MacroDefinitionMap
    , buildPointsToContext
    , analyzeFunctionWithSummaries
    , toAbstractLocation
    , analyzeStatementForPointers -- Export for testing
    , fixpointSummaries
    , evalPointsToSet
    , transferPointsToState
    ) where

import           Control.Monad               (when)
import           Control.Monad.State.Strict  (State, evalState, execState, get,
                                              modify, put)
import           Data.Fix                    (Fix (..), foldFix)
import           Data.Foldable               (asum, foldl')
import           Data.List                   (find, findIndex)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromJust, fromMaybe, isJust,
                                              mapMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Debug.Trace                 (trace, traceShow)
import qualified Language.Cimple             as C
import           Language.Cimple.TraverseAst (AstActions (..), astActions,
                                              traverseAst)
import           Text.Groom                  (groom)
import           Tokstyle.Analysis.Context   (kLimit, pushContext)
import           Tokstyle.Analysis.DataFlow
import           Tokstyle.Analysis.Types     (AbstractLocation (..), CallGraph,
                                              CallSite (..), CallType (..),
                                              Context, FunctionName,
                                              PointsToMap, PointsToSummary,
                                              PointsToSummaryData (..),
                                              getCallers, lookupOrError,
                                              toAbstractLocation)
import           Tokstyle.Worklist

fakeTestSource :: FilePath
fakeTestSource = "test.c"

debugging :: Bool
debugging = False

dtrace :: String -> a -> a
dtrace msg x = if debugging then trace msg x else x

-- | A map from a macro's name to its AST definition node.
type MacroDefinitionMap = Map FunctionName (C.Node (C.Lexeme Text))

-- | The state for the points-to analysis, including the points-to map and
-- the set of currently defined macros.
data PointsToState = PointsToState
    { ptsMap    :: PointsToMap
    , ptsMacros :: MacroDefinitionMap
    } deriving (Eq, Show)

-- | The context for the inter-procedural analysis. This data structure holds
-- all the global information needed while analyzing the program.
data PointsToContext l = PointsToContext
    { -- | The global call graph of the program. This is initially built from
      -- direct calls and is updated dynamically as function pointers are resolved.
      ptcCallGraph        :: CallGraph
      -- | The current, evolving map of summaries for all functions. This is the
      -- central piece of state for the summary-based analysis. It's a map from
      -- function names to another map from calling contexts to the summary for
      -- that specific context.
    , ptcSummaries        :: Map FunctionName PointsToSummary
      -- | A map from function names to their AST definitions. Used to find the
      -- code for a function when it needs to be analyzed.
    , ptcFuncDefs         :: FunctionDefs
      -- | A map from function names to their AST declarations (or definitions).
      -- Used to check if an identifier is a function.
    , ptcFuncDecls        :: FunctionDefs
      -- | A map from struct/union names to their AST definitions.
    , ptcStructDefs       :: Map Text (C.Node (C.Lexeme l))
      -- | A map from variable names to their type AST nodes for the current scope.
    , ptcVarTypes         :: Map Text (C.Node (C.Lexeme l))
      -- | The current call stack context, used for context-sensitive analysis.
    , ptcCurrentContext   :: Context
      -- | A dynamically constructed call graph that tracks calls discovered
      -- during the analysis (e.g., through function pointers). This is more
      -- precise than the initial, static call graph.
    , ptcDynamicCallGraph :: Map (FunctionName, Context) (Set (FunctionName, Context))
      -- | A cache of the Control Flow Graphs (CFGs) that have been analyzed.
      -- This avoids rebuilding the CFG for the same function and context pair.
    , ptcAnalyzedCfgs     :: Map (FunctionName, Context) (CFG Text PointsToState)
      -- | The set of local variables (including parameters) for the function
      -- currently being analyzed. This is crucial for distinguishing between
      -- local and global variables.
    , ptcLocalVars        :: Set Text
    , ptcFileMacros       :: MacroDefinitionMap
    }

-- | The PointsTo analysis is an instance of the generic DataFlow framework.
instance DataFlow PointsToContext Text PointsToState where
    emptyFacts _ = PointsToState Map.empty Map.empty

    join _ (PointsToState map1 macros1) (PointsToState map2 macros2) =
        PointsToState
            { ptsMap = Map.unionWith Set.union map1 map2
            , ptsMacros = Map.union macros1 macros2
            }

    transfer = transferPointsToState

-- | The new top-level transfer function. It inspects the current statement
-- and either updates the macro map or delegates to the existing points-to logic.
transferPointsToState :: PointsToContext Text -> FunctionName -> PointsToState -> C.Node (C.Lexeme Text) -> (PointsToState, Set (FunctionName, Context))
transferPointsToState ctx funcName currentState stmt =
    case unFix stmt of
        -- A macro is defined. Add it to the current state's macro map.
        C.PreprocDefineMacro (C.L _ _ name) _ _ ->
            let newMacros = Map.insert name stmt (ptsMacros currentState)
            in dtrace ("Defining macro: " <> T.unpack name) (currentState { ptsMacros = newMacros }, Set.empty)

        -- A macro is undefined. Remove it from the current state's macro map.
        C.PreprocUndef (C.L _ _ name) ->
            let newMacros = Map.delete name (ptsMacros currentState)
            in (currentState { ptsMacros = newMacros }, Set.empty)

        -- This is a statement that can affect pointers.
        _ ->
            -- Delegate to a new function that handles pointer logic.
            -- Crucially, we pass the current, flow-sensitive macro map to it.
            let (newPtsMap, newWork) = analyzeStatementForPointers (ptsMacros currentState) ctx funcName (ptsMap currentState) stmt
            in (currentState { ptsMap = newPtsMap }, newWork)

-- | The transfer function for a single statement. It takes the current points-to
-- state and a statement, and returns the new state and a set of new functions
-- that need to be added to the worklist (due to being called).
analyzeStatementForPointers :: MacroDefinitionMap -> PointsToContext Text -> Text -> PointsToMap -> C.Node (C.Lexeme Text) -> (PointsToMap, Set (FunctionName, Context))
analyzeStatementForPointers currentMacros ctx funcName currentPtsMap stmt =
    let tracePrefix = "PointsTo.analyzeStatementForPointers (" <> T.unpack funcName <> "): "
    in dtrace (unlines [ tracePrefix <> "STATE_IN: " <> groom currentPtsMap
                       , "  STMT: " <> groom stmt
                       , "  LOCAL VARS: " <> groom (ptcLocalVars ctx)
                       ]) $ case unFix stmt of

    -- An expression statement just transfers the effects of the inner expression.
    C.ExprStmt expr -> analyzeStatementForPointers currentMacros ctx funcName currentPtsMap expr

    C.AssignExpr (Fix (C.CastExpr _ lhs)) op rhs ->
        analyzeStatementForPointers currentMacros ctx funcName currentPtsMap (Fix (C.AssignExpr lhs op rhs))

    -- Case: *p = &y or *p = q (Assignment to a dereferenced pointer)
    -- This is a "strong update" where we change what a location points to.
    C.AssignExpr (Fix (C.UnaryExpr C.UopDeref lhsPtr)) C.AopEq rhs ->
        let
            -- First, find out what `lhsPtr` points to. This gives us the locations
            -- on the heap or stack that we need to update.
            (lhsPointsToSet, work1) = evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName currentPtsMap lhsPtr
            -- Second, find out what the `rhs` expression points to.
            (rhsPointsToSet, work2) = case unFix rhs of
                C.VarExpr (C.L _ _ name) -> (Set.singleton (VarLocation name), Set.empty)
                _                        -> evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName currentPtsMap rhs
            -- For every location that `lhsPtr` can point to, we update its
            -- points-to set to be the `rhsPointsToSet`.
            updates = Map.fromList [ (loc, rhsPointsToSet) | loc <- Set.toList lhsPointsToSet ]
        in
            (dtrace (tracePrefix <> unlines [ "Assign *p="
                                            , "  STMT: " <> groom stmt
                                            , "  LHS points to: " <> groom lhsPointsToSet
                                            , "  RHS points to: " <> groom rhsPointsToSet
                                            , "  UPDATES: " <> groom updates
                                            ]) $ Map.union updates currentPtsMap, Set.union work1 work2)

    -- Case: p->field = &y or p->field = q (Assignment to a struct field via pointer)
    C.AssignExpr (Fix (C.PointerAccess ptrExpr (C.L _ _ fieldName))) C.AopEq rhs ->
        let
            -- Find out what the base pointer `ptrExpr` points to.
            (ptrPointsToSet, work1) = evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName currentPtsMap ptrExpr
            -- Find out what the `rhs` points to.
            (rhsPointsToSet, work2) = evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName currentPtsMap rhs
            -- For each abstract location `loc` that `ptrExpr` can point to, we
            -- update the points-to set of its field `fieldName`.
            updates = Map.fromList [ (FieldLocation loc fieldName, rhsPointsToSet) | loc <- Set.toList ptrPointsToSet ]
        in
            (dtrace (tracePrefix <> unlines ["Assign p->field=q"
                                            , "  STMT: " <> groom stmt
                                            , "  LHS points to: " <> groom ptrPointsToSet
                                            , "  RHS points to: " <> groom rhsPointsToSet
                                            , "  UPDATES: " <> groom updates
                                            ]) $ Map.union currentPtsMap updates, Set.union work1 work2)

    -- Case: s.field = &y (Assignment to a struct field directly)
    C.AssignExpr (Fix (C.MemberAccess structExpr (C.L _ _ fieldName))) C.AopEq rhs ->
        let
            structLoc = toAbstractLocation structExpr
            (rhsPointsToSet, work) = evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName currentPtsMap rhs
            finalRhsSet = if Set.null rhsPointsToSet
                          then case unFix rhs of
                                 C.VarExpr (C.L _ _ name) | Map.member name (ptcFuncDefs ctx) || Map.member name (ptcFuncDecls ctx) ->
                                     Set.singleton (FunctionLocation name)
                                 C.UnaryExpr C.UopAddress (Fix (C.VarExpr (C.L _ _ name))) | Map.member name (ptcFuncDefs ctx) || Map.member name (ptcFuncDecls ctx) ->
                                     Set.singleton (FunctionLocation name)
                                 _ -> rhsPointsToSet
                          else rhsPointsToSet

            -- Check if the base is a union type
            baseVarName = getBaseVarName structExpr
            baseTypeNode = Map.lookup baseVarName (ptcVarTypes ctx)
            (isUnion, updates) = case baseTypeNode of
                Just (Fix (C.TyUserDefined (C.L _ _ typeName))) ->
                    case Map.lookup typeName (ptcStructDefs ctx) of
                        Just (Fix (C.Union _ members)) ->
                            let memberNames = getMemberNames members
                            in (True, Map.fromList [ (FieldLocation structLoc member, finalRhsSet) | member <- memberNames ])
                        _ -> (False, Map.singleton (FieldLocation structLoc fieldName) finalRhsSet)
                Just (Fix (C.TyUnion (C.L _ _ typeName))) ->
                    case Map.lookup typeName (ptcStructDefs ctx) of
                        Just (Fix (C.Union _ members)) ->
                            let memberNames = getMemberNames members
                            in (True, Map.fromList [ (FieldLocation structLoc member, finalRhsSet) | member <- memberNames ])
                        _ -> (False, Map.singleton (FieldLocation structLoc fieldName) finalRhsSet)
                _ -> (False, Map.singleton (FieldLocation structLoc fieldName) finalRhsSet)
        in
            (dtrace (tracePrefix <> unlines [ "Assign s.field=q"
                                            , "  STMT: " <> groom stmt
                                            , "  LHS: " <> groom (FieldLocation structLoc fieldName)
                                            , "  RHS points to: " <> groom finalRhsSet
                                            , "  IS_UNION: " <> show isUnion
                                            , "  UPDATES: " <> groom updates
                                            ]) $ Map.unionWith Set.union currentPtsMap updates, work)

    -- Case: p = &x; (Address-of assignment)
    C.AssignExpr lhs C.AopEq (Fix (C.UnaryExpr C.UopAddress rhs)) ->
        let lhsLoc = toAbstractLocation lhs
            -- The RHS is the abstract location of the variable `x` itself.
            -- If `rhs` is a function, we create a `FunctionLocation`.
            rhsLoc = case unFix rhs of
                C.VarExpr (C.L _ _ name) | Map.member name (ptcFuncDefs ctx) || Map.member name (ptcFuncDecls ctx) -> FunctionLocation name
                _ -> toAbstractLocation rhs
        in (dtrace (tracePrefix <> unlines [ "Assign p=&x"
                                           , "  STMT: " <> groom stmt
                                           , "  LHS LOC: " <> groom lhsLoc
                                           , "  RHS LOC: " <> groom rhsLoc
                                           ]) $ Map.insert lhsLoc (Set.singleton rhsLoc) currentPtsMap, Set.empty)

    -- Case: q = p (pointer copy) or r = f(p) (assignment from function call)
    C.AssignExpr lhs C.AopEq rhs ->
        let lhsLoc = toAbstractLocation lhs
            -- Evaluate the RHS to find out what it points to.
            (rhsPointsTo, newWork) = evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName currentPtsMap rhs
            newState = if Set.null rhsPointsTo
                then case unFix rhs of
                   -- Handle assignment of a function name directly.
                   C.VarExpr (C.L _ _ name) | Map.member name (ptcFuncDefs ctx) ->
                       Map.insert lhsLoc (Set.singleton (FunctionLocation name)) currentPtsMap
                   C.VarExpr (C.L _ _ name) ->
                       Map.insert lhsLoc (fromMaybe (Set.singleton (VarLocation name)) (Map.lookup (VarLocation name) currentPtsMap)) currentPtsMap
                   -- If the RHS is a function call, we've already evaluated it.
                   C.FunctionCall _ _ ->
                       let (pointsTo, _) = evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName currentPtsMap rhs in
                       Map.insert lhsLoc pointsTo currentPtsMap
                   C.BinaryExpr {} ->
                       Map.insert lhsLoc Set.empty currentPtsMap
                   -- If the RHS points to nothing (e.g., `p = NULL`), we remove the
                   -- entry for the LHS variable, as it no longer points to anything.
                   _ -> Map.delete lhsLoc currentPtsMap
                else Map.insert lhsLoc rhsPointsTo currentPtsMap
        in (dtrace (tracePrefix <> unlines [ "Assign generic"
                                           , "  STMT: " <> groom stmt
                                           , "  LHS LOC: " <> groom lhsLoc
                                           , "  RHS points to: " <> groom rhsPointsTo
                                           ]) newState, newWork)

    -- Case: f(p) (standalone function call, handled for its side effects)
    C.FunctionCall callExpr args ->
        case unFix callExpr of
            C.VarExpr (C.L _ _ calleeName) ->
                dtrace ("FunctionCall: " <> T.unpack calleeName <> ", MACROS IN SCOPE: " <> groom (Map.keys currentMacros)) $
                case Map.lookup calleeName currentMacros of
                    Just macroDefNode ->
                        analyzeMacroBody macroDefNode args currentMacros ctx funcName currentPtsMap
                    Nothing ->
                        analyzeFunctionCall callExpr args currentMacros ctx funcName currentPtsMap
            C.LiteralExpr C.ConstId (C.L _ _ calleeName) ->
                dtrace ("FunctionCall (Literal): " <> T.unpack calleeName <> ", MACROS IN SCOPE: " <> groom (Map.keys currentMacros)) $
                case Map.lookup calleeName currentMacros of
                    Just macroDefNode ->
                        analyzeMacroBody macroDefNode args currentMacros ctx funcName currentPtsMap
                    Nothing ->
                        analyzeFunctionCall callExpr args currentMacros ctx funcName currentPtsMap
            _ -> analyzeFunctionCall callExpr args currentMacros ctx funcName currentPtsMap

    -- Case: int *p = get_x(); (Variable declaration with initialization)
    C.VarDeclStmt (Fix (C.VarDecl _ (C.L _ _ varName) _)) (Just initializer) ->
        let varLoc = VarLocation varName
            (rhsPointsTo, newWork) = evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName currentPtsMap (case unFix initializer of C.CastExpr _ inner -> inner; _ -> initializer)
            newState = if Set.null rhsPointsTo
                then Map.delete varLoc currentPtsMap
                else Map.insert varLoc rhsPointsTo currentPtsMap
        in (dtrace (tracePrefix <> unlines [ "VarDecl"
                                           , "  STMT: " <> groom stmt
                                           , "  VAR LOC: " <> groom varLoc
                                           , "  RHS points to: " <> groom rhsPointsTo
                                           ]) newState, newWork)

    -- Other statements don't affect points-to information in this simplified model.
    _ -> (currentPtsMap, Set.empty)

analyzeFunctionCall :: C.Node (C.Lexeme Text) -> [C.Node (C.Lexeme Text)] -> MacroDefinitionMap -> PointsToContext Text -> Text -> PointsToMap -> (PointsToMap, Set (FunctionName, Context))
analyzeFunctionCall callExpr args currentMacros ctx funcName currentPtsMap =
    let
        nodeId = C.getNodeId callExpr
        -- Determine the possible callees. This can be a direct call or an
        -- indirect call through a function pointer.
        (calleeNames, indirectCalleePointsTo, newWorkFromEval) = case unFix callExpr of
            C.VarExpr (C.L _ _ name) | Map.member name (ptcFuncDefs ctx) -> ([name], Set.empty, Set.empty)
            _ -> let (pointsTo, work) = evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName currentPtsMap callExpr in ([], pointsTo, work)

        -- Resolve function pointers to get a list of function names.
        indirectCalleeNames = mapMaybe (\loc -> case loc of FunctionLocation fname -> Just fname; _ -> Nothing) (Set.toList indirectCalleePointsTo)
        allCalleeNames = calleeNames ++ indirectCalleeNames

        -- Process the side effects of each possible callee.
        processCallee :: PointsToMap -> FunctionName -> PointsToMap
        processCallee current_state calleeName =
            let
                -- Create the new context for this specific call site.
                newContext = pushContext kLimit nodeId (ptcCurrentContext ctx)
                -- Look up the summary for the callee in the new context.
                summariesForCallee = fromMaybe Map.empty (Map.lookup calleeName (ptcSummaries ctx))
                summary = fromMaybe (fromMaybe emptySummaryData (Map.lookup [] summariesForCallee)) (Map.lookup newContext summariesForCallee)
                paramNames = getParamNamesFromDef (ptcFuncDefs ctx) calleeName

                -- `substitute` is a crucial helper function. It translates an abstract
                -- location from the callee's summary into the caller's context.
                substitute loc =
                    let
                        -- A helper to evaluate an argument at a call site, looking through casts.
                        evalArg arg = case unFix arg of
                            C.CastExpr _ innerExpr             -> evalArg innerExpr
                            C.UnaryExpr C.UopAddress innerExpr -> Set.singleton (toAbstractLocation innerExpr)
                            C.FunctionCall {} ->
                                fst $ evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName current_state arg
                            C.VarExpr _ ->
                                let argLoc = toAbstractLocation arg
                                in fromMaybe (Set.singleton argLoc) (Map.lookup argLoc current_state)
                            C.LiteralExpr {} -> Set.empty -- Literals don't point to anything.
                            C.SizeofType {} -> Set.empty -- sizeof(T) doesn't point to anything.
                            C.SizeofExpr {} -> Set.empty -- sizeof(e) doesn't point to anything.
                            C.BinaryExpr lhs C.BopPlus _ -> evalArg lhs
                            C.BinaryExpr {} -> Set.empty -- Other binary ops don't point to anything.
                            _ -> fromMaybe Set.empty $ Map.lookup (toAbstractLocation arg) current_state
                    in case loc of
                    -- If the location is a parameter, substitute it with what the
                    -- corresponding argument at the call site points to.
                    VarLocation vName ->
                        case findIndex (== vName) paramNames of
                            Just i -> evalArg (args !! i)
                            -- If it's not a parameter, it could be a global variable.
                            Nothing -> if Set.member vName (ptcLocalVars ctx)
                                       then Set.empty
                                       else Set.singleton (GlobalVarLocation vName)
                    -- Handle dereferences similarly.
                    DerefLocation (VarLocation pName) ->
                        case findIndex (== pName) paramNames of
                            Just i  -> evalArg (args !! i)
                            Nothing -> Set.singleton loc
                    -- Recursively substitute for field locations.
                    FieldLocation l f -> Set.map (`FieldLocation` f) (substitute l)
                    FunctionLocation fName -> Set.singleton (FunctionLocation fName)
                    _ -> Set.singleton loc

                -- In the summary, the RHS is a set of locations. We need to substitute each of them.
                -- Iterate over all side effects (outputPointsTo) in the summary.
                updates = Map.foldlWithKey' (\acc summaryLhs summaryRhs ->
                    let
                        -- Substitute the LHS of the summary's side effect.
                        callerLhsSet = substitute summaryLhs
                        -- Substitute the RHS. Any local variable from the callee's
                        -- summary that is not a parameter must be treated as a
                        -- global from the caller's perspective.
                        callerRhsSet = Set.unions $ map (\loc -> case loc of
                            VarLocation vName | vName `notElem` paramNames -> Set.singleton (GlobalVarLocation vName)
                            _ -> substitute loc
                            ) (Set.toList summaryRhs)
                        newUpdates = Map.fromSet (const callerRhsSet) callerLhsSet
                    in
                        Map.unionWith Set.union acc newUpdates
                    ) Map.empty (outputPointsTo summary)

            in
                -- Apply the updates to the caller's state.
                let updatedState = Map.unionWith Set.union updates current_state
                in dtrace ("processCallee for " <> T.unpack calleeName <> ":\n  UPDATES: " <> groom updates <> "\n  CURRENT_STATE: " <> groom current_state <> "\n  UPDATED_STATE: " <> groom updatedState) updatedState

        -- Fold over all possible callees and apply their summary effects.
        newState = foldl' processCallee currentPtsMap allCalleeNames
        -- Add all callees to the worklist for future analysis.
        newWork = Set.union newWorkFromEval $ Set.fromList $ map (\name -> (name, pushContext kLimit nodeId (ptcCurrentContext ctx))) allCalleeNames
    in
        (newState, newWork)

substituteInNode :: Map Text (C.Node (C.Lexeme Text)) -> C.Node (C.Lexeme Text) -> C.Node (C.Lexeme Text)
substituteInNode subMap = foldFix go
  where
    go (C.VarExpr lexeme@(C.L _ _ name)) =
        fromMaybe (Fix (C.VarExpr lexeme)) (Map.lookup name subMap)
    go other = Fix other

analyzeMacroBody :: C.Node (C.Lexeme Text) -> [C.Node (C.Lexeme Text)] -> MacroDefinitionMap -> PointsToContext Text -> FunctionName -> PointsToMap -> (PointsToMap, Set (FunctionName, Context))
analyzeMacroBody (Fix (C.PreprocDefineMacro _ macroParams body)) callArgs currentMacros ctx funcName initialPtsMap =
    let
        paramNames = mapMaybe (\case (Fix (C.MacroParam (C.L _ _ name))) -> Just name; _ -> Nothing) macroParams
        substitutionMap = Map.fromList $ zip paramNames callArgs
        substitutedBody = substituteInNode substitutionMap body

        -- The macro body can be a single statement or a compound statement.
        -- We need to handle both cases.
        getStmts (Fix (C.MacroBodyStmt (Fix (C.CompoundStmt stmts)))) = stmts
        getStmts (Fix (C.MacroBodyStmt stmt))                         = [stmt]
        getStmts _                                                    = []

        substitutedStmts = getStmts substitutedBody
    in
        foldl'
            (\(accMap, accWork) stmt ->
                let (newMap, newWork) = analyzeStatementForPointers currentMacros ctx funcName accMap stmt
                in (newMap, Set.union accWork newWork))
            (initialPtsMap, Set.empty)
            substitutedStmts
analyzeMacroBody _ _ _ _ _ currentPtsMap = (currentPtsMap, Set.empty)

-- | Checks if a variable name refers to a global variable within the current scope.
-- A variable is considered global if it's not a parameter and not in the set of
-- locally declared variables for the current function.
isGlobalVar :: Set Text -> [Text] -> Text -> Bool
isGlobalVar localVars params varName =
    let result = not (varName `elem` params) && not (Set.member varName localVars)
    in dtrace ("isGlobalVar: " <> T.unpack varName <> " in " <> groom params <> " with locals " <> groom localVars <> " -> " <> show result) result

-- | Evaluates an expression to determine the set of abstract locations it points to.
-- This is a key part of the transfer function, used to resolve the RHS of assignments
-- and the targets of function calls.
evalPointsToSet :: MacroDefinitionMap -> PointsToContext Text -> Set Text -> FunctionName -> PointsToMap -> C.Node (C.Lexeme Text) -> (Set AbstractLocation, Set (FunctionName, Context))
evalPointsToSet currentMacros ctx localVars funcName currentPtsMap expr =
    let result = go expr
    in dtrace (unlines [ "PointsTo.evalPointsToSet:"
                       , "  EXPR: " <> groom expr
                       , "  STATE: " <> groom currentPtsMap
                       , "  RESULT: " <> groom result
                       , "  LOCAL VARS: " <> groom localVars
                       ]) result
  where
    go localExpr@(Fix node) = dtrace ("PointsTo.evalPointsToSet.go: " <> show node) $ case node of

        -- Case: A direct function call, e.g., `f(x)`.
        C.FunctionCall callExpr@(Fix (C.VarExpr (C.L _ _ calleeName))) args ->
            case Map.lookup calleeName (ptsMacros (PointsToState currentPtsMap currentMacros)) of
                Just macroDefNode ->
                    let (newPtsMap, newWork) = analyzeMacroBody macroDefNode args currentMacros ctx funcName currentPtsMap
                    in (fromMaybe Set.empty (Map.lookup (toAbstractLocation callExpr) newPtsMap), newWork)
                Nothing ->
                    let
                        nodeId = C.getNodeId localExpr
                        newContext = pushContext kLimit nodeId (ptcCurrentContext ctx)
                        -- Look up the summary for the callee.
                        summariesForCallee = fromMaybe Map.empty (Map.lookup calleeName (ptcSummaries ctx))
                        summary = fromMaybe (fromMaybe emptySummaryData (Map.lookup [] summariesForCallee)) (Map.lookup newContext summariesForCallee)
                        paramNames = getParamNamesFromDef (ptcFuncDefs ctx) calleeName
                        -- The same substitution logic as in `analyzeStatementForPointers` is used here
                        -- to translate the summary's return values into the caller's context.
                        substitute loc = case loc of
                            VarLocation paramName ->
                                (case findIndex (== paramName) paramNames of
                                    Just i  -> (case unFix (args !! i) of
                                                   C.UnaryExpr C.UopAddress innerExpr ->
                                                       let innerLoc = case unFix innerExpr of
                                                               C.VarExpr (C.L _ _ name) | Map.member name (ptcFuncDefs ctx) || Map.member name (ptcFuncDecls ctx) -> FunctionLocation name
                                                               _ -> toAbstractLocation innerExpr
                                                       in Set.singleton innerLoc
                                                   C.FunctionCall {} ->
                                                       fst $ evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName currentPtsMap (args !! i)
                                                   C.VarExpr _ ->
                                                       let argLoc = toAbstractLocation (args !! i)
                                                       in fromMaybe (Set.singleton argLoc) (Map.lookup argLoc currentPtsMap)
                                                   _ -> fst $ evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName currentPtsMap (args !! i))
                                    Nothing -> if isGlobalVar localVars paramNames paramName
                                               then Set.singleton (GlobalVarLocation paramName)
                                               else Set.singleton (VarLocation paramName))
                            FunctionLocation fName -> Set.singleton (FunctionLocation fName)
                            DerefLocation (VarLocation paramName) ->
                                (case findIndex (== paramName) paramNames of
                                    Just i  -> (case unFix (args !! i) of
                                                   C.UnaryExpr C.UopAddress innerExpr ->
                                                       let innerLoc = case unFix innerExpr of
                                                               C.VarExpr (C.L _ _ name) | Map.member name (ptcFuncDefs ctx) || Map.member name (ptcFuncDecls ctx) -> FunctionLocation name
                                                               _ -> toAbstractLocation innerExpr
                                                       in Set.singleton innerLoc
                                                   C.FunctionCall {} ->
                                                       fst $ evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName currentPtsMap (args !! i)
                                                   C.VarExpr _ ->
                                                       let argLoc = toAbstractLocation (args !! i)
                                                       in fromMaybe (Set.singleton argLoc) (Map.lookup argLoc currentPtsMap)
                                                   _ -> fst $ evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName currentPtsMap (args !! i))
                                    Nothing -> Set.singleton loc)
                        -- A `ReturnLocation` in a summary means the function returns
                        -- one of its parameters. We substitute it with the actual
                        -- argument passed at the call site.
                            ReturnLocation fName ->
                                let calleeSummary = fromMaybe emptySummaryData (Map.lookup fName (ptcSummaries ctx) >>= \sm -> Map.lookup newContext sm)
                                in substituteSet (returnPointsTo calleeSummary)
                            _ -> Set.singleton loc
                        substituteSet = Set.unions . map substitute . Set.toList
                    in
                        -- Special handling for `malloc`: we treat it as returning a unique
                        -- heap location based on the call site's node ID.
                        if calleeName == "malloc"
                        then (Set.singleton (HeapLocation (C.getNodeId localExpr)), Set.empty)
                        -- Otherwise, apply the substitution to the summary's `returnPointsTo`.
                        else (substituteSet (returnPointsTo summary), Set.singleton (calleeName, newContext))

        -- Case: An indirect function call, e.g., `s->fp(x)`.
        C.FunctionCall (Fix (C.PointerAccess ptrExpr (C.L _ _ fieldName))) _ ->
            let
                -- First, evaluate the pointer expression to find potential structs.
                (ptrPointsTo, work1) = go ptrExpr
                -- Then, find the field location within those structs.
                fieldLocs = Set.map (\loc -> FieldLocation loc fieldName) ptrPointsTo
                -- Look up what those field locations point to, which should be functions.
                calleeLocs = Set.unions $ Set.map (\loc -> fromMaybe Set.empty (Map.lookup loc currentPtsMap)) fieldLocs
                calleeNames = mapMaybe (\case FunctionLocation name -> Just name; _ -> Nothing) (Set.toList calleeLocs)

                -- For each possible callee, get its return value from its summary.
                processCallee :: (Set AbstractLocation, Set (FunctionName, Context)) -> FunctionName -> (Set AbstractLocation, Set (FunctionName, Context))
                processCallee (accSet, accWork) calleeName =
                    let
                        nodeId = C.getNodeId localExpr
                        newContext = pushContext kLimit nodeId (ptcCurrentContext ctx)
                        summariesForCallee = fromMaybe Map.empty (Map.lookup calleeName (ptcSummaries ctx))
                        summary = fromMaybe (fromMaybe emptySummaryData (Map.lookup [] summariesForCallee)) (Map.lookup newContext summariesForCallee)
                    in
                        (Set.union accSet (returnPointsTo summary), Set.union accWork (Set.singleton (calleeName, newContext)))

            in
                foldl' processCallee (Set.empty, work1) calleeNames

        -- Case: `p->field`.
        C.PointerAccess ptrExpr (C.L _ _ fieldName) ->
            let
                -- Find what `ptrExpr` points to.
                (ptrPointsTo, work) = go ptrExpr
                -- Get the locations of the field for each of those base locations.
                fieldLocs = Set.map (\loc -> FieldLocation loc fieldName) ptrPointsTo
                -- Look up what those field locations point to.
                results = Set.unions $ Set.map (\loc -> fromMaybe Set.empty (Map.lookup loc currentPtsMap)) fieldLocs
            in dtrace (unlines [ "PointerAccess to " <> T.unpack fieldName
                               , "  ptrPointsTo: " <> groom ptrPointsTo
                               , "  fieldLocs: " <> groom fieldLocs
                               , "  results: " <> groom results
                               ]) (results, work)

        -- Case: `s.field`.
        C.MemberAccess structExpr (C.L _ _ fieldName) ->
            let
                structLoc = toAbstractLocation structExpr
                -- Check if the base is a union type
                baseVarName = getBaseVarName structExpr
                baseTypeNode = Map.lookup baseVarName (ptcVarTypes ctx)

                (isUnion, pointsToSet) = case baseTypeNode of
                    Just (Fix (C.TyUserDefined (C.L _ _ typeName))) ->
                        case Map.lookup typeName (ptcStructDefs ctx) of
                            Just (Fix (C.Typedef (Fix (C.Union _ members)) _)) ->
                                let memberNames = getMemberNames members
                                    memberLocs = [ FieldLocation structLoc member | member <- memberNames ]
                                in (True, Set.unions (mapMaybe (`Map.lookup` currentPtsMap) memberLocs))
                            Just (Fix (C.Union _ members)) ->
                                let memberNames = getMemberNames members
                                    memberLocs = [ FieldLocation structLoc member | member <- memberNames ]
                                in (True, Set.unions (mapMaybe (`Map.lookup` currentPtsMap) memberLocs))
                            _ -> (False, fromMaybe Set.empty $ Map.lookup (FieldLocation structLoc fieldName) currentPtsMap)
                    Just (Fix (C.TyUnion (C.L _ _ typeName))) ->
                        case Map.lookup typeName (ptcStructDefs ctx) of
                            Just (Fix (C.Union _ members)) ->
                                let memberNames = getMemberNames members
                                    memberLocs = [ FieldLocation structLoc member | member <- memberNames ]
                                in (True, Set.unions (mapMaybe (`Map.lookup` currentPtsMap) memberLocs))
                            _ -> (False, fromMaybe Set.empty $ Map.lookup (FieldLocation structLoc fieldName) currentPtsMap)
                    _ -> (False, fromMaybe Set.empty $ Map.lookup (FieldLocation structLoc fieldName) currentPtsMap)
            in
                dtrace (unlines [ "MemberAccess to " <> T.unpack fieldName
                                , "  structLoc: " <> groom structLoc
                                , "  isUnion: " <> show isUnion
                                , "  pointsToSet: " <> groom pointsToSet
                                ]) (pointsToSet, Set.empty)

        -- Case: A variable `x`.
        C.VarExpr (C.L _ _ name) ->
            -- If it's a known function, return its location.
            if Map.member name (ptcFuncDefs ctx) || Map.member name (ptcFuncDecls ctx)
            then (Set.singleton (FunctionLocation name), Set.empty)
            -- Otherwise, look it up in the current points-to map.
            else (fromMaybe Set.empty (Map.lookup (VarLocation name) currentPtsMap), Set.empty)

        -- Case: `&x`.
        C.UnaryExpr C.UopAddress inner ->
            case unFix inner of
                -- If taking the address of a function, return a `FunctionLocation`.
                C.VarExpr (C.L _ _ name) | Map.member name (ptcFuncDefs ctx) || Map.member name (ptcFuncDecls ctx) ->
                    (Set.singleton (FunctionLocation name), Set.empty)
                -- Otherwise, return the abstract location of the inner expression.
                _ -> (Set.singleton (toAbstractLocation inner), Set.empty)

        -- Case: `*p`.
        C.UnaryExpr C.UopDeref ptr ->
            -- First, find out what `ptr` points to.
            let (ptrPointsTo, work) = go ptr
            -- The result is the union of what all of those locations point to.
            in (Set.unions $ Set.map (\loc -> fromMaybe Set.empty (Map.lookup loc currentPtsMap)) ptrPointsTo, work)

        -- Case: `(T *)p`.
        C.CastExpr _ inner ->
            go inner

        -- Case: `p + 1`. For simplicity, we treat pointer arithmetic as returning
        -- a pointer to the same base location. This is an over-approximation but
        -- is often sufficient.
        C.BinaryExpr lhs C.BopPlus _ ->
            go lhs

        -- Other expressions don't point to anything in this model.
        _ -> (Set.empty, Set.empty)



getParamName :: C.Node (C.Lexeme Text) -> Maybe Text
getParamName (Fix (C.VarDecl _ (C.L _ _ name) _)) = Just name
getParamName _                                    = Nothing

getParamNamesFromDef :: Map Text (C.Node (C.Lexeme Text)) -> Text -> [Text]
getParamNamesFromDef funcDefs funcName =
    case Map.lookup funcName funcDefs of
        Just (Fix (C.FunctionDefn _ (Fix (C.FunctionPrototype _ _ params)) _)) ->
            mapMaybe getParamName params
        _ -> []

-- | A map from function names to their AST definitions.
type FunctionDefs = Map Text (C.Node (C.Lexeme Text))

-- | Finds all function definitions in the given translation units.
findFunctionDefs :: [(FilePath, [C.Node (C.Lexeme Text)])] -> FunctionDefs
findFunctionDefs tus = execState (traverseAst collector tus) Map.empty
  where
    collector = astActions
        { doNode = \_ node act -> do
            case unFix node of
                C.FunctionDefn _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)) _ ->
                    modify (Map.insert name node)
                _ -> return ()
            act
        }

-- | Finds all function declarations and definitions.
findFunctionDecls :: [(FilePath, [C.Node (C.Lexeme Text)])] -> FunctionDefs
findFunctionDecls tus = execState (traverseAst collector tus) Map.empty
  where
    collector = astActions
        { doNode = \_ node act -> do
            case unFix node of
                C.FunctionDefn _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)) _ ->
                    modify (Map.insert name node)
                C.FunctionDecl _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)) ->
                    modify (Map.insert name node)
                _ -> return ()
            act
        }

findToplevelMacros :: [C.Node (C.Lexeme Text)] -> MacroDefinitionMap
findToplevelMacros nodes =
    foldl' (\acc node -> case unFix node of
        C.PreprocDefineMacro (C.L _ _ name) _ _ -> Map.insert name node acc
        _                                       -> acc
    ) Map.empty nodes

-- | The main entry point for the inter-procedural points-to analysis. It sets
-- up the initial context and kicks off the fixed-point iteration.
buildPointsToContext :: [(FilePath, [C.Node (C.Lexeme Text)])] -> CallGraph -> Map FunctionName PointsToSummary -> PointsToContext Text
buildPointsToContext tus callGraph initialSummaries =
    let
        funcDefs = findFunctionDefs tus
        funcDecls = findFunctionDecls tus
        structDefs = findStructOrUnionDefs (concatMap snd tus)
        toplevelMacros = foldMap (findToplevelMacros . snd) tus

        -- The initial worklist contains all function definitions, each with an
        -- empty context, representing the most general analysis.
        worklist = fromList $ map (, []) (Map.keys funcDefs)

        initialContext = PointsToContext callGraph initialSummaries funcDefs funcDecls structDefs Map.empty [] Map.empty Map.empty Set.empty toplevelMacros
    in
        fixpointSummaries initialContext worklist

-- | Analyze a single function using the pre-computed summaries to get the final
-- points-to map at its exit points. This is useful for debugging or for
-- getting the final state of a specific function like `main`.
analyzeFunctionWithSummaries :: PointsToContext Text -> FunctionName -> PointsToMap
analyzeFunctionWithSummaries ctx funcName =
    case Map.lookup funcName (ptcFuncDefs ctx) of
        Nothing -> Map.empty
        Just funcDef ->
            let
                -- It's crucial to set the local variables for the function being
                -- analyzed, otherwise global/local distinction will be incorrect.
                localVars = Set.union (Set.fromList (getParamNamesFromDef (ptcFuncDefs ctx) funcName)) (findDeclaredVars funcDef)
                varTypes = findVarTypes funcDef
                ctxForFunc = ctx { ptcCurrentContext = [], ptcLocalVars = localVars, ptcVarTypes = varTypes }
                -- Run the intra-procedural fixed-point analysis for this function.
                (finalCfg, _) = fixpoint ctxForFunc funcName (buildCFG ctxForFunc funcDef (createInitialFacts ctxForFunc funcDef))
                -- Get the facts from all exit nodes of the CFG.
                exitNodes = filter (\n -> null (cfgSuccs (lookupOrError "analyzeFunctionWithSummaries exitNodes" finalCfg n))) (Map.keys finalCfg)
                exitFacts = map (ptsMap . cfgOutFacts . lookupOrError "analyzeFunctionWithSummaries exitFacts" finalCfg) exitNodes
            in
                -- Join the facts from all exit nodes to get the final points-to map.
                foldl' (Map.unionWith Set.union) Map.empty exitFacts



getFuncNameFromDef :: C.Node (C.Lexeme Text) -> Text
getFuncNameFromDef (Fix (C.FunctionDefn _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)) _)) = name
getFuncNameFromDef (Fix (C.FunctionDecl _ (Fix (C.FunctionPrototype _ (C.L _ _ name) _)))) = name
getFuncNameFromDef _ = error "Node is not a function definition or declaration"


-- | An empty points-to summary.
emptySummaryData :: PointsToSummaryData
emptySummaryData = PointsToSummaryData Set.empty Map.empty

-- | The fixed-point iteration loop for computing function summaries. This is the
-- heart of the inter-procedural analysis.
fixpointSummaries :: PointsToContext Text -> Worklist (FunctionName, Context) -> PointsToContext Text
fixpointSummaries initialCtx initialWorklist =
    let finalCtx = go initialCtx initialWorklist
        finalDynamicGraph = ptcDynamicCallGraph finalCtx
        -- After the analysis, convert the discovered dynamic call graph into the
        -- static `CallGraph` format for use by other analyses.
        finalStaticGraph = Map.foldlWithKey' (\acc (caller, _) dynCallees ->
                let
                    simpleCallees = Map.fromListWith Set.union $
                        mapMaybe (\(callee, calleeCtx) ->
                            if null calleeCtx then Nothing else
                            Just (callee, Set.singleton (CallSite (head calleeCtx) IndirectCall))
                        ) (Set.toList dynCallees)
                    existingCallees = fromMaybe Map.empty (Map.lookup caller acc)
                    newCallees = Map.unionWith Set.union existingCallees simpleCallees
                in
                    Map.insert caller newCallees acc
            ) Map.empty finalDynamicGraph
    in finalCtx { ptcCallGraph = finalStaticGraph }
  where
    go ctx worklist
        -- If the worklist is not empty, pop an item and process it.
        | Just ((funcName, context), worklist') <- pop worklist =
            -- Don't analyze functions without a definition.
            if Map.notMember funcName (ptcFuncDefs ctx)
            then go ctx worklist'
            else
                let
                    tracePrefix = "PointsTo.fixpointSummaries (" <> T.unpack funcName <> "): "
                    funcDefs = ptcFuncDefs ctx
                    funcDef = fromMaybe (error $ "Function not found: " ++ T.unpack funcName) (Map.lookup funcName funcDefs)
                    -- Determine the set of local variables for the current function.
                    localVars = Set.union (Set.fromList (getParamNamesFromDef funcDefs funcName)) (findDeclaredVars funcDef)
                    varTypes = findVarTypes funcDef

                    -- Create a context specifically for this intra-procedural analysis.
                    ctxForIntra = ctx { ptcCurrentContext = context, ptcLocalVars = localVars, ptcVarTypes = varTypes }

                    -- Create the initial dataflow facts for the function's entry point.
                    initialFacts = createInitialFacts ctxForIntra funcDef
                    -- Run the intra-procedural analysis to get the final CFG and any newly discovered callees.
                    (finalCfg, newCallees) = fixpoint ctxForIntra funcName (buildCFG ctxForIntra funcDef initialFacts)

                    -- Update the dynamic call graph with the new findings.
                    dynamicCallGraph' = Map.insert (funcName, context) newCallees (ptcDynamicCallGraph ctx)

                    -- Add the newly discovered callees to the worklist.
                    worklistWithCallees = pushList (Set.toList newCallees) worklist'

                    -- Generate a new summary for the function based on the analysis results.
                    newSummaryData = generateSummary ctxForIntra funcDef finalCfg

                    -- Get the old summary to check if anything has changed.
                    oldSummaries = fromMaybe Map.empty (Map.lookup funcName (ptcSummaries ctx))
                    oldSummaryData = fromMaybe emptySummaryData (Map.lookup context oldSummaries)

                    -- Update the global summary map with the new summary.
                    summaries' = Map.insert funcName (Map.insert context newSummaryData oldSummaries) (ptcSummaries ctx)

                    -- This is the core of the fixed-point logic:
                    -- If the summary has changed, we must re-analyze all functions
                    -- that call the current function (its dependents), because the
                    -- new information might change their analysis results.
                    worklist'' = if newSummaryData /= oldSummaryData
                                 then
                                     let dependents = Map.foldlWithKey' (\acc (caller, callerCtx) callees ->
                                                if Set.member (funcName, context) callees
                                                then Set.insert (caller, callerCtx) acc
                                                else acc
                                            ) Set.empty dynamicCallGraph'
                                     in dtrace (tracePrefix <> "Summary changed, adding dependents to worklist: " <> groom dependents) $ pushList (Set.toList dependents) worklistWithCallees
                                 else
                                     worklistWithCallees

                    -- Cache the analyzed CFG and update the context.
                    analyzedCfgs' = Map.insert (funcName, context) finalCfg (ptcAnalyzedCfgs ctx)
                    ctx' = ctx { ptcSummaries = summaries', ptcDynamicCallGraph = dynamicCallGraph', ptcAnalyzedCfgs = analyzedCfgs' }
                in
                    dtrace (tracePrefix <> "Processing " <> T.unpack funcName <> " in context " <> groom context) $
                    dtrace (tracePrefix <> "Worklist: " <> groom worklist'') $
                    dtrace (tracePrefix <> "Summaries: " <> groom summaries') $
                    -- Recurse with the updated context and worklist.
                    go ctx' worklist''
        -- If the worklist is empty, the analysis has reached a fixed point.
        | otherwise = ctx

isReturn :: C.Node (C.Lexeme Text) -> Bool
isReturn (Fix (C.Return _)) = True
isReturn _                  = False

-- | Given a return statement, find what it points to.
getReturnPointsTo :: MacroDefinitionMap -> PointsToContext Text -> Set Text -> FunctionName -> PointsToMap -> C.Node (C.Lexeme Text) -> Set AbstractLocation
getReturnPointsTo currentMacros ctx _ funcName facts (Fix (C.Return (Just expr))) =
    fst $ evalPointsToSet currentMacros ctx (ptcLocalVars ctx) funcName facts expr
getReturnPointsTo _ _ _ _ _ _ = Set.empty

-- | Generates a function summary from the final CFG of a function.
-- A summary abstracts the function's behavior into two parts:
-- 1. `returnPointsTo`: What the function's return value can point to.
-- 2. `outputPointsTo`: The side effects on pointers passed as arguments.
generateSummary :: PointsToContext Text -> C.Node (C.Lexeme Text) -> CFG Text PointsToState -> PointsToSummaryData
generateSummary ctx funcDef cfg =
    let
        funcName = getFuncNameFromDef funcDef
        paramNames = Set.fromList $ getParamNamesFromDef (ptcFuncDefs ctx) funcName
        -- The initial facts at the function's entry.
        initialFacts = ptsMap $ cfgInFacts (lookupOrError "generateSummary initialFacts" cfg 0)
        -- The final facts at the function's exit.
        exitNodeId = fromJust $ find (\n -> null (cfgSuccs (lookupOrError "generateSummary exitNodeId" cfg n))) (Map.keys cfg)
        finalFacts = ptsMap $ cfgOutFacts (lookupOrError "generateSummary finalFacts" cfg exitNodeId)

        -- Find all return statements in the CFG.
        returnStmts = concatMap (filter isReturn . cfgStmts) (Map.elems cfg)

        -- Collect the points-to sets from all return statements.
        returnSets = map (\stmt ->
            let node = fromMaybe (error "cannot find node for stmt") (find (\n -> stmt `elem` cfgStmts n) (Map.elems cfg))
                stmtsBefore = takeWhile (/= stmt) (cfgStmts node)
                (stateBefore, _) = foldl'
                    (\(accFacts, accWork) s ->
                        let (newFacts, newWork) = transferPointsToState ctx funcName accFacts s
                        in (newFacts, Set.union accWork newWork))
                    (cfgInFacts node, Set.empty)
                    stmtsBefore
            in getReturnPointsTo (ptsMacros stateBefore) ctx paramNames funcName (ptsMap stateBefore) stmt
            ) returnStmts
        finalReturnPointsTo = foldr Set.union Set.empty returnSets

        -- Create a reverse map from what parameters point to, back to the parameter itself.
        -- This is not currently used but could be useful for more complex summary generation.
        _paramDerefMap = Set.foldl' (\acc pName ->
                let
                    pLoc = VarLocation pName
                    pointsToSet = fromMaybe Set.empty (Map.lookup pLoc initialFacts)
                    derefParam = DerefLocation pLoc
                in
                    Set.foldl' (\m pointedToLoc -> Map.insert pointedToLoc derefParam m) acc pointsToSet
            ) Map.empty paramNames

        -- Find side effects on pointer parameters by comparing the initial and final
        -- points-to maps.
        outputMap = Map.foldlWithKey' (\acc loc val ->
            let
                -- Helper to find which parameter's data is being modified.
                _findParamForLoc l = Set.foldl' (\found pName ->
                    case found of
                        Just _  -> found
                        Nothing ->
                            let
                                pLoc = VarLocation pName
                                pointsToSet = fromMaybe Set.empty (Map.lookup pLoc initialFacts)
                            in if Set.member l pointsToSet then Just pName else Nothing
                  ) Nothing paramNames

                -- Build the LHS of the summary's side effect map. For example, if
                -- parameter `p` is modified via `*p = ...`, the summary LHS will be
                -- `DerefLocation(VarLocation("p"))`.
                buildSummaryLhs pName = case loc of
                    FieldLocation base field | Set.member base (fromMaybe Set.empty (Map.lookup (VarLocation pName) initialFacts)) ->
                        Just $ FieldLocation (DerefLocation (VarLocation pName)) field
                    _ -> if Set.member loc (fromMaybe Set.empty (Map.lookup (VarLocation pName) initialFacts))
                         then Just $ DerefLocation (VarLocation pName)
                         else Nothing

                -- Find which parameter's data is being modified.
                paramName = Set.foldl' (\found pName ->
                    case found of
                        Just _ -> found
                        Nothing -> if isJust (buildSummaryLhs pName) then Just pName else Nothing
                  ) Nothing paramNames
            in
            case paramName of
                Just p ->
                    let summaryLhs = fromJust (buildSummaryLhs p)
                        existingVal = fromMaybe Set.empty (Map.lookup summaryLhs acc)
                    in Map.insert summaryLhs (Set.union val existingVal) acc
                Nothing -> acc
            -- We only consider facts that have changed from the initial state.
            ) Map.empty finalFacts

        summary = PointsToSummaryData finalReturnPointsTo outputMap
    in
        dtrace (unlines [ "PointsTo.generateSummary (" <> T.unpack funcName <> "):"
                        , "  SUMMARY: " <> groom summary
                        ]) summary


getParamsFromDef :: C.Node (C.Lexeme Text) -> [C.Node (C.Lexeme Text)]
getParamsFromDef (Fix (C.FunctionDefn _ (Fix (C.FunctionPrototype _ _ params)) _)) = params
getParamsFromDef (Fix (C.FunctionDecl _ (Fix (C.FunctionPrototype _ _ params)))) = params
getParamsFromDef _ = []

-- | Given a callee and its context, find all the call sites (caller function,
-- caller context, and the call statement AST node) that could have resulted
-- in this call.
findCallersOf :: PointsToContext Text -> FunctionName -> Context -> [(FunctionName, Context, C.Node (C.Lexeme Text))]
findCallersOf ctx calleeName calleeContext =
    let
        dynamicCallGraph = ptcDynamicCallGraph ctx
        -- The context of the callee contains the node ID of the call site.
        callSiteNodeId = head calleeContext

        -- Helper to find the specific statement in a CFG that corresponds to a node ID.
        findCallingStmt :: CFG Text PointsToState -> Maybe (C.Node (C.Lexeme Text))
        findCallingStmt cfg =
            let allStmts = concatMap cfgStmts (Map.elems cfg)

                -- Search inside a statement for a node with the target nodeId (hash).
                -- If found, return the top-level statement itself.
                findNodeInStmt :: C.Node (C.Lexeme Text) -> Maybe (C.Node (C.Lexeme Text))
                findNodeInStmt topStmt =
                    let
                        collector = astActions {
                            doNode = \_ (node :: C.Node (C.Lexeme Text)) act -> do
                                -- If we found it, stop searching.
                                get >>= \case
                                    Just _ -> return ()
                                    Nothing -> do
                                        when (C.getNodeId node == callSiteNodeId) $ put (Just topStmt)
                                        act
                        }
                    in
                        execState (traverseAst collector [topStmt]) Nothing
            in
                -- Find the first statement that contains the node we're looking for.
                asum (map findNodeInStmt allStmts)

        -- Iterate through the dynamic call graph to find matching callers.
        callers = Map.foldlWithKey' (\acc (caller, callerCtx) callees ->
                dtrace ("findCallersOf: checking caller (" <> T.unpack caller <> ", " <> groom callerCtx <> ") with callees " <> groom callees) $
                if Set.member (calleeName, calleeContext) callees
                then case Map.lookup (caller, callerCtx) (ptcAnalyzedCfgs ctx) of
                        Just cfg -> case findCallingStmt cfg of
                            Just stmt -> (caller, callerCtx, stmt) : acc
                            Nothing   -> dtrace "findCallersOf: findCallingStmt failed" acc
                        Nothing -> dtrace "findCallersOf: CFG not found" acc
                else acc
            ) [] dynamicCallGraph
    in
        dtrace ("findCallersOf for " <> T.unpack calleeName <> " in context " <> groom calleeContext <> " found callers: " <> groom (map (\(f,c,_) -> (f,c)) callers)) callers

-- | Computes the initial points-to map for a callee function based on the state
-- at a specific call site in the caller.
computeFactsFromCallSite :: PointsToContext Text -> FunctionName -> (FunctionName, Context, C.Node (C.Lexeme Text)) -> PointsToState
computeFactsFromCallSite ctx calleeName (callerName, callerContext, callStmt) =
    let
        callerCfg = fromMaybe (error $ "CFG not found for " <> T.unpack callerName) (Map.lookup (callerName, callerContext) (ptcAnalyzedCfgs ctx))
        callNode = fromMaybe (error "call node not found in CFG") $ find (\n -> callStmt `elem` cfgStmts n) (Map.elems callerCfg)

        -- Re-calculate the state at the precise point *before* the call statement.
        stmtsBeforeCall = takeWhile (/= callStmt) (cfgStmts callNode)
        callerState = fst $ foldl'
            (\(accState, accWork) stmt ->
                let (newState, newWork) = transferPointsToState ctx callerName accState stmt
                in (newState, Set.union accWork newWork))
            (cfgInFacts callNode, Set.empty)
            stmtsBeforeCall

        -- Extract the arguments from the call statement.
        (_, args) = case unFix callStmt of
            C.ExprStmt (Fix (C.FunctionCall e a)) -> (e, a)
            C.ExprStmt (Fix (C.AssignExpr _ _ (Fix (C.FunctionCall e a)))) -> (e, a)
            C.ExprStmt (Fix (C.AssignExpr _ _ (Fix (C.CastExpr _ (Fix (C.FunctionCall e a)))))) -> (e, a)
            C.ExprStmt (Fix (C.AssignExpr _ _ (Fix (C.BinaryExpr (Fix (C.FunctionCall e a)) _ _)))) -> (e, a)
            C.AssignExpr _ _ (Fix (C.FunctionCall e a)) -> (e, a)
            C.AssignExpr _ _ (Fix (C.CastExpr _ (Fix (C.FunctionCall e a)))) -> (e, a)
            C.AssignExpr _ _ (Fix (C.BinaryExpr (Fix (C.FunctionCall e a)) _ _)) -> (e, a)
            C.FunctionCall e a                          -> (e, a)
            C.VarDeclStmt _ (Just (Fix (C.FunctionCall e a))) -> (e, a)
            C.VarDeclStmt _ (Just (Fix (C.CastExpr _ (Fix (C.FunctionCall e a))))) -> (e, a)
            _ -> error $ "Unhandled call statement structure: " ++ groom callStmt

        _calleeDef = fromMaybe (error $ "callee def not found: " ++ T.unpack calleeName) (Map.lookup calleeName (ptcFuncDefs ctx))
        paramNames = getParamNamesFromDef (ptcFuncDefs ctx) calleeName

        -- 1. Create initial parameter bindings: map each parameter of the callee
        --    to the points-to set of the corresponding argument from the caller.
        paramFactsList = zipWith (\pName arg ->
            let (pointsTo, work) = evalPointsToSet (ptsMacros callerState) ctx (ptcLocalVars ctx) callerName (ptsMap callerState) arg
            in ((VarLocation pName, pointsTo), work)
          ) paramNames args
        paramFacts = Map.fromList (map fst paramFactsList)
        _paramWork = Set.unions (map snd paramFactsList)

        -- 2. Create a worklist of all locations passed via parameters.
        worklist = Set.unions (Map.elems paramFacts)

        -- 3. Transitively copy all reachable facts from the caller's state.
        --    This is crucial for ensuring the callee has all the necessary
        --    information about the memory state it might interact with.
        go (acc, visited) work =
            case Set.minView work of
                Nothing -> acc
                Just (loc, restOfWork) ->
                    if Set.member loc visited
                    then go (acc, visited) restOfWork
                    else
                        let newVisited = Set.insert loc visited
                            pointsToSet = fromMaybe Set.empty (Map.lookup loc (ptsMap callerState))
                            -- Find all field locations based on the current location.
                            fieldFacts = Map.filterWithKey (\k _ -> case k of
                                FieldLocation base _ -> base == loc
                                _                    -> False) (ptsMap callerState)
                            -- Add the current location's points-to set AND its field facts to the accumulator.
                            newAcc = Map.unionsWith Set.union [acc, Map.singleton loc pointsToSet, fieldFacts]
                            -- Add the newly discovered locations from both the points-to set and the field values to the worklist.
                            newWorkItems = Set.unions [pointsToSet, Set.unions (Map.elems fieldFacts)]
                            newWork = Set.union restOfWork (Set.difference newWorkItems newVisited)
                        in go (newAcc, newVisited) newWork

        reachableFacts = go (Map.empty, Set.empty) worklist

        -- 4. Combine parameter bindings with the copied reachable facts.
        initialMap = Map.unionWith Set.union paramFacts reachableFacts
        initialState = PointsToState initialMap Map.empty
    in
        dtrace (unlines [ "computeFactsFromCallSite for " <> T.unpack calleeName <> " called by " <> T.unpack callerName
                        , "  CALL_STMT: " <> groom callStmt
                        , "  CALLER_STATE: " <> groom callerState
                        , "  INITIAL_FACTS: " <> groom initialState
                        ]) initialState

-- | Creates the initial data flow facts for a function analysis.
-- This function is context-aware.
createInitialFacts :: PointsToContext Text -> C.Node (C.Lexeme Text) -> PointsToState
createInitialFacts ctx funcDef =
    let
        funcName = getFuncNameFromDef funcDef
        calleeContext = ptcCurrentContext ctx
        initialMacros = ptcFileMacros ctx
    in
    if null calleeContext then
        -- If there's no context, this is an entry point (like `main`) or a
        -- function that hasn't been called yet. We create a generic initial
        -- state where each pointer parameter `p` points to `*p`.
        let
            toPointerParamFact :: C.Node (C.Lexeme Text) -> Maybe (AbstractLocation, Set AbstractLocation)
            toPointerParamFact (Fix (C.VarDecl (Fix (C.TyPointer _)) (C.L _ _ name) _)) =
                Just (VarLocation name, Set.singleton (DerefLocation (VarLocation name)))
            toPointerParamFact (Fix (C.VarDecl (Fix (C.TyStruct _)) (C.L _ _ name) _)) =
                Just (VarLocation name, Set.singleton (DerefLocation (VarLocation name)))
            toPointerParamFact _ = Nothing
            paramFacts = mapMaybe toPointerParamFact (getParamsFromDef funcDef)
        in PointsToState (Map.fromList paramFacts) initialMacros
    else
        -- If there is a context, we find all call sites that could lead to this
        -- function-context pair and join the facts computed from each call site.
        let
            callers = findCallersOf ctx funcName calleeContext
            factList = map (computeFactsFromCallSite ctx funcName) callers
            initialFacts = foldl' (join ctx) (emptyFacts ctx) factList
        in
            dtrace (unlines [ "createInitialFacts for " <> T.unpack funcName <> " in context " <> groom calleeContext
                            , "  CALLERS: " <> groom callers
                            , "  FACTS: " <> groom initialFacts
                            ]) initialFacts

-- | Traverses a function's body to find all locally declared variables.
findDeclaredVars :: C.Node (C.Lexeme Text) -> Set Text
findDeclaredVars (Fix (C.FunctionDefn _ _ body)) = execState (traverseAst collector [(fakeTestSource, [body])]) Set.empty
  where
    collector = astActions
        { doNode = \_ node act -> do
            case unFix node of
                C.VarDecl _ (C.L _ _ name) _ -> modify (Set.insert name)
                _                            -> pure ()
            act
        }
findDeclaredVars _ = Set.empty

-- Helper functions for union type analysis
getBaseVarName :: C.Node (C.Lexeme Text) -> Text
getBaseVarName (Fix (C.VarExpr (C.L _ _ name))) = name
getBaseVarName (Fix (C.MemberAccess base _))    = getBaseVarName base
getBaseVarName (Fix (C.PointerAccess base _))   = getBaseVarName base
getBaseVarName _                                = ""

getMemberNames :: [C.Node (C.Lexeme Text)] -> [Text]
getMemberNames = mapMaybe getMemberName
  where
    getMemberName (Fix (C.MemberDecl (Fix (C.VarDecl _ (C.L _ _ name) _)) _)) = Just name
    getMemberName _                                                          = Nothing

findVarTypes :: C.Node (C.Lexeme Text) -> Map Text (C.Node (C.Lexeme Text))
findVarTypes (Fix (C.FunctionDefn _ _ body)) = execState (traverseAst collector [(fakeTestSource, [body])]) Map.empty
  where
    collector = astActions
        { doNode = \_ node act -> do
            case unFix node of
                C.VarDecl ty (C.L _ _ name) _ -> modify (Map.insert name ty)
                _                             -> pure ()
            act
        }
findVarTypes _ = Map.empty

findStructOrUnionDefs :: [C.Node (C.Lexeme Text)] -> Map Text (C.Node (C.Lexeme Text))
findStructOrUnionDefs nodes = execState (traverseAst collector [(fakeTestSource, nodes)]) Map.empty
  where
    collector = astActions
        { doNode = \_ node act -> do
            case unFix node of
                C.Typedef defNode@(Fix (C.Struct (C.L _ _ name) _)) (C.L _ _ tyName) -> do
                    modify (Map.insert name defNode)
                    modify (Map.insert tyName defNode)
                C.Typedef defNode@(Fix (C.Union (C.L _ _ name) _)) (C.L _ _ tyName) -> do
                    modify (Map.insert name defNode)
                    modify (Map.insert tyName defNode)
                C.AggregateDecl defNode@(Fix (C.Struct (C.L _ _ name) _)) ->
                    modify (Map.insert name defNode)
                C.AggregateDecl defNode@(Fix (C.Union (C.L _ _ name) _)) ->
                    modify (Map.insert name defNode)
                _ -> return ()
            act
        }
