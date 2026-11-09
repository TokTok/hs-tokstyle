{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.PartialFailure (descr) where

import           Control.Monad.State.Strict           (State)
import qualified Control.Monad.State.Strict           as State
import           Data.Fix                             (Fix (..))
import qualified Data.List                            as List
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (mapMaybe)
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import           Language.Cimple                      (AssignOp (..),
                                                       BinaryOp (..),
                                                       Lexeme (..), NodeF (..),
                                                       UnaryOp (..))
import qualified Language.Cimple                      as C
import           Language.Cimple.Diagnostics          (CimplePos,
                                                       Diagnostic (..),
                                                       DiagnosticLevel (..),
                                                       DiagnosticSpan (..),
                                                       HasDiagnosticInfo (..),
                                                       IsPosition (..),
                                                       warnRich)
import           Prettyprinter                        (pretty, (<+>))
import           Tokstyle.Analysis.AccessPath         (AccessPath (..),
                                                       isPathPrefixOf)
import           Tokstyle.Analysis.Dataflow           (Dataflow (..), solve)
import           Tokstyle.Cimple.Analysis.CFG         (EdgeType (..), Node (..),
                                                       NodeKind (..),
                                                       fromFunction)
import           Tokstyle.Cimple.Analysis.Destructors
import           Tokstyle.Cimple.Analysis.Functions
import           Tokstyle.Common                      (backticks)


-- | Allocation and reallocation functions, with the position of their
-- element count or size argument.
sizedAllocators :: [(Text, Int)]
sizedAllocators = Map.toList allocators ++ [("realloc", 1), ("mem_vrealloc", 2), ("mem_brealloc", 2)]

-- | Find the fields used as array lengths: those in the size argument of an
-- allocation, directly or through a parameter of the function allocating.
lengthFields :: [Function] -> Set Text
lengthFields fns = Set.fromList (concatMap fieldsIn (direct ++ throughWrappers))
  where
    calls = [ (f, name, args) | f <- fns, Fix (FunctionCall callee args) <- universe (fnDefn f), L _ _ name <- [calleeLexeme callee] ]
    sizeArg table name args = [ a | Just i <- [lookup name table], (a : _) <- [drop i args] ]
    direct = concat [ sizeArg sizedAllocators name args | (_, name, args) <- calls ]
    -- The parameters of each function that flow into an allocation size.
    wrappers = Map.fromListWith (++)
        [ (fnName f, [i])
        | (f, name, args) <- calls, a <- sizeArg sizedAllocators name args
        , (i, p) <- zip [0 ..] (fnParams f), p `elem` varsIn a ]
    throughWrappers = concat [ sizeArg [ (name, i) | i <- is ] name args | (_, name, args) <- calls, Just is <- [Map.lookup name wrappers] ]
    varsIn e = [ v | Fix (VarExpr (L _ _ v)) <- universe e ]
    fieldsIn e = [ m | Fix n <- universe e, Just m <- [field n] ]
    field = \case
        PointerAccess _ (L _ _ m) -> Just m
        MemberAccess _ (L _ _ m)  -> Just m
        _                         -> Nothing

lastField :: AccessPath -> Maybe Text
lastField = \case
    PathField _ m -> Just (Text.pack m)
    _             -> Nothing

-- | Dataflow state: the net change of each array length reached through a
-- pointer, and where it first changed.
type Changes = Map AccessPath (Integer, Lexeme Text)

-- | Get the lexeme a diagnostic for an expression points at.
anchor :: C.Node (Lexeme Text) -> Maybe (Lexeme Text)
anchor (Fix node) = case node of
    VarExpr l         -> Just l
    PointerAccess _ l -> Just l
    MemberAccess _ l  -> Just l
    ParenExpr e       -> anchor e
    ArrayAccess e _   -> anchor e
    _                 -> Nothing

-- | How much each function adds to array lengths when it succeeds, by the
-- parameter they're reached through. Only lengths that removals lower count.
type Grows = Map Text (Map AccessPath Integer)

data Context = Context
    { ctxShapes      :: Map Text Shape
    , ctxGrows       :: Grows
    , ctxLengths     :: Set Text
    , ctxDestructors :: Destructors
    , ctxUndoers     :: Set Text
    -- ^ Functions that failure returns call to undo a call that grew a length.
    }

-- | Replace the variable or parameter a path starts at.
reroot :: AccessPath -> AccessPath -> AccessPath
reroot new = \case
    PathField p m -> PathField (reroot new p) m
    PathIndex p i -> PathIndex (reroot new p) i
    PathDeref p   -> PathDeref (reroot new p)
    _             -> new

paramOf :: AccessPath -> Maybe Int
paramOf = \case
    PathParam i   -> Just i
    PathField p _ -> paramOf p
    PathIndex p _ -> paramOf p
    PathDeref p   -> paramOf p
    _             -> Nothing

evalExpr :: Context -> (AccessPath -> Bool) -> C.Node (Lexeme Text) -> Changes -> Changes
evalExpr ctx outlives = go
  where
    go (Fix node) s = case node of
        UnaryExpr op x | op `elem` [UopIncr, UopDecr] ->
            change x (if op == UopIncr then 1 else -1) (go x s)
        AssignExpr l AopPlus r | Just n <- intLit r -> change l n (go r s)
        AssignExpr l AopMinus r | Just n <- intLit r -> change l (negate n) (go r s)
        AssignExpr l _ r -> maybe id under (accessPath l) (go r s)
        FunctionCall callee args
            | gone@(_ : _) <- mapMaybe accessPath (destroyed (ctxDestructors ctx) (Fix node)) ->
                List.foldl' (flip under) (List.foldl' (flip go) s args) gone
            | L _ _ name <- calleeLexeme callee, Just m <- Map.lookup name (ctxGrows ctx) ->
                List.foldl' (\acc (q, n) -> maybe acc (\p -> changeAt p n (calleeLexeme callee) acc) (argPath args q))
                    (List.foldl' (flip go) s args) (Map.toList m)
        _ -> List.foldl' (flip go) s (foldr (:) [] node)
    -- Forget what is known about a path once it is reassigned or destroyed.
    under p = Map.filterWithKey (\k _ -> not (p `isPathPrefixOf` k))
    argPath args q = do
        i <- paramOf q
        (a : _) <- pure (drop i args)
        base <- accessPath a
        pure (reroot base q)
    change x n s = case (accessPath x, anchor x) of
        (Just p, Just at) -> changeAt p n at s
        _                 -> s
    changeAt p n at s
        | outlives p = Map.alter (bump n at) p s
        | otherwise  = s
    bump n at = \case
        Nothing        -> Just (n, at)
        Just (m, at')  | m + n == 0 -> Nothing
                       | otherwise  -> Just (m + n, at')

-- | Dataflow state: the length changes, the calls each local may hold the
-- result of, and the calls each local was looked up from.
type St = (Changes, Map Text (Set (Lexeme Text)), Map Text (Set (Lexeme Text)))

-- | The state at each exit of a function, with the expression it returns.
exits :: Context -> Function -> [(Node, Maybe (C.Node (Lexeme Text)), Changes)]
exits ctx fn =
    [ (n, returned n, if cleaned n then Map.filter (not . grown . snd) s else s)
    | (n, out) <- Map.toList cfg
    , any (isExit . snd) out
    , Just st <- [Map.lookup n result]
    , let (s, _, _) = stepSt n st
    ]
  where
    (entry, cfg) = fromFunction (fnDefn fn)
    outlives p = throughPointer p && maybe False (`Set.member` ctxLengths ctx) (lastField p)
    isExit (Node _ ExitNode) = True
    isExit _                 = False
    returned = \case
        Node _ (StmtNode (Fix (Return e))) -> e
        _                                  -> Nothing

    stepSt :: Node -> St -> St
    stepSt n (s, rs, found) =
        (List.foldl' (flip (evalExpr ctx outlives)) s (nodeExprs n), results rs n, derive rs found n)
    assigned n = case n of
        Node _ (StmtNode (Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ v) _)) ini))) -> [(v, bare <$> ini)]
        _ -> [ (v, Just (bare r)) | e <- nodeExprs n, Fix (AssignExpr (Fix (VarExpr (L _ _ v))) _ r) <- universe e ]
    results rs n = List.foldl' (\m (v, r) -> case r of
        Just (Fix (FunctionCall callee _)) -> Map.insert v (Set.singleton (calleeLexeme callee)) m
        _                                  -> Map.delete v m) rs (assigned n)
    derive rs found n = List.foldl' (\m (v, r) ->
        let ls = Set.unions [ l | Just (Fix (FunctionCall _ args)) <- [r], Fix (VarExpr (L _ _ w)) <- map bare args, Just l <- [Map.lookup w rs] ]
        in if Set.null ls then Map.delete v m else Map.insert v ls m) found (assigned n)
    dropFrom ls = Map.filter ((`Set.notMember` ls) . snd)
    -- Calling what other failure returns call to undo a call counts as undoing
    -- it.
    grown (L _ _ g) = g `Map.member` ctxGrows ctx
    cleaned = \case
        Node _ (StmtNode ret) -> maybe False (not . Set.null . Set.intersection (ctxUndoers ctx)) (lookup ret (cleanupsIn ctx fn))
        _                     -> False

    -- Nothing changed where a call failed or a lookup of its result found
    -- nothing, and nothing is left to undo where an array is empty.
    edge n e (s, rs, found) = case n of
        Node _ (BranchNode cond) ->
            let failed = Set.fromList [ l | c <- resolve rs cond, (l, Failure) <- knownOn c e ]
                missing = Set.unions [ ls | Just (v, nullOn) <- [nullTest var cond], nullOn == e, Just ls <- [Map.lookup v found] ]
            in (emptied cond e (dropFrom (Set.union failed missing) s), rs, found)
        _ -> (s, rs, found)
    emptied cond e s = case (unFix' (strip cond), e) of
        (BinaryExpr x BopEq r, TrueBranch) | intLit r == Just 0, Just p <- accessPath x -> Map.delete p s
        (BinaryExpr x BopNe r, FalseBranch) | intLit r == Just 0, Just p <- accessPath x -> Map.delete p s
        _ -> s
    var x = case unFix' (strip x) of
        VarExpr (L _ _ v) -> Just v
        _                 -> Nothing
    knownOn cond e = case (unFix' (strip cond), e) of
        (BinaryExpr l BopAnd r, TrueBranch) -> knownOn l e ++ knownOn r e
        (BinaryExpr l BopOr r, FalseBranch) -> knownOn l e ++ knownOn r e
        -- Comparing an ID with the one we expect treats any other as failure.
        (BinaryExpr (Fix (FunctionCall callee _)) op r, _)
            | L _ _ g <- calleeLexeme callee, Just IdShape <- Map.lookup g (ctxShapes ctx)
            , Just k <- intLit r, k >= 0, op `elem` [BopEq, BopNe] ->
                [(calleeLexeme callee, if (op == BopEq) == (e == TrueBranch) then Success else Failure)]
        _ -> case (branchOn (ctxShapes ctx) cond, e) of
            (Just (l, o), TrueBranch)  -> [(l, o)]
            (Just (l, o), FalseBranch) -> [(l, if o == Success then Failure else Success)]
            _                          -> []
    -- Also read the condition with each local replaced by the call it came
    -- from, so `if (rc < 0)` tests that call.
    resolve rs cond = cond :
        [ subst v l cond | (v, ls) <- Map.toList rs, l <- Set.toList ls ]
    subst v l (Fix node) = case node of
        VarExpr (L _ _ v') | v' == v -> Fix (FunctionCall (Fix (VarExpr l)) [])
        _                            -> Fix (fmap (subst v l) node)

    result = solve entry cfg Dataflow
        { transfer     = stepSt
        , edgeTransfer = edge
        , merge        = \(a, ra, fa) (b, rb, fb) ->
            (Map.unionWith (\x y -> if fst x /= 0 then x else y) a b, Map.unionWith Set.union ra rb, Map.unionWith Set.union fa fb)
        , initial      = (Map.empty, Map.empty, Map.empty)
        }

-- | Find the calls each failure return makes on something looked up from the
-- result of a call that grew a length.
cleanupsIn :: Context -> Function -> [(C.Node (Lexeme Text), Set Text)]
cleanupsIn ctx fn =
    [ (ret, Set.fromList [ name | Fix (FunctionCall callee args) <- concatMap onTheWay before
                                , L _ _ name <- [calleeLexeme callee], any (localIn found) args ])
    | (ret, before) <- failureReturns fn ]
  where
    found = Set.fromList [ v | (v, Fix (FunctionCall _ args)) <- given, any (localIn grown) args ]
    grown = Set.fromList
        [ v | (v, Fix (FunctionCall callee _)) <- given, L _ _ g <- [calleeLexeme callee], g `Map.member` ctxGrows ctx ]
    given = [ (v, bare r) | (v, Just r) <- localDecls (fnDefn fn) ]
        ++ [ (v, bare r) | (v, rs) <- Map.toList (assignments (fnDefn fn)), r <- rs ]
    localIn vs a = case bare a of
        Fix (VarExpr (L _ _ v)) -> Text.unpack v `Set.member` vs
        _                       -> False

-- | Find the failure returns of a function with the array lengths changed and
-- not restored on the way there.
failures :: Context -> Function -> [(C.Node (Lexeme Text), Changes)]
failures ctx fn =
    [ (ret, changes)
    | (Node _ (StmtNode ret@(Fix (Return e))), _, changes) <- exits ctx fn
    , isFailure fn e
    , ret `notElem` stopped
    , not (Map.null changes)
    ]
  where
    -- A return after `abort` or `LOGGER_FATAL` never runs.
    stopped = [ ret | (ret, before) <- failureReturns fn
                    , L _ _ g <- [ calleeLexeme callee | Fix (FunctionCall callee _) <- concatMap universe before ]
                    , g `elem` ["abort", "LOGGER_FATAL"] ]

-- | Find how each function changes the lengths in @shrunk@ when it succeeds.
growers :: Context -> Set Text -> [Function] -> Grows
growers ctx shrunk fns = Map.fromList
    [ (fnName f, m)
    | f <- fns
    , let param p = rootOf p >>= \v -> List.elemIndex (Text.pack v) (fnParams f)
          m = Map.unionsWith const
            [ Map.fromList [ (reroot (PathParam i) p, d) | (p, (d, _)) <- Map.toList changes
                           , maybe False (`Set.member` shrunk) (lastField p), Just i <- [param p] ]
            | (_, e, changes) <- exits ctx f
            , Success `elem` returnOutcomes (fnShape f) e ]
    , not (Map.null m)
    ]

-- | Find the fields some function lowers by a constant, as removing an entry
-- lowers a count.
shrunkFields :: [Function] -> Set Text
shrunkFields fns = Set.fromList
    [ Text.pack m
    | f <- fns, Fix node <- universe (fnDefn f)
    , Just x <- [lowered node], Just (PathField _ m) <- [accessPath x] ]
  where
    lowered = \case
        UnaryExpr UopDecr x                          -> Just x
        AssignExpr x AopMinus r | Just _ <- intLit r -> Just x
        _                                            -> Nothing

-- | Find the failure returns of a function, each with the statements before
-- it in its block.
failureReturns :: Function -> [(C.Node (Lexeme Text), [C.Node (Lexeme Text)])]
failureReturns fn =
    [ (ret, take i stmts)
    | Fix (CompoundStmt stmts) <- universe (fnDefn fn)
    , (i, ret@(Fix (Return e))) <- zip [0 :: Int ..] stmts
    , isFailure fn e
    ]

-- | The parts of a statement that can run before the statement after it,
-- leaving out blocks that end in a return.
onTheWay :: C.Node (Lexeme Text) -> [C.Node (Lexeme Text)]
onTheWay node@(Fix n) = case n of
    CompoundStmt stmts | (Fix Return{} : _) <- reverse stmts -> []
    _ -> node : concatMap onTheWay (foldr (:) [] n)

-- | What comes before at least two failure returns: the function's cleanup
-- convention.
conventions :: Ord a => [Set a] -> Set a
conventions befores = Map.keysSet . Map.filter (>= (2 :: Int)) $
    Map.fromListWith (+) [ (x, 1) | xs <- befores, x <- Set.toList xs ]

-- | Find destructor calls on a local immediately before failure returns that
-- are the function's cleanup convention.
failureCleanups :: Destructors -> Function -> Set (Text, String)
failureCleanups ds fn = conventions [ Set.fromList (cleanupsBefore ds fn before) | (_, before) <- failureReturns fn ]

-- | Find destructor calls on locals.
cleanupsBefore :: Destructors -> Function -> [C.Node (Lexeme Text)] -> [(Text, String)]
cleanupsBefore ds fn before =
    [ (name, v)
    | Fix (ExprStmt call@(Fix (FunctionCall callee _))) <- before
    , L _ _ name <- [calleeLexeme callee]
    , Fix (VarExpr (L _ _ v')) <- map strip (destroyed ds call)
    , let v = Text.unpack v', v `Set.member` locals
    ]
  where
    locals = pointerLocals fn

-- | Find the call each local got its value from, directly or as the index of
-- the slot it points at. A local given values by more than one call has none.
acquirers :: Function -> Map String Text
acquirers fn = Map.mapMaybe single . Map.fromListWith Set.union $
    [ (v, Set.singleton c) | (v, r) <- given, Just c <- [from r] ]
  where
    from r = case bare r of
        Fix (FunctionCall callee _) | L _ _ c <- calleeLexeme callee -> Just c
        _                                                           -> slotFrom calls r
    given = [ (v, r) | (v, Just r) <- localDecls (fnDefn fn) ]
        ++ [ (v, r) | (v, rs) <- Map.toList (assignments (fnDefn fn)), r <- rs ]
    calls = Map.mapMaybe single $ Map.fromListWith Set.union
        [ (v, Set.singleton c) | (v, r) <- given, Fix (FunctionCall callee _) <- [bare r], L _ _ c <- [calleeLexeme callee] ]
    single s = case Set.toList s of
        [c] -> Just c
        _   -> Nothing

-- | Find the call that gave the index of the slot a value points at.
slotFrom :: Map String Text -> C.Node (Lexeme Text) -> Maybe Text
slotFrom calls r = case bare r of
    Fix (UnaryExpr UopAddress (Fix (ArrayAccess _ i))) | Fix (VarExpr (L _ _ w)) <- bare i -> Map.lookup (Text.unpack w) calls
    _ -> Nothing

-- | Find the cleanups that callers of a function usually do when they fail
-- after it: at least twice, and more often than not, as callers of a lookup
-- seldom destroy what it found.
callerCleanups :: Destructors -> Map Text (Int, Set Text) -> [Function] -> Set (Text, Text)
callerCleanups ds statuses fns = Map.keysSet (Map.filterWithKey usual cleaned)
  where
    cleaned = Map.fromListWith (+)
        [ (k, 1 :: Int)
        | fn <- fns, let got = acquirers fn, (_, before) <- failureReturns fn
        , k <- Set.toList (Set.fromList [ (c, name) | (name, v) <- cleanupsBefore ds fn before, Just c <- [Map.lookup v got] ]) ]
    skipped = Map.fromListWith (+)
        [ (c, 1 :: Int)
        | fn <- fns, let got = acquirers fn, (_, owned) <- ownedAtFailures ds statuses fn
        , c <- Set.toList (Set.fromList (mapMaybe (`Map.lookup` got) (Set.toList owned))) ]
    usual (c, _) n = n >= 2 && n > Map.findWithDefault 0 c skipped

pointerLocals :: Function -> Set String
pointerLocals fn = Set.fromList
    [ Text.unpack v | Fix (VarDeclStmt (Fix (VarDecl ty (L _ _ v) _)) _) <- universe (fnDefn fn), isPointer ty ]
  where
    isPointer ty = not (null [ () | Fix (TyPointer _) <- universe ty ])

-- | Find functions returning a pointer that report a status through an
-- out-parameter, with the parameter's position and the status values that
-- come with a non-null pointer. A function qualifies only if it sets the
-- status right before every return, and no status value comes with both a
-- pointer and @nullptr@.
statusFunctions :: [Function] -> Map Text (Int, Set Text)
statusFunctions fns = Map.fromList
    [ (fnName f, (i, okValues))
    | f@Function{fnShape = PtrShape} <- fns
    , let said = [ (st, isNullLit e) | Fix (CompoundStmt stmts) <- universe (fnDefn f)
                                  , (k, Fix (Return (Just e))) <- zip [0 :: Int ..] stmts
                                  , let st = lastStatus (reverse (take k stmts)) ]
    , not (null said)
    , Just params <- [mapM fst said]
    , (p : rest) <- [map fst params]
    , all (== p) rest
    , Just i <- [List.elemIndex p (fnParams f)]
    , let okValues = Set.fromList [ c | ((_, c), False) <- zip params (map snd said) ]
          badValues = Set.fromList [ c | ((_, c), True) <- zip params (map snd said) ]
    , not (Set.null okValues)
    , Set.null (Set.intersection okValues badValues)
    ]
  where
    lastStatus = \case
        (Fix (ExprStmt e) : _) | Just st <- status e -> Just st
        _                                            -> Nothing
    status e = case unFix' e of
        AssignExpr (Fix (UnaryExpr UopDeref (Fix (VarExpr (L _ _ p))))) _ c -> (,) p <$> literal c
        FunctionCall callee [Fix (VarExpr (L _ _ p)), c]
            | L _ _ "SET_ERROR_PARAMETER" <- calleeLexeme callee -> (,) p <$> literal c
        _ -> Nothing

literal :: C.Node (Lexeme Text) -> Maybe Text
literal c = case (constName c, intLit c) of
    (Just t, _) -> Just t
    (_, Just n) -> Just (Text.pack (show n))
    _           -> Nothing

isFailure :: Function -> Maybe (C.Node (Lexeme Text)) -> Bool
isFailure fn e = Failure `elem` outcomes && Success `notElem` outcomes
  where outcomes = returnOutcomes (fnShape fn) e

-- | Find the failure returns of a function with the locals it may still own
-- there: assigned the result of a call, and since then not destroyed, stored
-- elsewhere or known to be null.
ownedAtFailures :: Destructors -> Map Text (Int, Set Text) -> Function -> [(C.Node (Lexeme Text), Set String)]
ownedAtFailures ds statuses fn =
    [ (ret, step n owned)
    | (n@(Node _ (StmtNode ret@(Fix (Return e)))), _) <- Map.toList cfg
    , isFailure fn e
    , Just owned <- [Map.lookup n result]
    ]
  where
    (entry, cfg) = fromFunction (fnDefn fn)
    locals = pointerLocals fn
    got = acquirers fn
    step n owned = case n of
        Node _ (StmtNode (Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ v) _)) ini))) ->
            acquire (Text.unpack v) ini owned
        _ -> List.foldl' (flip expr) owned (nodeExprs n)
    acquire v ini owned
        | Just (Fix FunctionCall{}) <- strip <$> ini, v `Set.member` locals = Set.insert v owned
        | Just _ <- slotFrom got =<< ini, v `Set.member` locals = Set.insert v owned
        | otherwise = Set.delete v owned
    expr e owned = List.foldl' (flip effect) owned (universe e)
    effect (Fix node) owned = case node of
        AssignExpr (Fix (VarExpr (L _ _ v))) _ r -> acquire (Text.unpack v) (Just r) owned
        -- Storing a local transfers its ownership to what it is stored in.
        AssignExpr _ _ r | Fix (VarExpr (L _ _ v)) <- strip r -> Set.delete (Text.unpack v) owned
        FunctionCall{} ->
            owned `Set.difference` Set.fromList [ Text.unpack v | Fix (VarExpr (L _ _ v)) <- map strip (destroyed ds (Fix node)) ]
        _ -> owned
    -- A local that is null on an edge needs no destroying there.
    edge n e owned = case n of
        Node _ (BranchNode cond) ->
            let byTest = case nullTest var cond of
                    Just (v, nullOn) | nullOn == e -> Set.delete v
                    _                              -> id
            in byTest (Set.difference owned (nullByStatus cond e))
        _ -> owned
    -- The locals whose accompanying status shows they are null on this edge.
    nullByStatus cond e = Set.fromList
        [ v
        | (v, (st, okValues)) <- Map.toList linked
        , Just isOk <- [statusOnEdge st okValues cond e]
        , not isOk
        ]
    statusOnEdge st okValues cond e = case (unFix' (strip cond), e) of
        (VarExpr (L _ _ x), CaseBranch c) | x == st, Just c' <- literal c -> Just (c' `Set.member` okValues)
        (BinaryExpr (Fix (VarExpr (L _ _ x))) op r, _) | x == st, Just c <- literal r -> case (op, e) of
            (BopEq, TrueBranch)  -> Just (c `Set.member` okValues)
            (BopEq, FalseBranch) | okValues == Set.singleton c -> Just False
            (BopNe, TrueBranch)  | okValues == Set.singleton c -> Just False
            (BopNe, FalseBranch) -> Just (c `Set.member` okValues)
            _                    -> Nothing
        _ -> Nothing
    -- Locals assigned from a status-reporting function, with the status
    -- variable and the status values that come with a non-null result.
    linked = Map.fromList
        [ (Text.unpack v, (st, okValues))
        | (v, r) <- [ (v, r) | Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ v) _)) (Just r)) <- universe (fnDefn fn) ]
                 ++ [ (v, r) | Fix (AssignExpr (Fix (VarExpr (L _ _ v))) _ r) <- universe (fnDefn fn) ]
        , Fix (FunctionCall callee args) <- [strip r]
        , L _ _ name <- [calleeLexeme callee]
        , Just (i, okValues) <- [Map.lookup name statuses]
        , (Fix (UnaryExpr UopAddress (Fix (VarExpr (L _ _ st)))) : _) <- [drop i args]
        ]
    var x = case unFix' (strip x) of
        VarExpr (L _ _ v) -> Just (Text.unpack v)
        _                 -> Nothing
    result = solve entry cfg Dataflow
        { transfer     = step
        , edgeTransfer = edge
        , merge        = Set.union
        , initial      = Set.empty
        }

-- | Find failure returns that may skip a cleanup: one the function does before
-- other failure returns, or one that other callers of the call a local came
-- from usually do (with that call).
skippedCleanups :: Destructors -> Map Text (Int, Set Text) -> Set (Text, Text) -> Function -> [(C.Node (Lexeme Text), (Text, String), Maybe Text)]
skippedCleanups ds statuses callers fn =
    [ found
    | (ret, owned) <- ownedAtFailures ds statuses fn
    , found <- [ (ret, c, Nothing) | c@(_, v) <- Set.toList pairs, v `Set.member` owned ]
        ++ [ (ret, (name, v), Just from)
           | v <- Set.toList owned, Just from <- [Map.lookup v got]
           , (from', name) <- Set.toList callers, from' == from
           , (name, v) `Set.notMember` pairs ]
    ]
  where
    pairs = failureCleanups ds fn
    got = acquirers fn

analyse :: [(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse tus = reverse $ State.execState (mapM_ report reports >> mapM_ reportSkipped skipped) []
  where
    fns = definitions tus
    lengths = lengthFields fns
    shapes = Map.fromList [ (fnName f, fnShape f) | f <- fns ]
    direct = Context shapes Map.empty lengths ds Set.empty
    grows = direct { ctxGrows = growers direct (Set.intersection lengths (shrunkFields fns)) fns }
    ctx = grows { ctxUndoers = Set.unions [ conventions (map snd (cleanupsIn grows f)) | f <- fns ] }
    reports = Map.toList $ Map.fromListWith (++)
        [ ((fnFile f, at), [(fnName f, p, ret)])
        | f <- fns
        , (ret, changes) <- failures ctx f
        , (p, (_, at)) <- Map.toList changes ]
    statuses = statusFunctions fns
    ds = destructors tus
    callers = callerCleanups ds statuses fns
    skipped = [ (f, ret, c, from) | f <- fns, (ret, c, from) <- skippedCleanups ds statuses callers f ]

reportSkipped :: (Function, C.Node (Lexeme Text), (Text, String), Maybe Text) -> State [Diagnostic CimplePos] ()
reportSkipped (fn, ret, (name, v), Just from) = do
    let (pos, len) = getDiagnosticInfo (fnFile fn) ret
    warnRich $ Diagnostic pos len ErrorLevel
        (backticks (pretty (fnName fn)) <+> "fails without calling" <+> backticks (pretty name)
            <+> "on" <+> backticks (pretty v) <> ", which other callers of" <+> backticks (pretty from) <+> "do when they fail after it")
        Nothing [DiagnosticSpan pos len ["fails here"]] []
reportSkipped (fn, ret, (name, v), Nothing) = do
    let file = fnFile fn
        (pos, len) = getDiagnosticInfo file ret
        cleanups = Map.elems $ Map.fromList
            [ ((posLine cp, posColumn cp), DiagnosticSpan cp cl ["called before another failure"])
            | (_, before) <- failureReturns fn
            , Fix (ExprStmt (Fix (FunctionCall callee args))) <- before
            , l@(L _ _ name') <- [calleeLexeme callee], name' == name
            , or [ True | Fix (VarExpr (L _ _ a)) <- map strip args, Text.unpack a == v ]
            , let (cp, cl) = getDiagnosticInfo file l ]
    warnRich $ Diagnostic pos len ErrorLevel
        (backticks (pretty (fnName fn)) <+> "fails without calling" <+> backticks (pretty name)
            <+> "on" <+> backticks (pretty v) <> ", which it does when it fails before this")
        Nothing (DiagnosticSpan pos len ["fails here"] : cleanups) []

report :: ((FilePath, Lexeme Text), [(Text, AccessPath, C.Node (Lexeme Text))]) -> State [Diagnostic CimplePos] ()
report ((file, at), sites) = case sites of
    [] -> return ()
    (fn, p, _) : _ -> do
        let (pos, len) = getDiagnosticInfo file at
            rets = Map.elems $ Map.fromList
                [ ((posLine rp, posColumn rp), DiagnosticSpan rp rl ["fails here"])
                | (_, _, ret) <- sites, let (rp, rl) = getDiagnosticInfo file ret ]
        warnRich $ Diagnostic pos len ErrorLevel
            (backticks (render p) <+> "is not changed back when" <+> backticks (pretty fn) <+> "fails")
            Nothing (DiagnosticSpan pos len ["changed here"] : rets) []

descr :: ([(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("partial-failure", Text.unlines
    [ "Checks that a function that fails leaves the lengths of the arrays it was"
    , "given as they were."
    , ""
    , "**Reason:** a caller that sees the failure goes on as if nothing changed."
    , "A length changed anyway no longer matches the array, and the work that was"
    , "meant to go with the change, such as telling a callback, never happens."
    , ""
    , "Also checks that a failure destroys what the failures before it destroy, or"
    , "what other callers usually destroy when they fail after the same call, so"
    , "that what the function made is not left behind for a caller who was told"
    , "nothing was made."
    ]))
