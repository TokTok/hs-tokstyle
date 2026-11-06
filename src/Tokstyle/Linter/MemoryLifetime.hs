{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
-- | What functions allocate, free and store, followed through their bodies:
-- memory a function loses, and memory it frees while it is still stored.
module Tokstyle.Linter.MemoryLifetime (descr) where

import qualified Control.Monad.State.Strict         as State
import           Data.Fix                           (Fix (..))
import qualified Data.List                          as List
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Language.Cimple                    (BinaryOp (..), Lexeme (..),
                                                     NodeF (..), UnaryOp (..))
import qualified Language.Cimple                    as C
import           Language.Cimple.Diagnostics        (CimplePos, Diagnostic (..),
                                                     DiagnosticLevel (..),
                                                     DiagnosticSpan (..),
                                                     HasDiagnosticInfo (..),
                                                     warnRich)
import           Prettyprinter                      (pretty, (<+>))
import           Tokstyle.Analysis.AccessPath       (AccessPath (..),
                                                     isPathPrefixOf)
import           Tokstyle.Analysis.Dataflow         (Dataflow (..), solve,
                                                     solveBackward)
import           Tokstyle.Cimple.Analysis.CFG       (EdgeType (..), Node (..),
                                                     NodeKind (..),
                                                     fromFunction)
import           Tokstyle.Cimple.Analysis.Functions
import           Tokstyle.Common                    (backticks)


data Summary = Summary
    { sumFrees   :: Map Text (Map Outcome (Set Int))
    -- ^ The arguments each function frees, by the outcome it reports.
    , sumFresh   :: Set Text
    -- ^ The functions that may return memory they allocated.
    , sumShapes  :: Map Text Shape
    , sumKept    :: Set Text
    -- ^ The functions that may return memory they also stored somewhere.
    , sumUnlinks :: Map Text (Set Int)
    -- ^ The arguments each function may remove from where they are stored,
    -- by writing through a pointer other than the argument.
    , sumKeeps   :: Map Text (Map Outcome (Set Int))
    -- ^ The arguments each function may store somewhere, by the outcome it
    -- reports.
    }

keptBy :: Summary -> Text -> [Outcome] -> [Int]
keptBy summary g outcomes = Set.toList . Set.unions $
    [ Map.findWithDefault Set.empty o byOutcome
    | Just byOutcome <- [Map.lookup g (sumKeeps summary)], o <- outcomes ]

freedBy :: Summary -> Text -> [Outcome] -> [Int]
freedBy summary g outcomes = case Map.lookup g freers of
    Just i  -> [i]
    Nothing -> Set.toList . Set.unions $
        [ Map.findWithDefault Set.empty o byOutcome
        | Just byOutcome <- [Map.lookup g (sumFrees summary)], o <- outcomes ]

summarise :: [Function] -> Summary
summarise fns = withUnlinks (bottomUp callees add look fns (Summary Map.empty fresh shapes Set.empty Map.empty Map.empty))
  where
    withUnlinks s0 = s0 { sumUnlinks = unlinksFix Map.empty }
    unlinksFix u = let u' = List.foldl' unlinkStep u fns in if u' == u then u else unlinksFix u'
    unlinkStep u fn = case Set.fromList [ i | (i, param) <- zip [0 ..] (fnParams fn), unlinks u fn (Text.unpack param) ] of
        is | Set.null is -> u
           | otherwise   -> Map.insert (fnName fn) is u
    -- A write through a pointer not rooted at the argument, here or in a
    -- function that removes one of its own arguments.
    unlinks u fn param = any (`Map.member` u) (callees fn) || or
        [ True
        | Fix (AssignExpr l _ _) <- universe (fnDefn fn), Just p <- [accessPath l], throughPointer p
        , Just root <- [rootOf p], root /= param ]
    shapes = Map.fromList [ (fnName f, fnShape f) | f <- fns ]
    add s f =
        let (_, frees, keepsArgs, keepsResult) = analyseFunction s f
        in s { sumFrees = insertNonEmpty (fnName f) frees (sumFrees s)
             , sumKeeps = insertNonEmpty (fnName f) keepsArgs (sumKeeps s)
             , sumKept = if keepsResult then Set.insert (fnName f) (sumKept s) else sumKept s }
    insertNonEmpty k v m
        | Map.null v = m
        | otherwise  = Map.insert k v m
    look s f = (Map.lookup (fnName f) (sumFrees s), Map.lookup (fnName f) (sumKeeps s), fnName f `Set.member` sumKept s)

    fresh = fixpoint (Map.keysSet allocators)
    fixpoint s = let s' = List.foldl' freshStep s fns in if s' == s then s else fixpoint s'
    freshStep s fn
        | any (callsOneOf s) (returned fn) = Set.insert (fnName fn) s
        | otherwise = s
    returned fn = concat
        [ case bare e of
            Fix (VarExpr (L _ _ v)) -> givenTo fn (Text.unpack v)
            e'                      -> [e']
        | Fix (Return (Just e)) <- universe (fnDefn fn) ]
    -- A local the function also stores somewhere holds memory that is not
    -- the caller's alone.
    givenTo fn v
        | v `elem` stored fn = []
        | otherwise = [ r | (v', Just r) <- localDecls (fnDefn fn), v' == v ]
            ++ Map.findWithDefault [] v (assignments (fnDefn fn))
    stored fn = [ v | Fix (AssignExpr l _ r) <- universe (fnDefn fn), not (isVar l), v <- passed r ]
    isVar = \case
        Fix (VarExpr{}) -> True
        _               -> False

callsOneOf :: Set Text -> C.Node (Lexeme Text) -> Bool
callsOneOf fns e = case bare e of
    Fix (FunctionCall callee _) | L _ _ g <- calleeLexeme callee -> g `Set.member` fns
    _                                                            -> False

data Facts = Facts
    { freed      :: Set AccessPath
    , owned      :: Map AccessPath (Lexeme Text)
    -- ^ Fields the function allocated that may still hold that memory, and
    -- where it allocated it.
    , held       :: Map String (Lexeme Text)
    -- ^ Locals that may hold memory the function must free, and where they
    -- got it.
    , shared     :: Map String (Lexeme Text)
    -- ^ Locals that may hold memory a call also stored, and that call.
    , keptParams :: Set String
    -- ^ Parameters the function may have stored somewhere lasting.
    , keptLocals :: Set String
    -- ^ Locals the function may have stored somewhere lasting itself.
    } deriving (Eq)

joinFacts :: Facts -> Facts -> Facts
joinFacts (Facts a b c d e f) (Facts g h i j k l) =
    Facts (Set.union a g) (Map.union b h) (Map.union c i) (Map.union d j) (Set.union e k) (Set.union f l)

data Finding
    = LeakedField (Lexeme Text) AccessPath AccessPath (Lexeme Text)
    -- ^ A free, what it frees, a field of that still holding memory, and
    -- where it got it.
    | LeakedLocal (Lexeme Text) String (Maybe (Lexeme Text))
    -- ^ Where a local got memory, the local, and the return that leaves it
    -- unfreed, if it has an expression.
    | FreedWhileStored (Lexeme Text) String (Lexeme Text)
    -- ^ A free, the local it frees, and the call that gave it memory it also
    -- stored.
    deriving (Eq, Ord)

-- | Forget what is known about a path and everything under it, and for a
-- variable, the elements it indexes.
forget :: AccessPath -> Facts -> Facts
forget p (Facts f o h s ps ls) = Facts (Set.filter keep f) (Map.filterWithKey (const . keep) o) (dropV h) (dropV s) ps ls'
  where
    dropV m = case p of
        PathVar v -> Map.delete v m
        _         -> m
    ls' = case p of
        PathVar v -> Set.delete v ls
        _         -> ls
    keep k = not (p `isPathPrefixOf` k) && not (indexedBy k)
    indexedBy k = case p of
        PathVar v -> any (elem (Text.pack v) . Text.split (not . isIdent) . Text.pack) (indices k)
        _         -> False
    isIdent c = c == '_' || c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9']
    indices = \case
        PathIndex q i -> i : indices q
        PathField q _ -> indices q
        PathDeref q   -> indices q
        _             -> []

-- | The locals an expression's value is, or points into.
passed :: C.Node (Lexeme Text) -> [String]
passed e = case bare e of
    Fix (VarExpr (L _ _ v)) -> [Text.unpack v]
    Fix (BinaryExpr l op r) | op `elem` [BopPlus, BopMinus] -> passed l ++ passed r
    Fix (TernaryExpr _ a b) -> passed a ++ passed b
    _ -> []

-- | Whether a call fills a variable through its address, as a status the
-- caller must check before it trusts the result.
fillsLocal :: C.Node (Lexeme Text) -> Bool
fillsLocal e = case bare e of
    Fix (FunctionCall _ args) -> or [ True | a <- args, Fix (UnaryExpr UopAddress (Fix VarExpr{})) <- [bare a] ]
    _                         -> False

-- | Hand memory in these locals on, unless the function frees them later
-- anyway, which shows it still owns them.
handOff :: Set String -> [String] -> Facts -> Facts
handOff later vs facts = facts { held = List.foldl' drop' (held facts) vs }
  where
    drop' h v
        | v `Set.member` later = h
        | otherwise = Map.delete v h

data Env = Env
    { envLocals   :: Set String
    , envParams   :: Set String
    , envPointers :: Set String
    , envContains :: Map String [String]
    -- ^ For a local struct, the variables whose memory its fields hold.
    }

-- | How an expression changes the facts, given the call outcomes known on
-- this path and the locals the function may free after it.
evalExpr :: Summary -> [(Lexeme Text, Outcome)] -> Env -> Set String -> C.Node (Lexeme Text) -> (Facts, [Finding]) -> (Facts, [Finding])
evalExpr summary known env later = go
  where
    locals = envLocals env
    params = envParams env
    go (Fix node) acc = case node of
        AssignExpr l _ r ->
            let (facts, found) = go r acc
                kept = if lasting l then contents r else []
                facts' = keepVars kept (handOff later (passed r) facts)
            in (maybe facts' (\p -> assign p r facts') (accessPath l), found)
        UnaryExpr op x | op `elem` [UopIncr, UopDecr], Just p <- accessPath x ->
            let (facts, found) = go x acc in (forget p facts, found)
        -- What has its address taken may be changed through it.
        UnaryExpr UopAddress x | Just p <- accessPath x ->
            let (facts, found) = go x acc in (forget p facts, found)
        FunctionCall callee args ->
            call callee args (List.foldl' (flip go) acc args)
        _ -> List.foldl' (flip go) acc (foldr (:) [] node)

    assign p r facts =
        let facts' = forget p facts
            -- Memory stored somewhere else belongs to that place now.
            moved = case accessPath (bare r) of
                Just q  -> facts' { owned = Map.delete q (owned facts') }
                Nothing -> facts'
        in case (bare r, p) of
            (Fix (FunctionCall callee _), _) | callsOneOf (sumFresh summary) r, isField p ->
                moved { owned = Map.insert p (calleeLexeme callee) (owned moved) }
            (Fix (FunctionCall callee _), PathVar v) | callsOneOf (sumFresh summary) r, v `Set.member` locals, not (fillsLocal r) ->
                moved { held = Map.insert v (calleeLexeme callee) (held moved) }
            (Fix (FunctionCall callee _), PathVar v) | callsOneOf (sumKept summary) r, v `Set.member` locals ->
                moved { shared = Map.insert v (calleeLexeme callee) (shared moved) }
            _ -> moved
    keepVars vs facts = facts
        { keptParams = Set.union (keptParams facts) (Set.fromList (filter (`Set.member` params) vs))
        , keptLocals = Set.union (keptLocals facts) (Set.fromList (filter (`Set.member` locals) vs))
        }
    -- A place that outlives the function: through a pointer, or not a local.
    lasting l = case accessPath l of
        Just p  -> throughPointer p || maybe True (`Set.notMember` locals) (rootOf p)
        Nothing -> True
    -- An argument a call can store into that outlives the function, other
    -- than memory under the kept variable itself.
    lastingFor v b = case bare b of
        Fix (UnaryExpr UopAddress x) -> case accessPath x >>= rootOf of
            Just root -> root `Set.notMember` locals && root /= v
            Nothing   -> False
        _ -> case accessPath b >>= rootOf of
            Just root -> root /= v && root `Set.member` envPointers env
            Nothing   -> False
    -- The variables an argument hands a call, with what a local struct
    -- passed by address holds.
    expand a = passed a ++ case bare a of
        Fix (UnaryExpr UopAddress x) | Just (PathVar l) <- accessPath x -> Map.findWithDefault [] l (envContains env)
        _ -> []
    -- The variables whose memory an expression's value points into or copies.
    contents r = passed r ++ case bare r of
        Fix (UnaryExpr UopDeref x) -> passed x
        _                          -> []
    isField = \case
        PathField q _ -> throughPointer q || isDeref q
        _             -> False
    isDeref = \case
        PathDeref{} -> True
        _           -> False

    call callee args (facts, found) =
        let here@(L _ _ g) = calleeLexeme callee
            outcomes = case lookup here known of
                Just o  -> [o]
                Nothing -> [minBound .. maxBound]
            freedArgs = [ p | i <- freedBy summary g outcomes, (a : _) <- [drop i args], Just p <- [accessPath a] ]
            unlinksOf = Set.toList (Map.findWithDefault Set.empty g (sumUnlinks summary))
            unlinkedArgs = [ p | i <- unlinksOf, (a : _) <- [drop i args], Just p <- [accessPath a] ]
            free (fs, found') p =
                let -- Only a function that frees no more than the memory it
                    -- is given leaves the fields of that memory behind.
                    leaks = [ LeakedField here p q at
                            | g `Map.member` freers
                            , (q, at) <- Map.toList (owned fs), q /= p, p `isPathPrefixOf` q ]
                    stillStored = [ FreedWhileStored here v at
                                  | p `notElem` unlinkedArgs
                                  , PathVar v <- [p], Just at <- [Map.lookup v (shared fs)] ]
                    fs' = forget p fs
                in (fs' { freed = Set.insert p (freed fs') }, found' ++ leaks ++ stillStored)
            (afterFree, found'') = List.foldl' free (facts, found) freedArgs
            -- Memory passed to a call is handed on, except where the call's
            -- result shows it failed.
            given = concatMap passed args
            handed0 = case lookup here known of
                Just Failure -> afterFree
                _            -> handOff later given afterFree
            keptArgs = [ v | j <- keptBy summary g outcomes, (a : _) <- [drop j args], v <- expand a
                           , or [ lastingFor v b | (k, b) <- zip [0 :: Int ..] args, k /= j ] ]
            -- A call removing an argument from where it is stored ends that.
            unstored = List.foldl' (flip Map.delete) (shared handed0)
                [ v | i <- unlinksOf, (a : _) <- [drop i args], v <- passed a, v `notElem` keptArgs ]
            handed = handed0
                { shared = List.foldl' (\m v -> Map.insert v here m) unstored (filter (`Set.member` locals) keptArgs)
                , keptParams = Set.union (keptParams handed0) (Set.fromList (filter (`Set.member` params) keptArgs))
                }
            -- A local filled through its address holds memory the function
            -- owns when it frees it later.
            filled = [ v | a <- args, Fix (UnaryExpr UopAddress x) <- [bare a], Just (PathVar v) <- [accessPath x]
                         , v `Set.member` locals, v `Set.member` later ]
        in (handed { held = List.foldl' (\h v -> Map.insert v here h) (held handed) filled }, found'')

-- | The findings in a function, the arguments it frees and the ones it
-- stores by the outcome it reports, and whether it may return memory it also
-- stored.
analyseFunction :: Summary -> Function -> ([Finding], Map Outcome (Set Int), Map Outcome (Set Int), Bool)
analyseFunction summary fn = (reported, frees, keepsArgs, keepsResult)
  where
    (entry, cfg) = fromFunction (fnDefn fn)
    decls = localDecls (fnDefn fn)
    locals = Set.fromList (map fst decls)
    env = Env
        { envLocals = locals
        , envParams = Set.fromList (map Text.unpack (fnParams fn))
        , envPointers = Set.fromList (paramPointers ++ localPointers)
        , envContains = Map.fromListWith (++)
            [ (root, passed r)
            | Fix (AssignExpr l _ r) <- universe (fnDefn fn), Just p <- [accessPath l]
            , not (throughPointer p), Just root <- [rootOf p], root `Set.member` locals, p /= PathVar root ]
        }
    paramPointers = case fnDefn fn of
        Fix (FunctionDefn _ (Fix (FunctionPrototype _ _ ps)) _) ->
            [ Text.unpack v | pd <- ps, Fix (VarDecl ty (L _ _ v) arrs) <- [unParam pd], isPointerType ty || not (null arrs) ]
        _ -> []
    localPointers = [ Text.unpack v | Fix (VarDeclStmt (Fix (VarDecl ty (L _ _ v) [])) _) <- universe (fnDefn fn), isPointerType ty ]
    unParam = \case
        Fix (NonNullParam x)  -> unParam x
        Fix (NullableParam x) -> unParam x
        x                     -> x
    isPointerType ty = or [ True | Fix TyPointer{} <- universe ty ]

    -- The locals the function may free after each point, before it gives
    -- them other memory.
    laterOut = solveBackward exits cfg Dataflow
        { transfer     = laterBefore
        , edgeTransfer = \_ _ s -> s
        , merge        = Set.union
        , initial      = Set.empty
        }
    exits = [ n | n@(Node _ ExitNode) <- Map.keys cfg ++ [ v | es <- Map.elems cfg, (_, v) <- es ] ]
    laterBefore n out = Set.union (freesIn n) (Set.difference out (setsIn n))
    laterAfter n = Map.findWithDefault Set.empty n laterOut
    laterOn n e = Set.unions [ laterBefore v (laterAfter v) |(e', v) <- Map.findWithDefault [] n cfg, e' == e ]
    -- Undoing work on the way to a failure return does not show that the
    -- function kept what it frees there.
    freesIn n | not (n `Set.member` toSuccess) = Set.empty
    freesIn n = Set.fromList
        [ v | x <- nodeExprs n, Fix (FunctionCall callee args) <- universe x
            , let L _ _ g = calleeLexeme callee
            , i <- freedBy summary g [minBound .. maxBound], (a : _) <- [drop i args]
            , Fix (VarExpr (L _ _ v')) <- [bare a], let v = Text.unpack v', v `Set.member` locals ]
    toSuccess = reach (Set.fromList [ n | (n, es) <- Map.toList cfg, any (isExit . snd) es, Success `elem` outcomesAt n ])
    outcomesAt = \case
        Node _ (StmtNode (Fix (Return e))) -> returnOutcomes (fnShape fn) e
        _                                  -> [Success]
    preds = Map.fromListWith (++) [ (v, [u]) | (u, es) <- Map.toList cfg, (_, v) <- es ]
    reach s = let s' = Set.union s (Set.fromList [ p | n <- Set.toList s, p <- Map.findWithDefault [] n preds ])
              in if Set.size s' == Set.size s then s else reach s'
    setsIn n = Set.fromList $ case n of
        Node _ (StmtNode (Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ v) _)) _))) -> [Text.unpack v]
        _ -> [ Text.unpack v | x <- nodeExprs n, Fix (AssignExpr (Fix (VarExpr (L _ _ v))) _ _) <- universe x ]
          ++ [ Text.unpack v | x <- nodeExprs n, Fix (UnaryExpr UopAddress (Fix (VarExpr (L _ _ v)))) <- universe x ]
    assigned = assignments (fnDefn fn)
    -- The locals that only hold enum constants, whose values are followed.
    flagVars = Set.fromList
        [ v | (v, ini) <- decls
            , let given = maybe [] pure ini ++ Map.findWithDefault [] v assigned
            , not (null given), all (maybe False isEnumConst . constName) given ]

    -- A branch's condition is evaluated on each edge out of it, knowing the
    -- outcomes it implies there.
    run n facts = case n of
        Node _ BranchNode{} -> (facts, [])
        _ -> returned n (List.foldl' (flip (evalExpr summary [] env (laterAfter n))) (declare n facts, []) (nodeExprs n))
    -- What a function returns is the caller's to free.
    returned n (facts, found) = case n of
        Node _ (StmtNode (Fix (Return (Just e)))) -> (facts { held = List.foldl' (flip Map.delete) (held facts) (passed e) }, found)
        _ -> (facts, found)
    declare n facts = case n of
        Node _ (StmtNode (Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ v) _)) ini))) ->
            let forgot = forget (PathVar (Text.unpack v)) facts
            in case ini of
                Just r | Fix (FunctionCall callee _) <- bare r, callsOneOf (sumFresh summary) r, not (fillsLocal r) ->
                    forgot { held = Map.insert (Text.unpack v) (calleeLexeme callee) (held forgot) }
                Just r | Fix (FunctionCall callee _) <- bare r, callsOneOf (sumKept summary) r ->
                    forgot { shared = Map.insert (Text.unpack v) (calleeLexeme callee) (shared forgot) }
                _ -> forgot
        _ -> facts
    onEdge n cond e facts = evalExpr summary (knownOn cond e) env (laterOn n e) cond (facts, [])

    setFlags n flags = List.foldl' setFlag flags $ case n of
        Node _ (StmtNode (Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ v) _)) ini))) -> [(v, ini)]
        _ -> [ (v, Just r) | e <- nodeExprs n, Fix (AssignExpr (Fix (VarExpr (L _ _ v))) _ r) <- universe e ]
    setFlag flags (v, ini)
        | name `Set.member` flagVars, Just c <- ini >>= constName = Map.insert name c flags
        | otherwise = Map.delete name flags
      where name = Text.unpack v

    -- The facts on the paths to a point are kept by the values the flags
    -- have on them. No entries means no path gets there.
    step n st = Map.fromListWith joinFacts
        [ (setFlags n flags, fst (run n facts)) | (flags, facts) <- Map.toList st ]

    edge n e st = case n of
        Node _ (BranchNode cond) ->
            Map.map (dropFailed cond e . dropNull cond e . fst . onEdge n cond e) (Map.filterWithKey (\flags _ -> flagHolds flags cond e) st)
        _ -> st
    knownOn cond e = case (unFix' (strip cond), e) of
        (BinaryExpr l BopAnd r, TrueBranch) -> knownOn l e ++ knownOn r e
        (BinaryExpr l BopOr r, FalseBranch) -> knownOn l e ++ knownOn r e
        _ -> case (branchOn (sumShapes summary) (resolve cond), e) of
            (Just (l, o), TrueBranch)  -> [(l, o)]
            (Just (l, o), FalseBranch) -> [(l, if o == Success then Failure else Success)]
            _                          -> []
    -- A local given only a call's result stands for the call in a branch.
    resultOf = Map.fromList
        [ (v, c) | (v, ini) <- decls
                 , [c@(Fix FunctionCall{})] <- [map bare (maybe [] pure ini ++ Map.findWithDefault [] v assigned)] ]
    resolve (Fix n) = case n of
        VarExpr (L _ _ v) | Just c <- Map.lookup (Text.unpack v) resultOf -> c
        _ -> Fix (fmap resolve n)
    -- Where a call failed, it filled and kept nothing.
    dropFailed cond e facts = facts { held = Map.filter (`notElem` failed) (held facts), shared = Map.filter (`notElem` failed) (shared facts) }
      where failed = [ l | (l, Failure) <- knownOn cond e ]
    -- Where a pointer is null, it holds no memory.
    dropNull cond e facts = case nullOn cond of
        Just (p, on) | on == e -> facts { owned = Map.delete p (owned facts), held = dropVar p (held facts) }
        _                      -> facts
    dropVar p h = case p of
        PathVar v -> Map.delete v h
        _         -> h
    nullOn = nullTest accessPath

    result = solve entry cfg Dataflow
        { transfer     = step
        , edgeTransfer = edge
        , merge        = Map.unionWith joinFacts
        , initial      = Map.singleton Map.empty (Facts Set.empty Map.empty Map.empty Map.empty Set.empty Set.empty)
        }

    reported = Set.toList . Set.fromList $ concat
        [ found
        | (n, st) <- Map.toList result
        , (flags, facts) <- Map.toList st
        , found <- case n of
            Node _ (BranchNode cond) ->
                [ snd (onEdge n cond e facts) | (e, _) <- Map.findWithDefault [] n cfg, flagHolds flags cond e ]
            _ -> [snd (run n facts)]
        ] ++
        [ LeakedLocal at v (exitLexeme n)
        | (n, edges) <- Map.toList cfg
        , any (isExit . snd) edges
        , not (aborts n)
        , Just st <- [Map.lookup n result]
        , (flags, facts) <- Map.toList st
        , after <- case n of
            Node _ (BranchNode cond) ->
                [ dropFailed cond e (dropNull cond e (fst (onEdge n cond e facts))) | (e, v') <- edges, isExit v', flagHolds flags cond e ]
            _ -> [fst (run n facts)]
        , (v, at) <- Map.toList (held after)
        ]
    -- Memory is not lost where the program stops.
    aborts n = or [ True | x <- nodeExprs n, Fix (FunctionCall callee _) <- universe x
                         , let L _ _ g = calleeLexeme callee, g `elem` ["abort", "exit", "LOGGER_FATAL"] ]
    exitLexeme = \case
        Node _ (StmtNode (Fix (Return (Just e)))) -> firstLexeme e
        _                                         -> Nothing

    keepsResult = or
        [ True
        | (after, ret) <- returnedAt
        , callsOneOf (sumKept summary) ret
          || any (\v -> v `Set.member` keptLocals after || v `Map.member` shared after) (passed ret) ]
    returnedAt =
        [ (fst (run n facts), e)
        | (n@(Node _ (StmtNode (Fix (Return (Just e))))), _) <- Map.toList cfg
        , Just st <- [Map.lookup n result], facts <- Map.elems st ]
    keepsArgs = Map.fromListWith Set.union
        [ (o, Set.fromList [ i | v <- Set.toList (keptParams after), Just i <- [List.elemIndex (Text.pack v) (fnParams fn)] ])
        | (after, o) <- atExits ]
    frees = Map.fromListWith Set.union
        [ (o, Set.fromList [ i | PathVar v <- Set.toList (freed after), Just i <- [List.elemIndex (Text.pack v) (fnParams fn)] ])
        | (after, o) <- atExits ]
    atExits =
        [ (after, o)
        | (n, edges) <- Map.toList cfg
        , any (isExit . snd) edges
        , Just st <- [Map.lookup n result]
        , facts <- Map.elems st
        , let after = fst (run n facts)
        , o <- case n of
            Node _ (StmtNode (Fix (Return e))) -> returnOutcomes (fnShape fn) e
            _                                  -> [Success]
        ]
    isExit (Node _ ExitNode) = True
    isExit _                 = False

firstLexeme :: C.Node (Lexeme Text) -> Maybe (Lexeme Text)
firstLexeme e = case [ l | Fix n <- universe e, l <- case n of
                            VarExpr l       -> [l]
                            LiteralExpr _ l -> [l]
                            _               -> [] ] of
    l : _ -> Just l
    []    -> Nothing

findings :: [(FilePath, [C.Node (Lexeme Text)])] -> [(FilePath, Finding)]
findings tus = [ (fnFile f, x) | f <- fns, let (found, _, _, _) = analyseFunction summary f, x <- found ]
  where
    fns = definitions tus
    summary = summarise fns

analyse :: [(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse tus = reverse $ flip State.execState [] $ sequence_
    [ warnRich $ Diagnostic pos len ErrorLevel msg Nothing (DiagnosticSpan pos len [label] : others) []
    | (file, finding) <- findings tus
    , let span' l note = let (p, n) = getDiagnosticInfo file l in DiagnosticSpan p n [note]
    , (here, msg, label, others) <- case finding of
        LeakedField at p q got ->
            [ ( at
              , backticks (render p) <+> "is freed while" <+> backticks (render q) <+> "may still hold memory"
              , "freed here"
              , [span' got "memory stored here"] ) ]
        LeakedLocal got v ret ->
            [ ( got
              , backticks (pretty v) <+> "may still hold memory when the function returns"
              , "memory from here"
              , [ span' r "returns here without freeing it" | Just r <- [ret] ] ) ]
        FreedWhileStored at v by@(L _ _ name) ->
            [ ( at
              , backticks (pretty v) <+> "is freed while" <+> backticks (pretty name) <+> "may still have it stored"
              , "freed here"
              , [span' by "stored by this call"] ) ]
    , let (pos, len) = getDiagnosticInfo file here
    ]

descr :: ([(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("memory-lifetime", Text.unlines
    [ "Checks that memory a function gets is freed, returned or handed on before"
    , "it returns, that a field's memory is freed before its struct, and that"
    , "memory is not freed while a call has stored it somewhere that outlives"
    , "the caller."
    , ""
    , "**Reason:** memory nothing points to is never freed, as on an error path"
    , "that returns early, and memory freed while still stored is then used or"
    , "freed again through the stored pointer."
    ]))
