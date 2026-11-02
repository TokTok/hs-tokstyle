{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.DanglingField (descr) where

import           Control.Monad.State.Strict         (State)
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
                                                     HasDiagnosticInfo (..),
                                                     warnRich)
import           Language.Cimple.Pretty             (showNodePlain)
import           Prettyprinter                      (pretty, (<+>))
import           Tokstyle.Analysis.AccessPath       (AccessPath (..),
                                                     isPathPrefixOf)
import           Tokstyle.Analysis.Dataflow         (Dataflow (..), solve)
import           Tokstyle.Cimple.Analysis.CFG       (EdgeType (..), Node (..),
                                                     NodeKind (..),
                                                     fromFunction, getFuncName)
import           Tokstyle.Cimple.Analysis.Functions
import           Tokstyle.Common                    (backticks)


-- | Where a field was freed: the file, the freeing call, and the field's
-- access path at that call.
data Origin = Origin FilePath (Lexeme Text) AccessPath
    deriving (Eq, Ord)

-- | One step of an access path relative to a parameter.
data Step = Field String | Index String | Deref
    deriving (Eq, Ord)

-- | The effects of a call on its arguments for one outcome.
data Effects = Effects
    { effFrees  :: Set Int
    -- ^ Parameters the call frees.
    , effLeaves :: Map (Int, [Step]) Origin
    -- ^ Parameter fields the call leaves pointing at freed memory.
    } deriving (Eq)

instance Semigroup Effects where
    Effects a b <> Effects c d = Effects (Set.union a c) (Map.union b d)

instance Monoid Effects where
    mempty = Effects Set.empty Map.empty

data Summary = Summary
    { effects :: Map Text (Map Outcome Effects)
    , shapes  :: Map Text Shape
    }

-- | Facts that hold on some path to a node.
data Facts = Facts
    { dangling :: Map AccessPath Origin
    -- ^ Fields still pointing at freed memory.
    , freed    :: Set String
    -- ^ Variables pointing at freed memory.
    } deriving (Eq)

joinFacts :: Facts -> Facts -> Facts
joinFacts (Facts a b) (Facts c d) = Facts (Map.union a c) (Set.union b d)

-- | The enum constant each error-flag local holds.
type Flags = Map String Text

-- | Path-sensitive state: the facts for each combination of error-flag
-- values. An empty map means the node is unreachable.
type St = Map Flags Facts

data Context = Context
    { ctxFile    :: FilePath
    , ctxParams  :: [Text]
    , ctxAliases :: Map String String
    -- ^ Locals only ever holding another variable, mapped to that variable.
    , ctxFlags   :: Set String
    -- ^ Locals only ever holding enum constants.
    , ctxSlots   :: Map String [Text]
    -- ^ Locals assigned from a slot getter, mapped to the call's arguments.
    , ctxGetters :: Set Text
    }

-- | Find slot getters: functions returning null or an element of what a
-- parameter points to, i.e. the element, its address, or a path under
-- another slot getter's result.
slotGetters :: [Function] -> Set Text
slotGetters fns = go Set.empty
  where
    go s = let s' = Set.fromList [ fnName f | f <- fns, gets s f ] in if s' == s then s else go s'
    gets s f = length (fnParams f) >= 2 && not (null (rets f)) && all (slot s f) (rets f)
    rets f = [ e | Fix (Return (Just e)) <- universe (fnDefn f) ]
    slot s f e = case bare e of
        e' | isNullLit e'            -> True
        Fix (UnaryExpr UopAddress x) -> under s f x
        x                            -> under s f x
    under s f x = case accessPath x of
        Just p | Just r <- rootOf p ->
            (Text.pack r `elem` fnParams f && indexed p)
            || any (fromGetter s) [ ini | (v, Just ini) <- localDecls (fnDefn f), v == r ]
        _ -> False
    indexed = \case
        PathIndex{}   -> True
        PathField q _ -> indexed q
        PathDeref q   -> indexed q
        _             -> False
    fromGetter s ini = case bare ini of
        Fix (FunctionCall callee _) | L _ _ g <- calleeLexeme callee -> g `Set.member` s
        _ -> False

contextOf :: Set Text -> Function -> Context
contextOf getters fn = Context
    { ctxFile = fnFile fn
    , ctxParams = fnParams fn
    , ctxAliases = Map.fromList
        [ (v, x) | (v, Just ini) <- decls, Map.notMember v assigned, Just x <- [plainVar ini] ]
    , ctxFlags = Set.fromList
        [ v | (v, ini) <- decls, let vs = values v ini, not (null vs), all (maybe False isEnumConst . constName) vs ]
    , ctxSlots = Map.fromList
        [ (v, map showNodePlain args)
        | (v, ini) <- decls, e <- values v ini
        , Fix (FunctionCall callee args) <- [strip e], Just name <- [getFuncName callee]
        , name `Set.member` getters ]
    , ctxGetters = getters
    }
  where
    decls = localDecls (fnDefn fn)
    assigned = assignments (fnDefn fn)
    values v ini = maybe [] pure ini ++ Map.findWithDefault [] v assigned
    plainVar e = case unFix' (strip e) of
        VarExpr (L _ _ x) -> Just (Text.unpack x)
        CastExpr _ x      -> plainVar x
        _                 -> Nothing

-- | Resolve a local to the variable it aliases.
canonical :: Context -> String -> String
canonical ctx = go (10 :: Int)
  where
    go 0 v = v
    go k v = maybe v (go (k - 1)) (Map.lookup v (ctxAliases ctx))

pathOf :: Context -> C.Node (Lexeme Text) -> Maybe AccessPath
pathOf ctx e = mapRoot (canonical ctx) <$> accessPath e

-- | Get the steps of an access path, from its root.
stepsOf :: AccessPath -> [Step]
stepsOf = reverse . go
  where
    go = \case
        PathField p s -> Field s : go p
        PathIndex p s -> Index s : go p
        PathDeref p   -> Deref : go p
        _             -> []

extend :: AccessPath -> [Step] -> AccessPath
extend = List.foldl' $ \p -> \case
    Field s -> PathField p s
    Index s -> PathIndex p s
    Deref   -> PathDeref p

forget :: AccessPath -> Facts -> Facts
forget p (Facts d f) = Facts (Map.filterWithKey (\k _ -> not (p `isPathPrefixOf` k)) d) (maybe f (`Set.delete` f) (varOf p))
  where
    varOf (PathVar v) = Just v
    varOf _           = Nothing

-- | Transfer function for an expression, given the outcome a call is known to
-- have reported on this path.
evalExpr :: Summary -> Context -> Maybe (Lexeme Text, Outcome) -> C.Node (Lexeme Text) -> (Flags, Facts) -> (Flags, Facts)
evalExpr summary ctx known = go
  where
    go (Fix node) st = case node of
        FunctionCall callee args ->
            let (flags', facts') = List.foldl' (flip go) st args
            in (flags', call callee args facts')
        AssignExpr l _ r ->
            let (flags', facts') = go r st
                assigned = maybe facts' (`forget` facts') (pathOf ctx l)
            in case (unFix' (strip l), constName r) of
                (VarExpr (L _ _ v), Just c) | Text.unpack v `Set.member` ctxFlags ctx ->
                    (Map.insert (Text.unpack v) c flags', assigned)
                _ -> (flags', assigned)
        _ -> List.foldl' (flip go) st (foldr (:) [] node)

    call callee args facts = case getFuncName callee of
        Nothing -> List.foldl' passArg facts args
        Just fn ->
            let direct = Effects (maybe Set.empty Set.singleton (Map.lookup fn freers)) Map.empty
                byOutcome = Map.findWithDefault Map.empty fn (effects summary)
                outcomes = case known of
                    Just (l, o) | l == calleeLexeme callee -> [o]
                    _                                      -> [minBound .. maxBound]
                eff = direct <> mconcat [ Map.findWithDefault mempty o byOutcome | o <- outcomes ]
                handled = effFrees eff `Set.union` Set.fromList (map fst (Map.keys (effLeaves eff)))
                arg i = case drop i args of
                    (a : _) -> pathOf ctx a
                    []      -> Nothing
                freeArg i fs = case arg i of
                    Just p@(PathVar v) -> (forget p fs) { freed = Set.insert v (freed fs) }
                    Just p -> (forget p fs) { dangling = Map.insert p (Origin (ctxFile ctx) (calleeLexeme callee) p) (dangling (forget p fs)) }
                    Nothing -> fs
                leaveArg fs ((i, steps), origin) = case arg i of
                    Just p  -> fs { dangling = Map.insert (extend p steps) origin (dangling fs) }
                    Nothing -> fs
                others = [ a | (i, a) <- zip [0 ..] args, i `Set.notMember` handled ]
                slotsTouched = [ PathVar v | (v, key) <- Map.toList (ctxSlots ctx), fn `Set.notMember` ctxGetters ctx
                                           , all (`elem` map showNodePlain args) key ]
                afterFree = List.foldl' (flip freeArg) facts (Set.toList (effFrees eff))
                afterLeave = List.foldl' leaveArg afterFree (Map.toList (effLeaves eff))
            in List.foldl' (flip forget) (List.foldl' passArg afterLeave others) slotsTouched

    -- A callee given a variable or an address may reset the fields under it.
    passArg facts a = case unFix' (strip a) of
        UnaryExpr UopAddress x | Just p <- pathOf ctx x -> forget p facts
        _ -> case pathOf ctx a of
            Just p@(PathVar _) -> forget p facts
            _                  -> facts

joinSt :: St -> St -> St
joinSt = Map.unionWith joinFacts

mapSt :: ((Flags, Facts) -> (Flags, Facts)) -> St -> St
mapSt f = Map.fromListWith joinFacts . map f . Map.toList

-- | Get the facts at each exit of a function, with the outcomes reported
-- there.
atReturns :: Summary -> Set Text -> Function -> [([Outcome], Facts)]
atReturns summary getters fn =
    [ (outcomes, facts)
    | (n, edges) <- Map.toList cfg
    , any (isExit . snd) edges
    , Just st <- [Map.lookup n result]
    , let (outcomes, st') = case n of
              Node _ (StmtNode (Fix (Return e))) -> (returnOutcomes (fnShape fn) e, step n st)
              _                                  -> ([Success], step n st)
    , facts <- Map.elems st'
    ]
  where
    ctx = contextOf getters fn
    (entry, cfg) = fromFunction (fnDefn fn)
    isExit (Node _ ExitNode) = True
    isExit _                 = False
    declare n st = case n of
        Node _ (StmtNode (Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ v) _)) ini))) ->
            let name = Text.unpack v
                setFlag flags = case ini >>= constName of
                    Just c | name `Set.member` ctxFlags ctx -> Map.insert name c flags
                    _                                       -> Map.delete name flags
            in mapSt (\(flags, facts) -> (setFlag flags, forget (PathVar name) facts)) st
        _ -> st
    step n st = case n of
        Node _ BranchNode{} -> st
        _ -> declare n (List.foldl' (\s e -> mapSt (evalExpr summary ctx Nothing e) s) st (nodeExprs n))
    edge n e st = case n of
        Node _ (BranchNode cond) ->
            let known = case (branchOn (shapes summary) cond, e) of
                    (Just (l, o), TrueBranch)  -> Just (l, o)
                    (Just (l, o), FalseBranch) -> Just (l, if o == Success then Failure else Success)
                    _                          -> Nothing
            in Map.map (dropNull cond e)
                . Map.filterWithKey (\flags _ -> holds flags cond e)
                $ mapSt (evalExpr summary ctx known cond) st
        _ -> st
    -- Nothing under a pointer dangles on the branch where it is null.
    dropNull cond e facts = case (nullTest (pathOf ctx) cond, e) of
        (Just (p, TrueBranch), TrueBranch)   -> forgetUnder p facts
        (Just (p, FalseBranch), FalseBranch) -> forgetUnder p facts
        _                                    -> facts
    forgetUnder p facts = facts { dangling = Map.filterWithKey (\k _ -> not (p `isPathPrefixOf` k)) (dangling facts) }
    -- Check if a branch on an error flag is feasible for the flag values.
    holds flags cond e = case unFix' (strip cond) of
        BinaryExpr (Fix (VarExpr (L _ _ v))) op r
            | Just c <- constName r, Just c' <- Map.lookup (Text.unpack v) flags, op `elem` [BopEq, BopNe] ->
                let equal = c == c'
                in case e of
                    TrueBranch  -> if op == BopEq then equal else not equal
                    FalseBranch -> if op == BopEq then not equal else equal
                    _           -> True
        _ -> True
    result = solve entry cfg Dataflow
        { transfer     = step
        , edgeTransfer = edge
        , merge        = joinSt
        , initial      = Map.singleton Map.empty (Facts Map.empty Set.empty)
        }

effectsOf :: Summary -> Set Text -> Function -> Map Outcome Effects
effectsOf summary getters fn = Map.fromListWith (<>)
    [ (o, Effects
        (Set.fromList [ i | v <- Set.toList (freed facts), Just i <- [paramIndex v] ])
        (Map.fromList [ ((i, stepsOf p), origin) | (p, origin) <- Map.toList (dangling facts), Just r <- [rootOf p], Just i <- [paramIndex r] ]))
    | (outcomes, facts) <- atReturns summary getters fn, o <- outcomes ]
  where
    paramIndex v = List.elemIndex (Text.pack v) (fnParams fn)

summarise :: Set Text -> [Function] -> Summary
summarise getters fns = bottomUp callees add look fns (Summary Map.empty shapeMap)
  where
    shapeMap = Map.fromList [ (fnName f, fnShape f) | f <- fns ]
    add s f = s { effects = Map.insertWith (Map.unionWith (<>)) (fnName f) (effectsOf s getters f) (effects s) }
    look s f = Map.lookup (fnName f) (effects s)

analyse :: [(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse tus = reverse $ State.execState (mapM_ report (Map.toList reports)) []
  where
    fns = definitions tus
    getters = slotGetters fns
    summary = summarise getters fns
    reports = Map.fromListWith (++)
        [ (o, [(fnName f, p)])
        | f <- fns
        , (_, facts) <- atReturns summary getters f
        , (p, o) <- Map.toList (dangling facts)
        , throughPointer p
        , Just r <- [rootOf p]
        , Text.pack r `notElem` fnParams f ]

report :: (Origin, [(Text, AccessPath)]) -> State [Diagnostic CimplePos] ()
report (Origin file at freedPath, sites) = case List.sort sites of
    [] -> return ()
    (fn, p) : _ -> do
        let (pos, len) = getDiagnosticInfo file at
        warnRich $ Diagnostic pos len ErrorLevel
            (backticks (render freedPath) <+> "is freed here, and" <+> backticks (render p)
                <+> "still points at it when" <+> backticks (pretty fn) <+> "returns")
            Nothing [] []

descr :: ([(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("dangling-field", Text.unlines
    [ "Checks that a struct field whose object gets freed is set to something else"
    , "before the struct is used again."
    , ""
    , "**Reason:** the struct outlives the function, and the next code that reads"
    , "the field uses freed memory."
    ]))
