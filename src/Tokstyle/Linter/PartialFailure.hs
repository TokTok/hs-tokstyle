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

evalExpr :: (AccessPath -> Bool) -> C.Node (Lexeme Text) -> Changes -> Changes
evalExpr outlives = go
  where
    go (Fix node) s = case node of
        UnaryExpr op x | op `elem` [UopIncr, UopDecr] ->
            change x (if op == UopIncr then 1 else -1) (go x s)
        AssignExpr l AopPlus r | Just n <- intLit r -> change l n (go r s)
        AssignExpr l AopMinus r | Just n <- intLit r -> change l (negate n) (go r s)
        AssignExpr l _ r -> maybe id under (accessPath l) (go r s)
        FunctionCall callee args
            | Just name <- calleeName callee, Just i <- Map.lookup name freers, (a : _) <- drop i args, Just p <- accessPath a ->
                under p (List.foldl' (flip go) s args)
        _ -> List.foldl' (flip go) s (foldr (:) [] node)
    -- Forget what is known about a path once it is reassigned or freed.
    under p = Map.filterWithKey (\k _ -> not (p `isPathPrefixOf` k))
    calleeName c = case calleeLexeme c of
        L _ _ t | not (Text.null t) -> Just t
        _                           -> Nothing
    change x n s = case (accessPath x, anchor x) of
        (Just p, Just at) | outlives p -> Map.alter (bump n at) p s
        _                              -> s
    bump n at = \case
        Nothing        -> Just (n, at)
        Just (m, at')  | m + n == 0 -> Nothing
                       | otherwise  -> Just (m + n, at')

-- | Find the failure returns of a function with the array lengths changed and
-- not restored on the way there.
failures :: Set Text -> Function -> [(C.Node (Lexeme Text), Changes)]
failures lengths fn =
    [ (ret, changes)
    | (n@(Node _ (StmtNode ret@(Fix (Return e)))), _) <- Map.toList cfg
    , isFailure fn e
    , Just s <- [Map.lookup n result]
    , let changes = step n s
    , not (Map.null changes)
    ]
  where
    (entry, cfg) = fromFunction (fnDefn fn)
    outlives p = throughPointer p && maybe False (`Set.member` lengths) (lastField p)
    step n s = List.foldl' (flip (evalExpr outlives)) s (nodeExprs n)
    result = solve entry cfg Dataflow
        { transfer     = step
        , edgeTransfer = \_ _ s -> s
        , merge        = Map.unionWith (\a b -> if fst a /= 0 then a else b)
        , initial      = Map.empty
        }

-- | Find destructor calls on a local immediately before failure returns.
-- Only calls preceding at least two failure returns count as the function's
-- cleanup convention.
failureCleanups :: Destructors -> Function -> Set (Text, String)
failureCleanups ds fn = Map.keysSet . Map.filter (>= (2 :: Int)) $ Map.fromListWith (+)
    [ ((name, v), 1)
    | Fix (CompoundStmt stmts) <- universe (fnDefn fn)
    , (i, Fix (Return e)) <- zip [0 :: Int ..] stmts
    , isFailure fn e
    , Fix (ExprStmt call@(Fix (FunctionCall callee _))) <- take i stmts
    , L _ _ name <- [calleeLexeme callee]
    , Fix (VarExpr (L _ _ v')) <- map strip (destroyed ds call)
    , let v = Text.unpack v', v `Set.member` locals
    ]
  where
    locals = pointerLocals fn

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
    step n owned = case n of
        Node _ (StmtNode (Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ v) _)) ini))) ->
            acquire (Text.unpack v) ini owned
        _ -> List.foldl' (flip expr) owned (nodeExprs n)
    acquire v ini owned
        | Just (Fix FunctionCall{}) <- strip <$> ini, v `Set.member` locals = Set.insert v owned
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

-- | Find failure returns where a local the function cleans up before other
-- failure returns may still be owned.
skippedCleanups :: Destructors -> Map Text (Int, Set Text) -> Function -> [(C.Node (Lexeme Text), (Text, String))]
skippedCleanups ds statuses fn =
    [ (ret, c)
    | (ret, owned) <- ownedAtFailures ds statuses fn
    , c@(_, v) <- Set.toList pairs
    , v `Set.member` owned
    ]
  where
    pairs = failureCleanups ds fn

analyse :: [(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse tus = reverse $ State.execState (mapM_ report reports >> mapM_ reportSkipped skipped) []
  where
    fns = definitions tus
    lengths = lengthFields fns
    reports = Map.toList $ Map.fromListWith (++)
        [ ((fnFile f, at), [(fnName f, p, ret)])
        | f <- fns
        , (ret, changes) <- failures lengths f
        , (p, (_, at)) <- Map.toList changes ]
    statuses = statusFunctions fns
    ds = destructors tus
    skipped = [ (f, ret, c) | f <- fns, (ret, c) <- skippedCleanups ds statuses f ]

reportSkipped :: (Function, C.Node (Lexeme Text), (Text, String)) -> State [Diagnostic CimplePos] ()
reportSkipped (fn, ret, (name, v)) = do
    let (pos, len) = getDiagnosticInfo (fnFile fn) ret
    warnRich $ Diagnostic pos len ErrorLevel
        (backticks (pretty (fnName fn)) <+> "fails here without calling" <+> backticks (pretty name)
            <+> "on" <+> backticks (pretty v) <> ", which it does when it fails before this")
        Nothing [] []

report :: ((FilePath, Lexeme Text), [(Text, AccessPath, C.Node (Lexeme Text))]) -> State [Diagnostic CimplePos] ()
report ((file, at), sites) = case sites of
    [] -> return ()
    (fn, p, ret) : _ -> do
        let (pos, len) = getDiagnosticInfo file at
            (retPos, _) = getDiagnosticInfo file ret
        warnRich $ Diagnostic pos len ErrorLevel
            (backticks (render p) <+> "is changed here, and" <+> backticks (pretty fn)
                <+> "then fails at line" <+> pretty (posLine retPos) <+> "without changing it back")
            Nothing [] []

descr :: ([(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("partial-failure", Text.unlines
    [ "Checks that a function that fails leaves the lengths of the arrays it was"
    , "given as they were."
    , ""
    , "**Reason:** a caller that sees the failure goes on as if nothing changed."
    , "A length changed anyway no longer matches the array, and the work that was"
    , "meant to go with the change, such as telling a callback, never happens."
    , ""
    , "Also checks that a failure destroys what the failures before it destroy, so"
    , "that what the function made is not left behind for a caller who was told"
    , "nothing was made."
    ]))
