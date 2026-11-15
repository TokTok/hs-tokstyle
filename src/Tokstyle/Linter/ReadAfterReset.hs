{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
-- | Finds fields read after a call overwrote the whole object they are in.
module Tokstyle.Linter.ReadAfterReset (descr) where

import qualified Control.Monad.State.Strict         as State
import           Data.Fix                           (Fix (..))
import           Data.Function                      (on)
import qualified Data.List                          as List
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Maybe                         (mapMaybe)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Language.Cimple                    (AssignOp (..), Lexeme (..),
                                                     NodeF (..), UnaryOp (..))
import qualified Language.Cimple                    as C
import           Language.Cimple.Diagnostics        (CimplePos, Diagnostic (..),
                                                     DiagnosticLevel (..),
                                                     HasDiagnosticInfo (..),
                                                     warnRich)
import           Prettyprinter                      (pretty, (<+>))
import           Tokstyle.Analysis.AccessPath       (AccessPath (..),
                                                     isPathPrefixOf)
import           Tokstyle.Cimple.Analysis.Functions (Function (..), accessPath,
                                                     bare, definitions,
                                                     localDecls, render,
                                                     universe)
import           Tokstyle.Common                    (backticks)


-- | The parameters each function overwrites whole, with the fields it sets
-- again after that.
type Resets = Map Text [(Int, Set String)]

objectOf :: C.Node (Lexeme Text) -> Maybe AccessPath
objectOf arg = case bare arg of
    Fix (UnaryExpr UopAddress e) -> accessPath e
    e                            -> PathDeref <$> accessPath e

-- | The objects a call statement overwrites whole, with the function it calls
-- and the fields that function sets again.
callResets :: Resets -> C.Node (Lexeme Text) -> [(Text, AccessPath, Set String)]
callResets resets = \case
    Fix (ExprStmt e) | Fix (FunctionCall (Fix (VarExpr (L _ _ callee))) args) <- bare e ->
        [ (callee, p, set)
        | (i, set) <- Map.findWithDefault [] callee resets
        , arg <- take 1 (drop i args)
        , Just p <- [objectOf arg]
        ]
    _ -> []

fieldsSet :: AccessPath -> [C.Node (Lexeme Text)] -> Set String
fieldsSet obj stmts = Set.fromList
    [ f
    | stmt <- stmts
    , Fix (AssignExpr lhs _ _) <- universe stmt
    , Just p <- [accessPath lhs]
    , Just f <- [fieldOf obj p]
    ]

-- | Returns the field of `obj` that a path goes through, e.g. `f` in
-- `obj.f.g`.
fieldOf :: AccessPath -> AccessPath -> Maybe String
fieldOf obj = \case
    PathField p f | p == obj -> Just f
    PathField p _            -> fieldOf obj p
    PathIndex p _            -> fieldOf obj p
    PathDeref p              -> fieldOf obj p
    _                        -> Nothing

-- | Each statement with the ones after it.
withRest :: [a] -> [(a, [a])]
withRest stmts = zip stmts (drop 1 (List.tails stmts))

statements :: C.Node (Lexeme Text) -> [C.Node (Lexeme Text)]
statements = \case
    Fix (FunctionDefn _ _ (Fix (CompoundStmt stmts))) -> stmts
    _                                                 -> []

-- | Finds the parameters each function overwrites, also through the
-- functions it calls, until nothing changes.
inferResets :: [Function] -> Resets
inferResets fns = go Map.empty
  where
    go resets =
        let resets' = Map.fromList (mapMaybe (summarise resets) fns)
        in if resets' == resets then resets else go resets'

    summarise resets fn =
        case [ (i, Set.union set (fieldsSet obj rest))
             | (stmt, rest) <- withRest (statements (fnDefn fn))
             , (obj, set) <- assigned stmt ++ [ (o, s) | (_, o, s) <- callResets resets stmt ]
             , (i, p) <- zip [0 ..] (fnParams fn)
             , obj == PathDeref (PathVar (Text.unpack p))
             ] of
            []    -> Nothing
            found -> Just (fnName fn, List.nubBy ((==) `on` fst) found)
      where
        own = Set.fromList (fnParams fn ++ map (Text.pack . fst) (localDecls (fnDefn fn)))
        assigned = \case
            Fix (ExprStmt e) | Fix (AssignExpr lhs AopEq rhs) <- bare e, fixed rhs ->
                [ (p, Set.empty) | Just p <- [accessPath lhs] ]
            _ -> []
        -- A value that doesn't depend on the caller, e.g. an empty object.
        -- Copying another object in or filling one from the parameters is not
        -- a reset.
        fixed rhs = and
            [ case node of
                Fix (VarExpr (L _ _ v)) -> v `Set.notMember` own
                Fix FunctionCall{}      -> False
                _                       -> True
            | node <- universe rhs
            ]

data Event
    = Read AccessPath (C.Node (Lexeme Text))
    | Write AccessPath
    | Leave
    | Branch [[Event]]
    -- ^ Paths of which at most one runs.
    | Loop [Event]
    -- ^ A path that runs any number of times.

arrayFields :: [(FilePath, [C.Node (Lexeme Text)])] -> Set Text
arrayFields tus = Set.fromList
    [ f
    | (_, nodes) <- tus, node <- nodes
    , Fix (MemberDecl (Fix (VarDecl _ (L _ _ f) (_ : _))) _) <- universe node
    ]

-- | What an expression or statement does to memory, in about the order it
-- happens.
events :: Set Text -> C.Node (Lexeme Text) -> [Event]
events arrays = go
  where
    go node = case unFix node of
        AssignExpr lhs _ rhs -> go rhs ++ target lhs
        UnaryExpr UopAddress e -> maybe (go e) (pure . Write) (accessPath e)
        UnaryExpr op e | op `elem` [UopIncr, UopDecr] -> target e
        FunctionCall callee args -> go callee ++ concatMap argument args
        PointerAccess{} -> access node
        MemberAccess{} -> access node
        SizeofExpr{} -> []
        IfStmt c t e -> go c ++ [Branch [go t, maybe [] go e]]
        TernaryExpr c t e -> go c ++ [Branch [go t, go e]]
        WhileStmt c body -> go c ++ [Loop (go body ++ go c)]
        DoWhileStmt body c -> [Loop (go body ++ go c)]
        ForStmt i c n body -> go i ++ go c ++ [Loop (go body ++ go n ++ go c)]
        -- A `break` ends its case, so each case is a path of its own.
        SwitchStmt c cases -> go c ++ [Branch (map go cases)]
        Return e -> maybe [] go e ++ [Leave]
        Break -> [Leave]
        Continue -> [Leave]
        Goto{} -> [Leave]
        n -> concatMap go n

    access node = case accessPath node of
        Just p  -> [Read p node]
        Nothing -> concatMap go (unFix node)

    target lhs = maybe (go lhs) (pure . Write) (accessPath lhs)

    -- A function can write through any pointer it gets, and an array field
    -- passes a pointer to the array, not a value.
    argument arg = case bare arg of
        Fix (PointerAccess _ (L _ _ f)) | f `Set.member` arrays -> address
        Fix (MemberAccess _ (L _ _ f)) | f `Set.member` arrays  -> address
        _ -> go arg ++ maybe [] (pure . Write . PathDeref) (accessPath arg)
      where
        address = maybe [] (pure . Write) (accessPath arg)

-- | Looks into branches and loops too.
writes :: [Event] -> [AccessPath]
writes = concatMap $ \case
    Write p    -> [p]
    Branch evs -> concatMap writes evs
    Loop evs   -> writes evs
    _          -> []

data Outcome
    = Goes (Set String)
    -- ^ Goes on, with these fields set again.
    | Stop
    -- ^ Leaves, or writes the object again.

-- | Reports the fields of `obj` read in the statements before anything sets
-- them again.
scan :: Set Text -> FilePath -> Text -> AccessPath -> Set String -> [C.Node (Lexeme Text)]
     -> State.State [Diagnostic CimplePos] ()
scan arrays file callee obj set stmts = () <$ go set (concatMap (events arrays) stmts)
  where
    go :: Set String -> [Event] -> State.State [Diagnostic CimplePos] Outcome
    go set' [] = pure (Goes set')
    go set' (ev : evs) = case ev of
        Leave -> pure Stop
        Read p node
            | Just f <- fieldOf obj p, f `Set.notMember` set' -> do
                let (pos, len) = getDiagnosticInfo file node
                warnRich $ Diagnostic pos len ErrorLevel
                    (backticks (render p) <+> "is read after" <+> backticks (pretty callee)
                        <+> "overwrote" <+> backticks (render obj))
                    Nothing [] []
                go (Set.insert f set') evs
            | otherwise -> go set' evs
        Write p
            | p `isPathPrefixOf` obj -> pure Stop
            | Just f <- fieldOf obj p -> go (Set.insert f set') evs
            | otherwise -> go set' evs
        Branch paths -> do
            outcomes <- mapM (go set') paths
            -- A path that wrote the object again or left doesn't come back here.
            go (Set.unions (set' : [ s | Goes s <- outcomes ])) evs
        Loop body -> do
            -- What the body sets carries over from the iteration before, so
            -- only the first one reads the value after the reset, e.g. to grow
            -- an array from null.
            let carried = Set.fromList (mapMaybe (fieldOf obj) (writes body))
            outcome <- go (Set.union set' carried) body
            go (case outcome of
                Goes s -> Set.union set' (s `Set.difference` carried)
                Stop   -> set') evs

-- | Checks the statements after each call that overwrites an object, in every
-- block of a function.
checkFunction :: Set Text -> Resets -> Function -> State.State [Diagnostic CimplePos] ()
checkFunction arrays resets fn = sequence_
    [ scan arrays (fnFile fn) callee obj set rest
    | Fix (CompoundStmt stmts) <- universe (fnDefn fn)
    , (stmt, rest) <- withRest stmts
    , (callee, obj, set) <- callResets resets stmt
    ]

analyse :: [(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse tus =
    reverse $ State.execState (mapM_ (checkFunction (arrayFields tus) resets) fns) []
  where
    fns = definitions tus
    resets = inferResets fns

descr :: ([(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("read-after-reset", Text.unlines
    [ "Checks that no field is read after a call overwrote the whole object it is in."
    , "A function overwrites what its caller passed as `p` if it sets `*p` to a value"
    , "that doesn't depend on its caller, e.g. an empty object."
    , ""
    , "**Reason:** the field then has the new value, usually 0, and not the one it had,"
    , "e.g. a message is acknowledged with id 0 after its entry was cleared."
    ]))
