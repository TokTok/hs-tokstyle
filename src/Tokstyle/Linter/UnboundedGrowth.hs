{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
-- | Finds lists and arrays that peers can grow without limit by sending
-- packets.
module Tokstyle.Linter.UnboundedGrowth (descr) where

import qualified Control.Monad.State.Strict                as State
import           Data.Fix                                  (Fix (..))
import qualified Data.List                                 as List
import           Data.Map.Strict                           (Map)
import qualified Data.Map.Strict                           as Map
import           Data.Maybe                                (isJust, mapMaybe)
import           Data.Set                                  (Set)
import qualified Data.Set                                  as Set
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import           Language.Cimple                           (AssignOp (..),
                                                            BinaryOp (..),
                                                            Lexeme (..),
                                                            NodeF (..),
                                                            UnaryOp (..))
import qualified Language.Cimple                           as C
import           Language.Cimple.Diagnostics               (CimplePos,
                                                            Diagnostic (..),
                                                            DiagnosticLevel (..),
                                                            DiagnosticSpan (..),
                                                            HasDiagnosticInfo (..),
                                                            warnRich)
import           Prettyprinter                             (pretty, (<+>))
import           Tokstyle.Analysis.AccessPath              (AccessPath (..))
import           Tokstyle.Cimple.Analysis.CFG              (CFG, Node (..),
                                                            NodeKind (..),
                                                            fromFunction,
                                                            getFuncName)
import           Tokstyle.Cimple.Analysis.FunctionPointers (FieldKey,
                                                            FieldTargets,
                                                            Targets (..),
                                                            callTargets,
                                                            fieldTargets)
import           Tokstyle.Cimple.Analysis.Functions        (Function (..),
                                                            accessPath,
                                                            allocators,
                                                            assignments, bare,
                                                            bottomUp, callees,
                                                            definitions, intLit,
                                                            isNullLit,
                                                            localDecls,
                                                            nodeExprs, nullTest,
                                                            render, rootOf,
                                                            universe)
import           Tokstyle.Cimple.Analysis.Types            (Structs, structName,
                                                            structs, typeIn)
import           Tokstyle.Common                           (backticks)


-- | How a call makes something grow.
data By
    = Grows
    | ResizedBy Int
    -- ^ Resized to the size the caller passes in this parameter.
    deriving (Eq, Ord, Show)

-- | Something growing, and the object holding it. Paths use the names in the
-- function being analysed.
data Growth = Growth
    { growFile  :: FilePath
    , growIn    :: Text
    , growAt    :: Lexeme Text
    , growWhat  :: AccessPath
    , growField :: Maybe FieldKey
    , growOwner :: AccessPath
    , growBy    :: By
    , growVia   :: [Text]
    -- ^ The calls from the function analysed down to where it grows.
    }

instance Eq Growth where
    a == b = key a == key b

instance Ord Growth where
    compare a b = compare (key a) (key b)

key :: Growth -> (FilePath, Int, Int, AccessPath, By)
key g = let L (C.AlexPn _ l c) _ _ = growAt g in (growFile g, l, c, growOwner g, growBy g)

data Context = Context
    { ctxTypes     :: Structs
    , ctxFns       :: Map Text Function
    , ctxFinders   :: Map Text FieldKey
    -- ^ Functions returning a pointer into an array field.
    , ctxShrinkers :: Set Text
    -- ^ Functions that may make a list or array smaller.
    , ctxGrowing   :: Set FieldKey
    -- ^ Fields packets may make grow with nothing limiting them.
    }

-- | The growth each function leaves to its callers to limit.
type Summary = Map Text (Set Growth)

reallocators :: Map Text (Int, Int)
reallocators = Map.fromList
    [("realloc", (0, 1)), ("mem_vrealloc", (1, 2)), ("mem_brealloc", (1, 2))]

-- | List nodes, i.e. structs with a field pointing to their own type.
isNode :: Structs -> Text -> Bool
isNode types s = any ((== Just s) . structName) (Map.elems (Map.findWithDefault Map.empty s types))

nth :: Int -> [a] -> Maybe a
nth i xs = case drop i xs of
    x : _ -> Just x
    []    -> Nothing

fieldOf :: C.Node (Lexeme Text) -> Maybe (C.Node (Lexeme Text), Lexeme Text)
fieldOf e = case bare e of
    Fix (PointerAccess x f) -> Just (x, f)
    Fix (MemberAccess x f)  -> Just (x, f)
    _                       -> Nothing

fieldKey :: Structs -> Function -> C.Node (Lexeme Text) -> Maybe FieldKey
fieldKey types fn e = do
    (x, L _ _ f) <- fieldOf e
    s <- typeIn types fn x >>= structName
    return (s, f)

-- | The values given to each local variable.
definedAs :: Function -> Map String [C.Node (Lexeme Text)]
definedAs fn = Map.unionWith (++)
    (Map.fromListWith (++) [ (v, [i]) | (v, Just i) <- localDecls (fnDefn fn) ])
    (assignments (fnDefn fn))

-- | The calls an expression makes, directly or through the local it reads.
callsIn :: Map String [C.Node (Lexeme Text)] -> C.Node (Lexeme Text) -> [(C.Node (Lexeme Text), Text, [C.Node (Lexeme Text)])]
callsIn defs e = case bare e of
    Fix (VarExpr (L _ _ v)) -> mapMaybe (call . bare) (Map.findWithDefault [] (Text.unpack v) defs)
    e'                      -> mapMaybe call [e']
  where
    call = \case
        Fix (FunctionCall callee args) | Just name <- getFuncName callee -> Just (callee, name, args)
        _ -> Nothing

-- | Replace a function's parameters in a path by what a call passes.
substitute :: Map String (C.Node (Lexeme Text)) -> AccessPath -> Maybe AccessPath
substitute args = go
  where
    go = \case
        PathDeref (PathVar p) | Just a <- Map.lookup p args -> pointee a
        PathVar p | Just a <- Map.lookup p args -> accessPath (bare a)
        PathDeref p   -> PathDeref <$> go p
        PathField p m -> (`PathField` m) <$> go p
        PathIndex p i -> (`PathIndex` i) <$> go p
        p             -> Just p
    pointee a = case bare a of
        Fix (UnaryExpr UopAddress e) -> accessPath e
        e                            -> PathDeref <$> accessPath e

-- | Whether a size expression makes something larger than it was. Scaling
-- by an element size doesn't.
enlarges :: Map String [C.Node (Lexeme Text)] -> C.Node (Lexeme Text) -> Bool
enlarges defs = go (3 :: Int)
  where
    go 0 _ = False
    go k e = any grows (universe e) || or [ go (k - 1) d | Fix (VarExpr (L _ _ v)) <- universe e, d <- Map.findWithDefault [] (Text.unpack v) defs ]
    grows (Fix n) = case n of
        BinaryExpr _ BopPlus _ -> True
        BinaryExpr a BopMul b  -> any (maybe False (> 1) . intLit) [a, b]
        UnaryExpr UopIncr _    -> True
        _                      -> False

-- | The parameter a size comes from, looking through scaling by an element
-- size.
sizeParam :: Map String Int -> C.Node (Lexeme Text) -> Maybe Int
sizeParam params e = case bare e of
    Fix (VarExpr (L _ _ p))                                       -> Map.lookup (Text.unpack p) params
    Fix (BinaryExpr a BopMul b) | all ((== Nothing) . intLit) [a, b] -> case (sizeParam params a, sizeParam params b) of
        (Just i, _) -> Just i
        (_, j)      -> j
    _ -> Nothing

-- | The growth a function's own statements cause.
localGrowth :: Structs -> Function -> [(Node, Growth)]
localGrowth types fn =
    [ (node, Growth (fnFile fn) (fnName fn) at what (fieldKey types fn l) owner by [])
    | (node, e) <- stmtExprs fn
    , Fix (AssignExpr l AopEq r) <- universe e
    , Just what@(PathField owner _) <- [accessPath (bare l)]
    , Just (at, by) <- [listLink l r, arrayGrowth what r]
    ]
  where
    defs = definedAs fn
    params = Map.fromList (zip (map Text.unpack (fnParams fn)) [0 ..])

    -- A new list node stored where it outlives the function. Linking it to
    -- another node doesn't count, as the limit is on the list's owner.
    listLink l r = case bare r of
        v@(Fix VarExpr{})
            | any (\(_, name, _) -> Map.member name allocators) (callsIn defs r)
            , Just s <- typeIn types fn v >>= structName
            , isNode types s
            , Just (x, f) <- fieldOf l
            , (typeIn types fn x >>= structName) /= Just s ->
                Just (f, Grows)
        _ -> Nothing

    -- A reallocation of the array stored back into the same field.
    arrayGrowth what r = case [ (callee, size) | (callee, name, args) <- callsIn defs r
                                               , Just (ptr, sizeArg) <- [Map.lookup name reallocators]
                                               , (accessPath . bare =<< nth ptr args) == Just what
                                               , Just size <- [nth sizeArg args] ] of
        (callee, size) : _ -> case sizeParam params size of
            Just i -> Just (lexOf callee, ResizedBy i)
            Nothing | enlarges defs size -> Just (lexOf callee, Grows)
                    | otherwise -> Nothing
        [] -> Nothing
    lexOf callee = case bare callee of
        Fix (VarExpr l) -> l
        _               -> L (C.AlexPn 0 0 0) C.IdVar ""

stmtExprs :: Function -> [(Node, C.Node (Lexeme Text))]
stmtExprs fn = [ (node, e) | node <- Map.keys cfg, e <- nodeExprs node ]
  where
    (_, cfg) = fromFunction (fnDefn fn)

-- | Lookup functions, which return a pointer into an array field of an
-- argument, directly or through another lookup.
finders :: Structs -> [Function] -> Map Text FieldKey
finders types fns = go Map.empty
  where
    go known =
        let known' = Map.fromList [ (fnName f, k) | f <- fns, k : _ <- [mapMaybe (into known f) (returned f)] ]
        in if Map.keysSet known' == Map.keysSet known then known' else go known'
    returned f = [ e | Fix (Return (Just e)) <- universe (fnDefn f) ]
    into known f e = case bare e of
        Fix (UnaryExpr UopAddress (Fix (ArrayAccess arr _))) -> fieldKey types f arr
        Fix (BinaryExpr arr BopPlus _)                       -> fieldKey types f arr
        Fix (FunctionCall callee _) | Just g <- getFuncName callee -> Map.lookup g known
        Fix (VarExpr (L _ _ v)) -> case [ k | d <- Map.findWithDefault [] (Text.unpack v) (definedAs f), Just k <- [into known f d] ] of
            k : _ -> Just k
            []    -> Nothing
        _ -> Nothing

-- | Functions that may make a list or array smaller, by lowering a count or
-- calling a function that does.
shrinkers :: [Function] -> Set Text
shrinkers fns = go (Set.fromList [ fnName f | f <- fns, any lowers (universe (fnDefn f)) ])
  where
    go known =
        let known' = Set.union known (Set.fromList [ fnName f | f <- fns, any (`Set.member` known) (callees f) ])
        in if known' == known then known else go known'

lowers :: C.Node (Lexeme Text) -> Bool
lowers (Fix n) = case n of
    UnaryExpr UopDecr e     -> isField e
    AssignExpr e AopMinus _ -> isField e
    _                       -> False
  where
    isField e = isJust (fieldOf e)

-- | Whether this function limits the growth at a node. Either a check like
-- `count >= MAX` returns before it, a failed lookup in a limited array returns
-- before it, or every path after it makes a list smaller, as moving an entry
-- between lists does.
limited :: Context -> Function -> CFG -> Node -> AccessPath -> Bool
limited ctx fn cfg site owner = any guards (Map.keys cfg) || moves
  where
    succs n = map snd (Map.findWithDefault [] n cfg)
    reachable = reach Set.empty
    reach seen [] = seen
    reach seen (n : ns)
        | n `Set.member` seen = reach seen ns
        | otherwise = reach (Set.insert n seen) (succs n ++ ns)
    reaches n = site `Set.member` reachable (succs n)
    bails n = n /= site && not (site `Set.member` reachable [n])

    guards b@(Node _ (BranchNode cond)) =
        reaches b && (compares cond && any bails (succs b) || lookupFails b cond)
    guards _ = False

    -- Capacity, overflow, null, 0 and -1 checks aren't limits. They only say
    -- when to grow, or that something is missing, empty or failed.
    compares cond = or
        [ any ofOwner (expand a) && not (any ofOwner (expand b)) && not (typeMax b) && not (isNullLit b)
          && not (maybe False (<= 0) (intLit b))
        | Fix (BinaryExpr l op r) <- universe cond, op `elem` [BopLt, BopLe, BopGt, BopGe, BopEq, BopNe]
        , (a, b) <- [(l, r), (r, l)] ]
    -- A struct stored inside another is limited by the outer one's limits.
    ofOwner e = case accessPath (bare e) of
        Just (PathField o _) -> holds o owner
        _                    -> False
    holds o p = o == p || case p of
        PathField p' _ -> holds o p'
        _              -> False
    defs = definedAs fn
    expand = go (3 :: Int)
      where
        go 0 e = universe e
        go k e = universe e ++ concat [ go (k - 1) d | Fix (VarExpr (L _ _ v)) <- universe e, d <- Map.findWithDefault [] (Text.unpack v) defs, arithmetic d ]
    arithmetic d = null [ () | Fix n <- universe d, case n of FunctionCall{} -> True; UnaryExpr UopAddress _ -> True; _ -> False ]
    typeMax e = case bare e of
        Fix (LiteralExpr _ (L _ _ t)) -> t `elem` ["SIZE_MAX", "INT_MAX", "UINT_MAX", "INT8_MAX", "UINT8_MAX", "INT16_MAX", "UINT16_MAX", "INT32_MAX", "UINT32_MAX", "INT64_MAX", "UINT64_MAX"]
        _                             -> False

    -- Looking up the thing that grows doesn't limit it.
    lookupFails b cond = case nullTest found cond of
        Just (v, edge) -> rootOf owner /= Just (Text.unpack v)
            && any (\(e, n) -> e == edge && bails n) (Map.findWithDefault [] b cfg)
        Nothing        -> False
    found e = case bare e of
        Fix (VarExpr (L _ _ v))
            | k : _ <- [ k | (_, name, _) <- callsIn (definedAs fn) e, Just k <- [Map.lookup name (ctxFinders ctx)] ]
            , not (k `Set.member` ctxGrowing ctx) -> Just v
        _ -> Nothing

    -- Every way out from the growth passes something making a list smaller.
    moves = not (any isExit (Set.toList (avoiding Set.empty (succs site))))
    avoiding seen [] = seen
    avoiding seen (n : ns)
        | n `Set.member` seen || shrinks n = avoiding seen ns
        | otherwise = avoiding (Set.insert n seen) (succs n ++ ns)
    shrinks n = or
        [ lowers x || maybe False (`Set.member` ctxShrinkers ctx) (getFuncName' x)
        | e <- nodeExprs n, x <- universe e ]
    getFuncName' = \case
        Fix (FunctionCall callee _) -> getFuncName callee
        _                           -> Nothing
    isExit = \case
        Node _ ExitNode -> True
        _               -> False

-- | The growth a function doesn't limit itself.
unlimited :: Context -> Summary -> Function -> [Growth]
unlimited ctx summary fn =
    [ g | (node, g) <- localGrowth (ctxTypes ctx) fn ++ callGrowth, not (limited ctx fn cfg node (growOwner g)) ]
  where
    (_, cfg) = fromFunction (fnDefn fn)
    defs = definedAs fn
    callGrowth =
        [ (node, g { growOwner = owner, growBy = by, growVia = name : growVia g })
        | (node, e) <- stmtExprs fn
        , Fix (FunctionCall callee args) <- universe e
        , Just name <- [getFuncName callee]
        , Just callee' <- [Map.lookup name (ctxFns ctx)]
        , Just grown <- [Map.lookup name summary]
        , let argMap = Map.fromList (zip (map Text.unpack (fnParams callee')) args)
        , g <- Set.toList grown
        , Just owner <- [substitute argMap (growOwner g)]
        , Just by <- [resized args (growBy g)]
        ]
    resized args = \case
        Grows -> Just Grows
        ResizedBy i -> case nth i args of
            Just a | Fix (VarExpr (L _ _ p)) <- bare a, Just j <- List.elemIndex p (fnParams fn) -> Just (ResizedBy j)
                   | enlarges defs a -> Just Grows
            _ -> Nothing

paramRooted :: Function -> Growth -> Bool
paramRooted fn g = maybe False ((`elem` fnParams fn) . Text.pack) (rootOf (growOwner g))

summarise :: Context -> Summary
summarise ctx = bottomUp callees add look (Map.elems (ctxFns ctx)) Map.empty
  where
    add s fn = Map.insert (fnName fn) (Set.fromList (filter (paramRooted fn) (unlimited ctx s fn))) s
    look s fn = Map.findWithDefault Set.empty (fnName fn) s

-- | Struct types, field targets, and the functions passed for each
-- parameter, by function and parameter name.
data Pointers = Pointers Structs FieldTargets (Map (Text, Text) (Set Text))

pointers :: [(FilePath, [C.Node (Lexeme Text)])] -> [Function] -> Pointers
pointers tus fns = Pointers types targets passed
  where
    types = structs tus
    targets = fieldTargets tus
    byName = Map.fromList [ (fnName f, f) | f <- fns ]
    passed = Map.fromListWith Set.union
        [ ((g, p), Set.fromList (valueTargets caller a))
        | caller <- fns
        , Fix (FunctionCall callee args) <- universe (fnDefn caller)
        , Just g <- [getFuncName callee]
        , Just f <- [Map.lookup g byName]
        , (p, a) <- zip (fnParams f) args ]
    valueTargets caller a = case bare a of
        Fix (UnaryExpr UopAddress e) -> valueTargets caller e
        Fix (VarExpr (L _ _ g)) | Map.member g byName -> [g]
        e -> case callTargets types targets caller e of
            Just (Only s)     -> Set.toList s
            Just (Anything s) -> Set.toList s
            Nothing           -> []

-- | The functions each function calls, directly or through a pointer.
calls :: Pointers -> Function -> [Text]
calls (Pointers types targets passed) f =
    [ name | name <- callees f, name `notElem` fnParams f ] ++
    [ t
    | Fix (FunctionCall callee _) <- universe (fnDefn f)
    , t <- case getFuncName callee of
        Just p | p `elem` fnParams f -> Set.toList (Map.findWithDefault Set.empty (fnName f, p) passed)
        Just _ -> []
        Nothing -> case callTargets types targets f callee of
            Just ts -> Set.toList (case ts of Only s -> s; Anything s -> s)
            Nothing -> []
    ]

-- | Functions that read packets from a socket into their own buffer. The
-- analysis starts there.
receivers :: Pointers -> [Function] -> [Function]
receivers ptrs fns =
    [ f
    | f <- fns
    , let buffers = Set.fromList [ v | Fix (VarDecl _ (L _ _ v) (_ : _)) <- universe (fnDefn f) ]
    , not (Set.null buffers)
    , or [ name `Set.member` readers && any (usesBuffer buffers) args
         | Fix (FunctionCall callee args) <- universe (fnDefn f), Just name <- [getFuncName callee] ]
    ]
  where
    usesBuffer buffers a = case bare a of
        Fix (VarExpr (L _ _ v))                                                    -> v `Set.member` buffers
        Fix (BinaryExpr (Fix (VarExpr (L _ _ v))) BopPlus _)                       -> v `Set.member` buffers
        Fix (UnaryExpr UopAddress (Fix (ArrayAccess (Fix (VarExpr (L _ _ v))) _))) -> v `Set.member` buffers
        _                                                                          -> False
    graph = Map.fromList [ (fnName f, calls ptrs f) | f <- fns ]
    -- Functions that end up calling the system's socket reads.
    readers = go (Set.fromList ["recv", "recvfrom"])
      where
        go known =
            let known' = Set.union known (Map.keysSet (Map.filter (any (`Set.member` known)) graph))
            in if known' == known then known else go known'

-- | Growth nothing limits, and the calls packets take to reach it.
data Finding = Finding Growth [Function]

findings :: [(FilePath, [C.Node (Lexeme Text)])] -> [Finding]
findings tus = settle (Set.fromList [ k | f <- fns, (_, g) <- localGrowth types f, Just k <- [growField g] ])
  where
    fns = definitions tus
    types = structs tus
    ptrs = pointers tus fns
    byName = Map.fromList [ (fnName f, f) | f <- fns ]
    found = finders types fns
    shrinking = shrinkers fns

    -- Lookups only limit growth when what they look in is limited, so start
    -- with every field growing and drop the ones found limited until none
    -- change.
    settle growing =
        let result = search growing
            growing' = Set.fromList [ k | Finding g _ <- result, Just k <- [growField g] ]
        in if growing' == growing then result else settle growing'

    search growing = Map.elems $ Map.fromListWith first
        [ (site g, Finding g (chain (fnName fn)))
        | fn <- fns
        , fnName fn `Map.member` reached
        , g <- unlimited ctx summary fn
        , not (paramRooted fn g) || fnName fn `Set.member` receiverNames
        ]
      where
        ctx = Context types byName found shrinking growing
        summary = summarise ctx

    -- One finding for each list or array a function grows.
    site g = (growIn g, case growWhat g of
        PathField o _ -> o
        p             -> p)
    first a@(Finding g _) b@(Finding g' _) = if pos g' <= pos g then b else a
    pos g = let L (C.AlexPn _ l c) _ _ = growAt g in (l, c)

    roots = receivers ptrs fns
    receiverNames = Set.fromList (map fnName roots)
    -- The function each reached function was first reached from.
    reached = bfs (Map.fromList [ (fnName r, Nothing) | r <- roots ]) (map fnName roots)
    bfs seen [] = seen
    bfs seen frontier =
        let fresh = Map.fromList [ (c, Just n) | n <- reverse frontier, c <- reverse (next n), not (c `Map.member` seen) ]
        in bfs (Map.union seen fresh) (Map.keys fresh)
    next n = maybe [] (calls ptrs) (Map.lookup n byName)
    chain n = case Map.lookup n reached of
        Just (Just parent) -> chain parent ++ maybe [] pure (Map.lookup n byName)
        _                  -> maybe [] pure (Map.lookup n byName)

analyse :: [(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse tus = reverse $ flip State.execState [] $ sequence_
    [ warnRich $ Diagnostic pos len ErrorLevel msg Nothing (DiagnosticSpan pos len ["grows here"] : [entrySpan]) [(NoteLevel, path)]
    | Finding g route@(entry : _) <- findings tus
    , let (pos, len) = getDiagnosticInfo (growFile g) (growAt g)
    , let (epos, elen) = getDiagnosticInfo (fnFile entry) (entryLexeme entry)
    , let entrySpan = DiagnosticSpan epos elen ["packets come in here"]
    , let msg = "nothing limits how far" <+> backticks (render (growWhat g)) <+> "grows; reached from" <+> backticks (pretty (fnName entry))
    , let names = map fnName route ++ growVia g
    , let path = "through" <+> mconcat (List.intersperse " -> " (map (backticks . pretty) names))
    ]
  where
    entryLexeme f = case fnDefn f of
        Fix (FunctionDefn _ (Fix (FunctionPrototype _ l _)) _) -> l
        _ -> L (C.AlexPn 0 0 0) C.IdVar (fnName f)

descr :: ([(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("unbounded-growth", Text.unlines
    [ "Checks that every list or array that peers can grow by sending packets"
    , "has a limit. A limit is a check like `count >= MAX` that returns before"
    , "the list grows, in the same function or a caller. Growing at most once for"
    , "each entry of a limited array also counts, and so does moving an entry"
    , "from another list."
    , ""
    , "**Reason:** otherwise a peer can make us use more and more memory by"
    , "sending packets."
    ]))
