{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
-- | The functions a call through a function pointer field may reach.
module Tokstyle.Cimple.Analysis.FunctionPointers
    ( Targets (..)
    , FieldKey
    , FieldTargets
    , fieldTargets
    , callTargets
    ) where

import           Data.Fix                           (Fix (..))
import qualified Data.List                          as List
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Maybe                         (isJust)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Language.Cimple                    (Lexeme (..), NodeF (..),
                                                     UnaryOp (..))
import qualified Language.Cimple                    as C
import           Tokstyle.Cimple.Analysis.Functions (Function (..), constName,
                                                     definitions, localDecls,
                                                     universe, valuesUsed)
import           Tokstyle.Cimple.Analysis.Types     (Structs, bare,
                                                     callbackType, structName,
                                                     structs, typeIn)
import           Tokstyle.Common                    (isPublicHeader)


data Targets
    = Only (Set Text)
    -- ^ One of these functions, or null.
    | Anything (Set Text)
    -- ^ One of these, or anything a client stores.
    deriving (Eq, Show)

instance Semigroup Targets where
    a <> b = case (a, b) of
        (Only x, Only y) -> Only (Set.union x y)
        _                -> Anything (Set.union (known a) (known b))
      where
        known (Only x)     = x
        known (Anything x) = x

-- | A struct and one of its fields.
type FieldKey = (Text, Text)

type FieldTargets = Map FieldKey Targets

-- | A function and the index of one of its parameters.
type ParamKey = (Text, Int)

-- | Where a stored value comes from.
data Source
    = Value Targets
    | FromField FieldKey
    | FromParam ParamKey

-- | The functions code stores in each field, also via other fields and
-- parameters. Fields clients can set, or that get something we can't follow,
-- may hold anything.
fieldTargets :: [(FilePath, [C.Node (Lexeme Text)])] -> FieldTargets
fieldTargets tus = Map.fromList [ (k, t) | (Left k, t) <- Map.toList resolved ]
  where
    resolved = solveSources (Map.fromListWith (++)
        [ (Left k, [src]) | (k, src) <- public ++ initialised ++ assigned ]
        `Map.union` Map.fromListWith (++) [ (Right k, [src]) | (k, src) <- passed ])

    types = structs tus
    order = fieldOrder tus
    fns = definitions tus
    functions = Set.fromList $ concat
        [ case n of
            FunctionDefn _ (Fix (FunctionPrototype _ (L _ _ name) _)) _ -> [name]
            FunctionDecl _ (Fix (FunctionPrototype _ (L _ _ name) _))   -> [name]
            _                                                         -> []
        | (_, nodes) <- tus, node <- nodes, Fix n <- universe node ]
    isCallback s f = isJust (Map.lookup s types >>= Map.lookup f >>= callbackType)
    clientFile file = isPublicHeader file || "tox_private.h" `List.isSuffixOf` file

    -- Clients set the structs in public headers and @tox_private.h@.
    public =
        [ ((s, f), Value (Anything Set.empty))
        | (file, nodes) <- tus, clientFile file
        , node <- nodes, Fix n <- universe node
        , s <- case n of
            Struct (L _ _ s') _                     -> [s']
            Typedef (Fix (Struct _ _)) (L _ _ s') _ -> [s']
            _                                       -> []
        , f <- Map.keys (Map.findWithDefault Map.empty s types), isCallback s f ]

    source fn e = case bare e of
        Fix (UnaryExpr UopAddress f@(Fix (VarExpr (L _ _ g)))) | g `Set.member` functions -> source fn f
        Fix (VarExpr (L _ _ g))
            | Just fn' <- fn, Just i <- List.elemIndex g (fnParams fn') -> FromParam (fnName fn', i)
            | Just fn' <- fn, g `elem` [ Text.pack v | (v, _) <- localDecls (fnDefn fn') ] -> Value (Anything Set.empty)
            | g `Set.member` functions -> Value (Only (Set.singleton g))
        e' | constName e' `elem` [Just "nullptr", Just "NULL"] -> Value (Only Set.empty)
        e' | Just fn' <- fn, Just (x, g) <- fieldOf e'
           , Just s <- typeIn types fn' x >>= structName, isCallback s g -> FromField (s, g)
        _ -> Value (Anything Set.empty)

    initialised = concat
        [ initList s vals
        | (_, nodes) <- tus, node <- nodes, Fix n <- universe node
        , (ty, Fix (InitialiserList vals)) <- case n of
            ConstDefn _ t _ v                          -> [(t, v)]
            VarDeclStmt (Fix (VarDecl t _ _)) (Just v) -> [(t, v)]
            CompoundLiteral t v                        -> [(t, v)]
            _                                          -> []
        , Just s <- [structName ty] ]
    initList s vals = concat
        [ case v of
            Fix (InitialiserList inner) | Just s' <- Map.lookup s types >>= Map.lookup f >>= structName ->
                initList s' inner
            _ | isCallback s f -> [((s, f), source Nothing v)]
            _                  -> []
        | (f, v) <- zip (Map.findWithDefault [] s order) vals ]

    assigned =
        [ store
        | fn <- fns
        , Fix (AssignExpr l _ r) <- universe (fnDefn fn)
        , Just (x, f) <- [fieldOf l]
        , store <- case typeIn types fn x >>= structName of
            Just s | isCallback s f -> [((s, f), source (Just fn) r)]
                   | otherwise      -> []
            -- We don't know the struct, so any with this callback field may
            -- hold anything.
            Nothing -> [ ((s, f), Value (Anything Set.empty)) | s <- Map.keys types, isCallback s f ]
        ]

    -- Public functions and functions used as values may get anything.
    passed =
        [ ((fnName callee, i), source (Just fn) a)
        | fn <- fns
        , Fix (FunctionCall (Fix (VarExpr (L _ _ g))) args) <- universe (fnDefn fn)
        , Just callee <- [Map.lookup g byName]
        , (i, a) <- zip [0 ..] args ]
        ++ [ ((fnName f, i), Value (Anything Set.empty))
           | f <- fns, fnName f `Set.member` open, i <- [0 .. length (fnParams f) - 1] ]
    byName = Map.fromList [ (fnName f, f) | f <- fns ]
    open = Set.union clientCalled usedAsValue
    clientCalled = Set.fromList
        [ name
        | (file, nodes) <- tus, clientFile file, node <- nodes
        , Fix (FunctionDecl _ (Fix (FunctionPrototype _ (L _ _ name) _))) <- universe node ]
    usedAsValue = Set.fromList
        [ g | (_, nodes) <- tus, node <- nodes, g <- valuesUsed node, g `Set.member` functions ]

solveSources :: Map (Either FieldKey ParamKey) [Source] -> Map (Either FieldKey ParamKey) Targets
solveSources sources = go (Map.map (const (Only Set.empty)) sources)
  where
    go known =
        let known' = Map.map (foldr ((<>) . look known) (Only Set.empty)) sources
        in if known' == known then known else go known'
    look known = \case
        Value t     -> t
        FromField k -> Map.findWithDefault (Only Set.empty) (Left k) known
        FromParam k -> Map.findWithDefault (Only Set.empty) (Right k) known

fieldOf :: C.Node (Lexeme Text) -> Maybe (C.Node (Lexeme Text), Text)
fieldOf e = case bare e of
    Fix (PointerAccess x (L _ _ f)) -> Just (x, f)
    Fix (MemberAccess x (L _ _ f))  -> Just (x, f)
    _                               -> Nothing

fieldOrder :: [(FilePath, [C.Node (Lexeme Text)])] -> Map Text [Text]
fieldOrder tus = Map.fromList
    [ (name, [ f | Fix (MemberDecl (Fix (VarDecl _ (L _ _ f) _)) _) <- members ])
    | (_, nodes) <- tus, node <- nodes, Fix n <- universe node
    , (name, members) <- case n of
        Struct (L _ _ s) ms                     -> [(s, ms)]
        Typedef (Fix (Struct _ ms)) (L _ _ s) _ -> [(s, ms)]
        _                                       -> []
    ]

callTargets :: Structs -> FieldTargets -> Function -> C.Node (Lexeme Text) -> Maybe Targets
callTargets types targets fn callee = do
    (x, f) <- fieldOf callee
    s <- typeIn types fn x >>= structName
    Map.lookup (s, f) targets
