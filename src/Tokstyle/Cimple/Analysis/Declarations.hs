{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}
-- | The types, functions, constants and macros a program declares.
module Tokstyle.Cimple.Analysis.Declarations
    ( Ast
    , Type (..)
    , Scheme (..)
    , Hole
    , vars
    , records
    , TypeDef (..)
    , Record (..)
    , Define (..)
    , Key
    , Body (..)
    , Global (..)
    , Program (..)
    , collect
    , flatten
    , subnodes
    , fromAst
    , arrays
    , parameters
    , paramName
    , macroParams
    , isVoid
    ) where

import           Control.Monad.State.Strict (State, evalStateT, get, lift, put,
                                             runState, state)
import           Data.Fix                   (Fix (..))
import           Data.Foldable              (toList)
import           Data.IntMap.Strict         (IntMap)
import qualified Data.IntMap.Strict         as IntMap
import           Data.List                  (foldl')
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import           Language.Cimple            (Lexeme (..), Node, NodeF (..),
                                             Scope (..))

type Ast = Node (Lexeme Text)

data Type
    = TVoid
    -- | Integers, enums, floats and bool. They convert to each other, but a
    -- pointer to one is not a pointer to another.
    | TNum Text
    | TPtr Type
    -- | Parameter types, return type, and whether the function is variadic.
    | TFun [Type] Type Bool
    -- | A struct or union, with one type argument for each of its void pointers.
    | TRec Text [Type]
    -- | A type the program doesn't define.
    | TExt Text
    | TNull
    -- | A type variable: the target of a void pointer, or the type of a name the
    -- program doesn't define.
    | TVar Int
    -- | In a field's type: the nth type argument of its struct.
    | TParam Int
    -- | The target of a void pointer that accepts any type.
    | TAny
    deriving (Eq, Ord, Show)

data Scheme = Forall [Int] Type

vars :: Type -> [Int]
vars = \case
    TVar a      -> [a]
    TPtr t      -> vars t
    TFun ps r _ -> concatMap vars (r : ps)
    TRec _ as   -> concatMap vars as
    _           -> []

records :: Type -> [(Text, [Type])]
records = \case
    TPtr t      -> records t
    TFun ps r _ -> concatMap records (r : ps)
    TRec n as   -> (n, as) : concatMap records as
    _           -> []

--------------------------------------------------------------------------------
-- The program's declarations

-- | A struct or callback type, and the index of one of its void pointers.
type Hole = (Text, Int)

data TypeDef
    -- | Another name for a type, with the number of array dimensions.
    = Alias Ast Int
    | RecordDef Text
    | EnumDef
    | FuncDef Ast

data Record = Record
    { recArity     :: Int
    -- | The void pointers that are parameters of a callback type.
    , recCallbacks :: IntMap Hole
    , recStruct    :: Bool
    , recOrder     :: [Text]
    , recFields    :: Map Text Type
    }

data Define
    = Constant Ast
    | Macro [Text] Ast

-- | Static functions and constants are keyed by their file.
type Key = (Maybe FilePath, Text)

-- | A definition: its file, the macros defined above it, its parameter names
-- and its body.
data Body = Body FilePath (Map Text Define) [Maybe Text] Ast

data Global
    -- | A function's prototype and its definitions.
    = Function Ast [Body]
    -- | A constant's type and its values.
    | Variable Ast [Body]

data Program = Program
    { pTypes       :: Map Text TypeDef
    , pMembers     :: Map Text (Bool, [(Text, Ast, Int)])
    , pRecords     :: Map Text Record
    , pGlobals     :: Map Key Global
    , pEnumerators :: Map Text Text
    , pDefines     :: Map Text Define
    }

flatten :: Ast -> [Ast]
flatten n@(Fix node) = case node of
    PreprocIf _ t e            -> concatMap flatten t ++ flatten e
    PreprocIfdef _ t e         -> concatMap flatten t ++ flatten e
    PreprocIfndef _ t e        -> concatMap flatten t ++ flatten e
    PreprocElif _ t e          -> concatMap flatten t ++ flatten e
    PreprocElse es             -> concatMap flatten es
    PreprocScopedDefine d es u -> d : concatMap flatten es ++ [u]
    ExternC es                 -> concatMap flatten es
    Group es                   -> concatMap flatten es
    Commented _ e              -> flatten e
    CommentSection _ es _      -> concatMap flatten es
    _                          -> [n]

subnodes :: Ast -> [Ast]
subnodes n = n : concatMap subnodes (toList (unFix n))

collect :: [(FilePath, [Ast])] -> Program
collect tus = prog { pRecords = Map.map (template prog) (pMembers prog) }
  where
    prog = foldl' file (Program Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty) tus

    file p (path', nodes) = fst (foldl' (decl path') (p, Map.empty) (concatMap flatten nodes))

    decl path' (p, defs) (Fix node) = case node of
        Typedef (Fix (Struct (L _ _ tag) ms)) (L _ _ n) _ -> (aggregate True tag ms (typedef n (RecordDef tag)), defs)
        Typedef (Fix (Union (L _ _ tag) ms)) (L _ _ n) _ -> (aggregate False tag ms (typedef n (RecordDef tag)), defs)
        Typedef (Fix (TyStruct (L _ _ tag))) (L _ _ n) _ -> (typedef n (RecordDef tag), defs)
        Typedef (Fix (TyUnion (L _ _ tag))) (L _ _ n) _ -> (typedef n (RecordDef tag), defs)
        Typedef (Fix (EnumDecl _ es (L _ _ n))) _ _ -> (enum n es, defs)
        Typedef ty (L _ _ n) dims -> (typedef n (Alias ty (length dims)), defs)
        TypedefFunction proto@(Fix (FunctionPrototype _ (L _ _ n) _)) -> (typedef n (FuncDef proto), defs)
        AggregateDecl (Fix (Struct (L _ _ tag) ms)) -> (aggregate True tag ms p, defs)
        AggregateDecl (Fix (Union (L _ _ tag) ms)) -> (aggregate False tag ms p, defs)
        EnumDecl _ es (L _ _ n) -> (enum n es, defs)
        EnumConsts _ es -> (enumerators "int" es p, defs)
        FunctionDecl scope proto@(Fix (FunctionPrototype _ (L _ _ n) _)) ->
            (declare' (key scope n) (Function proto []), defs)
        FunctionDefn scope proto@(Fix (FunctionPrototype _ (L _ _ n) ps)) body ->
            (declare' (key scope n) (Function proto [Body path' defs (map paramName (fst (parameters ps))) body]), defs)
        ConstDecl ty (L _ _ n) -> (declare' (Nothing, n) (Variable ty []), defs)
        ConstDefn scope ty (L _ _ n) value ->
            (declare' (key scope n) (Variable ty [Body path' defs [] value]), defs)
        PreprocDefineConst (L _ _ n) e -> define' n (Constant e)
        PreprocDefineMacro (L _ _ n) ps e -> define' n (Macro (macroParams ps) e)
        PreprocUndef (L _ _ n) -> (p, Map.delete n defs)
        _ -> (p, defs)
      where
        typedef n d = p { pTypes = Map.insert n d (pTypes p) }
        enum n es = enumerators n es (typedef n EnumDef)
        declare' k g = p { pGlobals = Map.insertWith merge k g (pGlobals p) }
        define' n d = (p { pDefines = Map.insert n d (pDefines p) }, Map.insert n d defs)
        key scope n = (if scope == Static then Just path' else Nothing, n)

    merge (Function _ new) (Function proto old) = Function proto (old ++ new)
    merge (Variable _ new) (Variable ty old)    = Variable ty (old ++ new)
    merge _ old                                 = old

    aggregate isStruct tag ms p = p { pMembers = Map.insert tag (isStruct, members ms) (pMembers p) }
    members ms = [ (n, ty, length dims) | Fix (MemberDecl (Fix (VarDecl ty (L _ _ n) dims)) _) <- concatMap flatten ms ]
    enumerators n es p = p { pEnumerators = foldr (`Map.insert` n) (pEnumerators p)
        [ e | Fix (Enumerator (L _ _ e) _) <- concatMap flatten es ] }

macroParams :: [Ast] -> [Text]
macroParams ps = [ n | Fix (MacroParam (L _ _ n)) <- ps ]

-- | Numbers the void pointers in a struct's fields.
template :: Program -> (Bool, [(Text, Ast, Int)]) -> Record
template prog (isStruct, ms) = Record arity (IntMap.fromList callbacks) isStruct (map fst fields) (Map.fromList fields)
  where
    (fields, (arity, callbacks)) = runState (mapM field ms) (0, [])
    -- Nested structs get their type arguments when the field is read.
    field :: (Text, Ast, Int) -> State (Int, [(Int, Hole)]) (Text, Type)
    field (n, ty, dims) = (n,) . arrays dims <$> fromAst prog hole (const (pure [])) ty
    hole :: Maybe Hole -> State (Int, [(Int, Hole)]) Type
    hole h = state $ \(i, cs) -> (TParam i, (i + 1, maybe id ((:) . (i,)) h cs))

arrays :: Int -> Type -> Type
arrays dims t = iterate TPtr t !! dims

-- | `hole` returns the target type of a void pointer, given the callback type
-- it is a parameter of. `args` returns the type arguments of a struct.
fromAst :: Monad m => Program -> (Maybe Hole -> m Type) -> (Text -> m [Type]) -> Ast -> m Type
fromAst prog hole args = flip evalStateT Nothing . go
  where
    go (Fix node) = case node of
        TyPointer t | isVoid t  -> TPtr <$> next
                    | otherwise -> TPtr <$> go t
        TyConst t                -> go t
        TyOwner t                -> go t
        TyNonnull t              -> go t
        TyNullable t             -> go t
        TyBitwise t              -> go t
        TyForce t                -> go t
        TyStd (L _ _ "void")     -> pure TVoid
        TyStd (L _ _ n)
            | n `elem` numbers   -> pure (TNum n)
            | otherwise          -> pure (TExt n)
        TyStruct (L _ _ tag)     -> record tag
        TyUnion (L _ _ tag)      -> record tag
        TyFunc (L _ _ n)         -> named n
        TyUserDefined (L _ _ n)  -> named n
        FunctionPrototype ret _ ps -> TFun <$> mapM param (fst (parameters ps)) <*> go ret <*> pure (snd (parameters ps))
        _                        -> next

    -- The state is the callback type being read and the index of its next void
    -- pointer.
    next = do
        callback' <- get
        put (fmap (+ 1) <$> callback')
        lift (hole callback')

    named n = case Map.lookup n (pTypes prog) of
        Just (Alias ty dims) -> arrays dims <$> go ty
        Just (RecordDef tag) -> record tag
        Just EnumDef         -> pure (TNum n)
        Just (FuncDef proto) -> get >>= \case
            Nothing -> put (Just (n, 0)) *> go proto <* put Nothing
            Just _  -> go proto
        Nothing              -> record n

    record tag
        | Map.member tag (pMembers prog) = TRec tag <$> lift (args tag)
        | otherwise = pure (TExt tag)

    param (Fix (VarDecl ty _ dims)) = arrays (length dims) <$> go ty
    param ty                        = go ty

numbers :: [Text]
numbers =
    [ "bool", "char", "double", "float", "int", "long", "long int", "long signed int"
    , "long unsigned int", "signed int", "unsigned", "unsigned int", "unsigned long"
    , "unsigned long long", "size_t"
    ] ++ [ sign <> "int" <> bits <> "_t" | sign <- ["", "u"], bits <- ["8", "16", "32", "64"] ]

-- | Returns a function's parameters, and whether it is variadic.
parameters :: [Ast] -> ([Ast], Bool)
parameters ps = (filter (not . isEllipsis) ps', any isEllipsis ps')
  where
    ps' = filter (not . isVoid) ps
    isEllipsis = (== Ellipsis) . unFix

isVoid :: Ast -> Bool
isVoid (Fix node) = case node of
    TyStd (L _ _ "void") -> True
    TyConst t            -> isVoid t
    TyOwner t            -> isVoid t
    TyNonnull t          -> isVoid t
    TyNullable t         -> isVoid t
    VarDecl t _ []       -> isVoid t
    _                    -> False

paramName :: Ast -> Maybe Text
paramName (Fix (VarDecl _ (L _ _ n) _)) = Just n
paramName _                             = Nothing
