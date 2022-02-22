{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wwarn #-}
module Tokstyle.Linter.TypeCheck where

import           Control.Monad                (foldM, void, zipWithM)
import           Control.Monad.State.Strict   (State)
import qualified Control.Monad.State.Strict   as State
import           Data.Fix                     (Fix (..), foldFixM)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Debug.Trace                  (trace)
import           GHC.Stack                    (HasCallStack)
import           Language.Cimple              (AssignOp (..), BinaryOp (..),
                                               Lexeme (..), LiteralType (..),
                                               Node, NodeF (..), UnaryOp (..))
import           Language.Cimple.Diagnostics  (HasDiagnostics (..), warn)
import           Language.Cimple.TraverseAst  (AstActions, astActions, doNode,
                                               traverseAst)
import           Text.PrettyPrint.ANSI.Leijen (Pretty (..), colon, int, text,
                                               vcat, (<+>))


data Type
    -- C types
    = T_Void
    | T_Null
    | T_Bool
    | T_Char
    | T_Int
    | T_ArrDim
    | T_Var {-# UNPACK #-} Int
    | T_Name Text
    | T_Arr Type
    | T_Ptr Type
    | T_Func Type [Type]
    | T_InitList [Type]
    deriving (Show, Eq)

stdTypes :: Map Text Type
stdTypes = Map.fromList
    [ ("void"               , T_Name "void")
    , ("bool"               , T_Bool)
    , ("char"               , T_Char)
    , ("float"              , T_Int)
    , ("double"             , T_Int)
    , ("size_t"             , T_Int)
    , ("int"                , T_Int)
    , ("unsigned"           , T_Int)
    , ("unsigned int"       , T_Int)
    , ("long"               , T_Int)
    , ("unsigned long"      , T_Int)
    , ("long long"          , T_Int)
    , ("unsigned long long" , T_Int)
    , ("int8_t"             , T_Int)
    , ("uint8_t"            , T_Int)
    , ("int16_t"            , T_Int)
    , ("uint16_t"           , T_Int)
    , ("int32_t"            , T_Int)
    , ("uint32_t"           , T_Int)
    , ("int64_t"            , T_Int)
    , ("uint64_t"           , T_Int)
    ]

data Env = Env
    { envDiags   :: [Text]
    , envLocals  :: [Text]
    , envTypes   :: Map Text Type
    , envVars    :: Map Int Type
    , envNextVar :: Int
    }
    deriving (Show)

instance HasDiagnostics Env where
    addDiagnostic diag env@Env{envDiags} = env{envDiags = addDiagnostic diag envDiags}

instance Pretty Env where
    pretty env = vcat
        [ vcat
          . map (\(k, v) -> text (Text.unpack k) <+> colon <+> text (show v))
          $ resolved
        , vcat
          . map (\(k, v) -> int k <+> colon <+> text (show v))
          . Map.assocs
          . envVars
          $ env
        ]
      where
        resolved :: [(Text, Type)]
        resolved = flip State.evalState env $ do
            tys <- Map.assocs . envTypes <$> State.get
            mapM (\(n, ty) -> (n,) <$> unify ty ty) tys


typeError :: (HasCallStack, Show a) => a -> State Env b
typeError x = do
    env <- State.get
    error (show $ vcat [text (show x), pretty env])

empty :: Env
empty = Env{..}
  where
    envDiags = []
    envLocals = []
    envTypes = Map.fromList
        [ ("nullptr"       , T_Ptr T_Null)
        -- TODO(iphydf): Can't deal with variadic functions yet.
        , ("LOGGER_ASSERT" , T_Func T_Void [T_Ptr (T_Name "Logger"), T_Bool, T_Ptr T_Char])
        , ("LOGGER_DEBUG"  , T_Func T_Void [T_Ptr (T_Name "Logger"), T_Ptr T_Char])
        , ("LOGGER_ERROR"  , T_Func T_Void [T_Ptr (T_Name "Logger"), T_Ptr T_Char])
        , ("LOGGER_FATAL"  , T_Func T_Void [T_Ptr (T_Name "Logger"), T_Ptr T_Char])
        , ("LOGGER_INFO"   , T_Func T_Void [T_Ptr (T_Name "Logger"), T_Ptr T_Char])
        , ("LOGGER_TRACE"  , T_Func T_Void [T_Ptr (T_Name "Logger"), T_Ptr T_Char])
        , ("LOGGER_WARNING", T_Func T_Void [T_Ptr (T_Name "Logger"), T_Ptr T_Char])
        , ("vpx_codec_control", T_Func T_Void [])
        , ("memset", T_Func T_Void [])
        , ("snprintf", T_Func T_Void [])
        , ("strerror_r", T_Func T_Void [])
        ]
    envVars = Map.empty
    envNextVar = 0

newTyVar :: State Env Type
newTyVar = do
    env@Env{envNextVar} <- State.get
    State.put env{envNextVar = envNextVar + 1}
    return $ T_Var envNextVar

addTyVar :: Int -> Type -> State Env ()
addTyVar v ty =
    State.modify $ \env@Env{envVars} ->
        env{envVars = Map.insert v ty envVars}

addLocal :: Text -> State Env ()
addLocal n = State.modify $ \env@Env{envLocals} -> env{envLocals = n:envLocals}

dropLocals :: State Env ()
dropLocals = State.modify $ \env@Env{envLocals, envTypes} ->
    env{envTypes = foldr Map.delete envTypes envLocals, envLocals = []}

addName :: HasCallStack => Text -> Type -> State Env ()
addName n ty = do
    trace ("addName: " <> show n) $ return ()
    found <- Map.lookup n . envTypes <$> State.get
    case found of
      Nothing -> State.modify $ \env@Env{envTypes} -> env{envTypes = Map.insert n ty envTypes}
      Just ty' -> void $ unify ty ty'

getName :: Text -> State Env Type
getName n = do
    trace ("getName: " <> show n) $ return ()
    found <- Map.lookup n . envTypes <$> State.get
    case found of
      Just ok -> return ok
      Nothing -> do
          ty <- newTyVar
          addName n ty
          return ty

resolve :: Int -> State Env (Maybe Type)
resolve v = Map.lookup v . envVars <$> State.get


unify :: HasCallStack => Type -> Type -> State Env Type
unify l r = -- trace ("unify: " <> show (l, r)) $
    go l r
  where
    go a (T_Var b) = do
        res <- resolve b
        case res of
          Nothing -> do
              addTyVar b a
              return a
          Just resolved@T_Var{} ->
              return resolved
          Just resolved ->
              unify a resolved
    go a@T_Var{} b = unify b a

    -- Equal types unify trivially.
    go a b | a == b = return a

    go a@T_Arr{} (T_Ptr T_Null) = return a
    go (T_Ptr T_Null) b@T_Arr{} = return b

    go a@T_Func{} (T_Ptr T_Null) = return a
    go (T_Ptr T_Null) b@T_Func{} = return b

    -- TODO(iphydf): Remove once all implicit bool conversions are fixed.
    go T_Bool T_Int = return T_Int
    go T_Int T_Bool = return T_Int

    -- Array and pointer types can unify.
    go (T_Arr a) (T_Ptr b) = unify a b
    go (T_Ptr a) (T_Arr b) = unify a b

    -- Arrays unify with all elements in their initialiser list.
    go (T_Arr a) (T_InitList b) = foldM unify a b

    go a (T_Name name) = unify a =<< getName name
    go (T_Name name) b = unify b =<< getName name

    -- Dereference function pointers for unification.
    go a@T_Func{} (T_Ptr b) = unify a b
    go (T_Ptr a) b@T_Func{} = unify a b

    go (T_InitList la) (T_InitList lb) = do
        T_InitList <$> zipWithM unify la lb

    go (T_Func ra argsa) (T_Func rb argsb) =
        T_Func <$> unify ra rb <*> zipWithM unify argsa argsb

    -- `void` and null unify with anything.
    go a T_Void = return a
    go T_Void b = return b
    -- `void *` unifies with any pointer type.
    go a@T_Ptr{} (T_Ptr T_Void) = return a
    go (T_Ptr T_Void) b@T_Ptr{} = return b
    -- Incompatible pointers unify to `void *`.
    go T_Ptr{} T_Ptr{} = return $ T_Ptr T_Void

    go a b = typeError (a, b)


inferBinaryExpr :: BinaryOp -> Type -> Type -> State Env Type
inferBinaryExpr BopMinus T_Ptr{} T_Ptr{}  = return T_Int
inferBinaryExpr BopAnd l r    = foldM unify T_Bool [l, r]
inferBinaryExpr BopOr l r     = foldM unify T_Bool [l, r]
inferBinaryExpr BopLe l r     = const T_Bool <$> unify l r
inferBinaryExpr BopLt l r     = const T_Bool <$> unify l r
inferBinaryExpr BopGe l r     = const T_Bool <$> unify l r
inferBinaryExpr BopGt l r     = const T_Bool <$> unify l r
inferBinaryExpr BopEq l r     = const T_Bool <$> unify l r
inferBinaryExpr BopNe l r     = const T_Bool <$> unify l r
inferBinaryExpr BopMul l r    = unify l r
inferBinaryExpr BopMod l r    = unify l r
inferBinaryExpr BopDiv l r    = unify l r
inferBinaryExpr BopMinus l r  = unify l r
inferBinaryExpr BopPlus l _   = return l
inferBinaryExpr BopBitOr l r  = unify l r
inferBinaryExpr BopBitAnd l r = unify l r
inferBinaryExpr BopBitXor l r = unify l r
inferBinaryExpr BopLsh l r    = unify l r
inferBinaryExpr BopRsh l r    = unify l r


inferUnaryExpr :: UnaryOp -> Type -> State Env Type
inferUnaryExpr UopAddress e = return $ T_Ptr e
inferUnaryExpr UopIncr e    = return e
inferUnaryExpr UopDecr e    = return e
inferUnaryExpr UopNeg e     = unify T_Int e
inferUnaryExpr UopMinus e   = unify T_Int e
inferUnaryExpr UopNot e     = unify T_Bool e
inferUnaryExpr UopDeref e = do
    memTy <- newTyVar
    void $ unify e (T_Ptr memTy)
    return memTy


inferTypes :: NodeF (Lexeme Text) Type -> State Env Type
inferTypes = \case
    SizeofExpr{} -> return T_Int
    SizeofType{} -> return T_Int
    LiteralExpr Bool _ -> return T_Bool
    LiteralExpr Char _ -> return T_Char
    LiteralExpr Int _ -> return T_Int
    LiteralExpr String _ -> return $ T_Ptr T_Char
    InitialiserList tys -> return $ T_InitList tys

    TyStruct (L _ _ name) -> return $ T_Name name
    TyFunc (L _ _ name) -> return $ T_Name name
    TyStd (L _ _ name) ->
        case Map.lookup name stdTypes of
          Nothing -> return $ T_Name name -- typeError name
          Just ty -> return ty
    TyUserDefined (L _ _ name) -> return $ T_Name name
    TyPointer ty -> return $ T_Ptr ty
    TyConst ty -> return ty
    Ellipsis -> return T_Void

    VLA ty (L _ _ name) size -> do
        void $ unify T_Int size
        addLocal name
        addName name $ T_Arr ty
        return T_Void
    DeclSpecArray{} -> return T_ArrDim
    VarDecl ty (L _ _ name) arrs -> do
        let ty' = foldr (const T_Arr) ty arrs
        addLocal name
        addName name ty'
        return ty'
    VarDeclStmt _ Nothing -> return T_Void
    VarDeclStmt decl (Just initExpr) -> do
        void $ unify decl initExpr
        return T_Void
    PreprocDefineConst (L _ _ name) ty -> do
        addName name ty
        return T_Void

    VarExpr (L _ _ name) -> getName name
    LiteralExpr ConstId (L _ _ name) -> getName name
    CastExpr ty _ -> return ty
    CompoundLiteral ty _ -> return ty
    DoWhileStmt body c -> const body <$> unify c T_Bool
    WhileStmt c body -> const body <$> unify c T_Bool
    ForStmt _ c _ body -> const body <$> unify c T_Bool
    SwitchStmt _ body -> foldM unify T_Void body
    IfStmt c t (Just e) -> do
        void $ unify c T_Bool
        unify t e
    IfStmt c t Nothing -> const t <$> unify c T_Bool
    CompoundStmt body -> do
        trace (show body) $ return ()
        foldM unify T_Void body
    Return (Just ty) -> return ty
    Return Nothing -> return T_Void
    ParenExpr e -> return e
    AssignExpr l AopPlus _ -> return l
    AssignExpr l AopMinus _ -> return l
    AssignExpr l _ r -> unify l r
    BinaryExpr l op r -> inferBinaryExpr op l r
    UnaryExpr op e -> inferUnaryExpr op e
    TernaryExpr c t e -> do
        void $ unify T_Bool c
        unify t e

    FunctionPrototype retTy (L _ _ name) args -> do
        let ty = T_Func retTy args
        addName name ty
        return ty
    FunctionCall callee args -> do
        retTy <- newTyVar
        funTy <- unify (T_Func retTy args) callee
        case funTy of
          T_Func result _ -> return result
          _               -> typeError funTy
    FunctionDefn _ (T_Func r _) body -> do
        dropLocals
        unify r body

    ExprStmt{} -> return T_Void
    Break -> return T_Void
    Continue -> return T_Void
    Goto{} -> return T_Void
    Default e -> return e
    Case _ e -> return e
    Label _ e -> return e
    Comment{} -> return T_Void
    CommentExpr _ e -> return e

    PreprocDefineMacro{} -> return T_Void
    PreprocUndef{} -> return T_Void
    PreprocScopedDefine _ body _ -> foldM unify T_Void body
    MacroParam{} -> return T_Void
    MacroBodyStmt body -> return body

    StaticAssert{} -> return T_Void

    PreprocDefined{} -> return T_Bool
    PreprocIf _ t e -> foldM unify e t
    PreprocIfdef _ t e -> foldM unify e t
    PreprocIfndef _ t e -> foldM unify e t
    PreprocElif _ t e -> foldM unify e t
    PreprocElse body -> foldM unify T_Void body

    ArrayAccess arr idx -> do
        memTy <- newTyVar
        void $ unify arr (T_Ptr memTy)
        void $ unify idx T_Int
        return memTy

    -- TODO(iphydf): Implement.
    MemberAccess{} -> return T_Void
    PointerAccess{} -> return T_Void

    x -> typeError x

linter :: AstActions (State Env) Text
linter = astActions
    { doNode = \_file node act ->
        case unFix node of
            FunctionDefn{} -> void $ foldFixM inferTypes node

            _              -> act
    }

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse = reverse . envDiags . flip State.execState empty . traverseAst linter
