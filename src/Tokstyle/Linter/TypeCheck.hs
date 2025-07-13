{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TupleSections     #-}
module Tokstyle.Linter.TypeCheck (descr) where

import           Control.Monad               (foldM, void, zipWithM)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..), foldFixM)
import           Data.IntMap.Strict          (IntMap)
import qualified Data.IntMap.Strict          as IntMap
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Debug.Trace                 (traceM)
import           GHC.Stack                   (HasCallStack)
import           Language.Cimple             (AssignOp (..), BinaryOp (..),
                                              Lexeme (..), LiteralType (..),
                                              Node, NodeF (..), UnaryOp (..))
import           Language.Cimple.Diagnostics (HasDiagnostics (..))
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Prettyprinter               (Pretty (..), colon, vcat, (<+>))


wantTrace :: Bool
wantTrace = False

traceMaybe :: Monad m => String -> m ()
traceMaybe = if wantTrace
    then traceM
    else const $ return ()


{-# ANN module ("HLint: ignore Use camelCase"::String) #-}
data Type
    -- C types
    = T_Var {-# UNPACK #-} Int
    | T_Intersect Type Type
    | T_Bot
    | T_Top
    | T_Void
    | T_Bool
    | T_Char
    | T_Int
    | T_ArrDim
    | T_Name Text
    | T_Arr Type
    | T_Ptr Type
    | T_Func Type [Type]
    | T_InitList [Type]
    | T_Struct (Map Text Type)
    | T_Add Type Type
    | T_Sub Type Type
    deriving (Show, Eq, Ord)

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

unionWithM :: (Monad m, Ord k) => (a -> a -> m a) -> Map k a -> Map k a -> m (Map k a)
unionWithM f mapA mapB =
  sequence $ Map.unionWith (\a b -> do {x <- a; y <- b; f x y}) (Map.map return mapA) (Map.map return mapB)

data Env = Env
    { envDiags   :: [Text]
    , envLocals  :: [Text]
    , envTypes   :: Map Text Type
    , envVars    :: IntMap Type
    , envNextVar :: Int
    }
    deriving (Show)

instance HasDiagnostics Env where
    addDiagnostic diag env@Env{envDiags} = env{envDiags = addDiagnostic diag envDiags}

instance Pretty Env where
    pretty env = vcat
        [ vcat
          . map (\(k, v) -> pretty k <+> colon <+> pretty (show v))
          $ resolved
        , vcat
          . map (\(k, v) -> pretty k <+> colon <+> pretty (show v))
          . IntMap.assocs
          . envVars
          $ env
        ]
      where
        resolved :: [(Text, Type)]
        resolved = flip State.evalState env $ do
            tys <- Map.assocs . envTypes <$> State.get
            mapM (\(n, ty) -> (n,) <$> unify ty ty) tys


typeError :: (HasCallStack, Show a) => String -> [a] -> State Env b
typeError msg x = do
    env <- State.get
    error $ msg <> ": " <> show (vcat $ map (pretty . show) x ++ [pretty env])

empty :: Env
empty = Env{..}
  where
    envDiags = []
    envLocals = []
    envTypes = Map.fromList
        [ ("nullptr"       , T_Ptr T_Top)
        , ("NULL"          , T_Ptr T_Top)
        -- TODO(iphydf): Can't deal with variadic functions yet.
        , ("LOGGER_ASSERT" , T_Func T_Void [T_Ptr (T_Name "Logger"), T_Bool, T_Ptr T_Char])
        , ("LOGGER_DEBUG"  , T_Func T_Void [T_Ptr (T_Name "Logger"), T_Ptr T_Char])
        , ("LOGGER_ERROR"  , T_Func T_Void [T_Ptr (T_Name "Logger"), T_Ptr T_Char])
        , ("LOGGER_FATAL"  , T_Func T_Void [T_Ptr (T_Name "Logger"), T_Ptr T_Char])
        , ("LOGGER_INFO"   , T_Func T_Void [T_Ptr (T_Name "Logger"), T_Ptr T_Char])
        , ("LOGGER_TRACE"  , T_Func T_Void [T_Ptr (T_Name "Logger"), T_Ptr T_Char])
        , ("LOGGER_WARNING", T_Func T_Void [T_Ptr (T_Name "Logger"), T_Ptr T_Char])
        , ("vpx_codec_control", T_Func T_Void [])
        , ("crypto_memzero", T_Func T_Void [T_Ptr T_Void, T_Int])
        , ("ioctl", T_Func T_Void [T_Int, T_Int, T_Ptr T_Void])
        , ("memset", T_Func T_Void [T_Ptr T_Void, T_Int, T_Int])
        , ("memcpy", T_Func T_Void [T_Ptr T_Void, T_Ptr T_Void, T_Int])
        , ("memmove", T_Func T_Void [T_Ptr T_Void, T_Ptr T_Void, T_Int])
        , ("snprintf", T_Func T_Void [])
        , ("strerror_r", T_Func T_Void [])
        ]
    envVars = IntMap.empty
    envNextVar = 0

newTyVar :: State Env Type
newTyVar = do
    env@Env{envNextVar} <- State.get
    State.put env{envNextVar = envNextVar + 1}
    return $ T_Var envNextVar

addTyVar :: Int -> Type -> State Env ()
addTyVar v ty =
    State.modify $ \env@Env{envVars} ->
        env{envVars = IntMap.insert v ty envVars}

addLocal :: Text -> State Env ()
addLocal n = State.modify $ \env@Env{envLocals} -> env{envLocals = n:envLocals}

dropLocals :: State Env ()
dropLocals = State.modify $ \env@Env{envLocals, envTypes} ->
    env{envTypes = foldr Map.delete envTypes envLocals, envLocals = []}

addName :: HasCallStack => Text -> Type -> State Env Type
addName n ty = do
    traceMaybe $ "a: " <> show n <> " = " <> show ty
    found <- Map.lookup n . envTypes <$> State.get
    case found of
      Nothing ->do
          State.modify $ \env@Env{envTypes} -> env{envTypes = Map.insert n ty envTypes}
          return ty
      Just ty' -> unify ty ty'

getName :: HasCallStack => Text -> State Env Type
getName n = do
    traceMaybe $ "g " <> show n
    found <- Map.lookup n . envTypes <$> State.get
    traceMaybe $ "g " <> show n <> " = " <> show found
    case found of
      Just ok -> return ok
      Nothing -> addName n =<< newTyVar

resolve :: Int -> State Env (Maybe Type)
resolve v = do
    traceMaybe $ "r " <> show v
    found <- IntMap.lookup v . envVars <$> State.get
    traceMaybe $ "r " <> show v <> " = " <> show found
    return found


data HasRecursed
    = NotRecursed
    | HasRecursed


unifyRecursive :: HasCallStack => [(Type, Type)] -> Type -> Type -> State Env Type
unifyRecursive stack ty1 ty2 | (ty1, ty2) `elem` stack =
    typeError "recursive unification" stack
unifyRecursive stack ty1 ty2 = do
    traceMaybe $ "unify: " <> show (ty1, ty2)
    res <- go NotRecursed ty1 ty2
    case res of
        T_Bot -> typeError "bottom" $ (ty1, ty2):stack
        ok    -> return ok
  where
    -- Equal types unify trivially.
    go _ a b | a == b = return a

    go _ (T_Struct a     ) (T_Struct b     ) = T_Struct   <$> unionWithM unifyRec a b
    go _ (T_InitList la  ) (T_InitList lb  ) = T_InitList <$> zipWithM unifyRec la lb
    go _ (T_Func ra argsa) (T_Func rb argsb) = T_Func     <$> unifyRec ra rb <*> zipWithM unifyRec argsa argsb
    go _ (T_Add la lb    ) (T_Add ra rb    ) = T_Add      <$> unifyRec la ra <*> unifyRec lb rb
    go _ (T_Sub la lb    ) (T_Sub ra rb    ) = T_Sub      <$> unifyRec la ra <*> unifyRec lb rb

    go _ (T_Intersect a1 a2) b = foldM unifyRec b [a1, a2]

    go _ (T_Name name) b = do
        r <- getName name
        traceMaybe $ "unify name: " <> show name <> " (= " <> show r <> ") with " <> show b
        unifyRec b r

    go _ (T_Var a) b = do
        res <- resolve a
        case res of
          Nothing -> do
              addTyVar a b
              return b
          Just resolved@T_Var{} ->
              return resolved
          Just resolved ->
              unifyRec b resolved

    go _ (T_Add l r) b@T_Ptr{} = do
        void $ unifyRec r T_Int
        unifyRec l b
    go _ (T_Add l r) b@T_Arr{} = do
        void $ unifyRec r T_Int
        unifyRec l b

    go _ (T_Add l r) T_Int = unifyRec l T_Int >>= unifyRec r
    go _ a@T_Add{} b@T_Sub{} = return $ T_Intersect a b

    go _ (T_Sub (T_Ptr l) (T_Ptr r)) b = unifyRec l r >> unifyRec b T_Int
    go _ (T_Sub (T_Ptr l) (T_Arr r)) b = unifyRec l r >> unifyRec b T_Int
    go _ (T_Sub l T_Int) r@T_Ptr{} = unifyRec l r
    go _ (T_Sub l r) T_Int = unifyRec l T_Int >>= unifyRec r

    -- Dereference function pointers for unification.
    go _ a@T_Func{} (T_Ptr b) = unifyRec a b

    -- Array and pointer types can unify and turn into pointer.
    go _ (T_Arr a) (T_Ptr b) = T_Ptr <$> unifyRec a b

    go _ a@T_Struct{} T_InitList{} = return a
    go _ a@T_Struct{} T_Name{} = return a

    -- Arrays unify with all elements in their initialiser list.
    go _ (T_Arr a) (T_InitList b) = foldM unifyRec a b

    -- `void *` unifies with any pointer type.
    go _ (T_Ptr T_Void) b@T_Ptr{} = return b
    -- Incompatible pointers unify to `void *`.
    go _ T_Ptr{} T_Ptr{} = return $ T_Ptr T_Void

    go _ T_Int T_Ptr{} = return T_Bot

    -- `void` and the top type (null) unifies with anything
    go _ T_Void b = return b
    go _ T_Top b = return b
    -- The bottom type turns everything into bottom.
    go _ T_Bot _ = return T_Bot

    go NotRecursed a b = go HasRecursed b a
    go HasRecursed a b = typeError "unification" [(a, b)]

    unifyRec = unifyRecursive ((ty1, ty2):stack)


unify :: HasCallStack => Type -> Type -> State Env Type
unify = unifyRecursive []


inferBinaryExpr :: BinaryOp -> Type -> Type -> State Env Type
inferBinaryExpr BopAnd l r    = foldM unify T_Bool [l, r]
inferBinaryExpr BopOr l r     = foldM unify T_Bool [l, r]
inferBinaryExpr BopLe l r     = T_Bool <$ unify l r
inferBinaryExpr BopLt l r     = T_Bool <$ unify l r
inferBinaryExpr BopGe l r     = T_Bool <$ unify l r
inferBinaryExpr BopGt l r     = T_Bool <$ unify l r
inferBinaryExpr BopEq l r     = T_Bool <$ unify l r
inferBinaryExpr BopNe l r     = T_Bool <$ unify l r
inferBinaryExpr BopMul l r    = unify l r
inferBinaryExpr BopMod l r    = unify l r
inferBinaryExpr BopDiv l r    = unify l r
inferBinaryExpr BopBitOr l r  = unify l r
inferBinaryExpr BopBitAnd l r = unify l r
inferBinaryExpr BopBitXor l r = unify l r
inferBinaryExpr BopLsh l r    = unify l r
inferBinaryExpr BopRsh l r    = unify l r
inferBinaryExpr BopPlus l r   = return $ T_Add l r
inferBinaryExpr BopMinus l r  = return $ T_Sub l r


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
          Nothing -> return $ T_Name name -- typeError "standard type" [name]
          Just ty -> return ty
    TyUserDefined (L _ _ name) -> return $ T_Name name
    TyPointer ty -> return $ T_Ptr ty
    TyBitwise ty -> return ty
    TyForce ty -> return ty
    TyConst ty -> return ty
    TyOwner ty -> return ty
    TyNonnull ty -> return ty
    TyNullable ty -> return ty
    Ellipsis -> return T_Void

    VLA ty (L _ _ name) size -> do
        void $ unify T_Int size
        addLocal name
        void $ addName name $ T_Arr ty
        return T_Void
    DeclSpecArray{} -> return T_ArrDim
    VarDecl ty (L _ _ name) arrs -> do
        let ty' = foldr (const T_Arr) ty arrs
        addLocal name
        addName name ty'
    VarDeclStmt _ Nothing -> return T_Void
    VarDeclStmt decl (Just initExpr) -> do
        void $ unify decl initExpr
        return T_Void
    PreprocDefineConst (L _ _ name) ty -> do
        void $ addName name ty
        return T_Void

    VarExpr (L _ _ name) -> do
        traceMaybe $ "infer var " <> show name
        getName name
    LiteralExpr ConstId (L _ _ name) -> do
        traceMaybe $ "infer const " <> show name
        getName name
    CastExpr ty _ -> return ty
    CompoundLiteral ty _ -> return ty
    DoWhileStmt body c -> body <$ unify c T_Bool
    WhileStmt c body -> body <$ unify c T_Bool
    ForStmt _ c _ body -> body <$ unify c T_Bool
    SwitchStmt _ body -> foldM unify T_Void body
    IfStmt c t (Just e) -> do
        void $ unify c T_Bool
        unify t e
    IfStmt c t Nothing -> t <$ unify c T_Bool
    CompoundStmt body -> foldM unify T_Void body
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
    NonNullParam e -> return e
    NullableParam e -> return e

    FunctionPrototype retTy (L _ _ name) args -> do
        -- traceMaybe $ "f " <> show f
        addName name $ T_Func retTy args
    FunctionCall callee args -> do
        retTy <- newTyVar
        -- traceMaybe ">>>>"
        -- traceMaybe $ show (T_Func retTy args)
        -- traceMaybe $ show callee
        -- traceMaybe "<<<<"
        funTy <- unify (T_Func retTy args) callee
        case funTy of
          T_Func result _ -> return result
          _               -> typeError "function type" [funTy]
    FunctionDefn _ defnTy body -> do
        dropLocals
        retTy <- newTyVar
        -- Ignore parameter types, unify will zip [] with the params, resulting in [].
        funTy <- unify (T_Func retTy []) defnTy
        case funTy of
          T_Func r _ -> unify r body
          _          -> typeError "function defn" [funTy]

    ExprStmt{} -> return T_Void
    Break -> return T_Void
    Continue -> return T_Void
    Goto{} -> return T_Void
    Default e -> return e
    Case _ e -> return e
    Label _ e -> return e
    Comment{} -> return T_Void
    CommentExpr _ e -> return e

    PreprocDefineMacro (L _ _ name) _ _ -> do
        addLocal name
        return T_Void
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

    MemberAccess e (L _ _ mem) -> do
        ty <- newTyVar
        void $ unify e (T_Struct (Map.singleton mem ty))
        return ty
    PointerAccess e (L _ _ mem) -> do
        ty <- newTyVar
        void $ unify e (T_Ptr (T_Struct (Map.singleton mem ty)))
        return ty

    x -> typeError "unhandled" [x]

linter :: AstActions (State Env) Text
linter = astActions
    { doNode = \_file node act ->
        case unFix node of
            FunctionDefn{} -> void $ foldFixM inferTypes node

            _              -> act
    }

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse = reverse . envDiags . flip State.execState empty . traverseAst linter

descr :: ([(FilePath, [Node (Lexeme Text)])] -> [Text], (Text, Text))
descr = (analyse, ("type-check", Text.unlines
    [ "Performs Hindley-Milner like type checking."
    , ""
    , "This is very much work in progress, so it may fail in cryptic ways. Talk to"
    , "@iphydf if it produces an error."
    , ""
    , "**Reason:** this allows us to validate various difficult to check aspects of C."
    ]))
