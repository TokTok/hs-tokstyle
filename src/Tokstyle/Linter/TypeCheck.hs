{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}

module Tokstyle.Linter.TypeCheck (descr) where

import           Control.Monad               (forM, forM_, void, when, zipWithM,
                                              zipWithM_)
import           Control.Monad.Except        hiding (catchError)
import           Control.Monad.State.Strict  (State, StateT, evalStateT, get,
                                              gets, lift, modify, put, runState)
import           Data.Fix                    (Fix (..))
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (mapMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (AssignOp (..), BinaryOp (..),
                                              HasLocation, Lexeme (..),
                                              LiteralType (..), Node,
                                              NodeF (..), UnaryOp (..),
                                              lexemeText)
import qualified Language.Cimple             as C
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)

-- | The core type system of our linter. It's a Hindley-Milner style type system.
data Type
    = TInt
    | TFloat
    | TBool
    | TChar
    | TString
    | TUnit -- void
    | TPointer Type
    | TFunc [Type] Type Bool
    | TStruct Text (Map Text Type)
    | TUnion Text (Map Text Type)
    | TUserDefined Text
    | TNullPtr
    | TVar TVar
    deriving (Ord)

-- Custom Eq for nominal typing of structs
instance Eq Type where
    TInt == TInt = True
    TFloat == TFloat = True
    TBool == TBool = True
    TChar == TChar = True
    TString == TString = True
    TUnit == TUnit = True
    (TPointer t1) == (TPointer t2) = t1 == t2
    (TFunc args1 ret1 v1) == (TFunc args2 ret2 v2) = args1 == args2 && ret1 == ret2 && v1 == v2
    (TStruct name1 _) == (TStruct name2 _) = name1 == name2
    (TUnion name1 _) == (TUnion name2 _) = name1 == name2
    (TUserDefined name1) == (TUserDefined name2) = name1 == name2
    TNullPtr == TNullPtr = True
    (TVar v1) == (TVar v2) = v1 == v2
    _ == _ = False

-- | A type variable.
newtype TVar = TV Int deriving (Eq, Ord)

instance Show TVar where
    show (TV i) = "t" ++ show i

instance Show Type where
    show TInt               = "int"
    show TFloat             = "float"
    show TBool              = "bool"
    show TChar              = "char"
    show TString            = "string"
    show TUnit              = "void"
    show (TPointer t)       = show t ++ "*"
    show (TFunc args ret isVariadic)   = "(" ++ Text.unpack (Text.intercalate ", " (map (Text.pack . show) args)) ++ (if isVariadic then ", ..." else "") ++ ") -> " ++ show ret
    show (TStruct name _)   = "struct " ++ Text.unpack name
    show (TUnion name _)    = "union " ++ Text.unpack name
    show (TUserDefined name)= Text.unpack name
    show TNullPtr           = "nullptr_t"
    show (TVar v)           = show v

-- | A type scheme, used for polymorphism.
data Scheme = Forall [TVar] Type
    deriving (Eq, Ord, Show)

-- Substitution

-- | A substitution is a mapping from type variables to types.
type Subst = Map TVar Type

-- | A class for types that can have substitutions applied to them.
class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set TVar

instance Substitutable Type where
    apply s t@(TVar a)            = Map.findWithDefault t a s
    apply s (TPointer t)          = TPointer (apply s t)
    apply s (TFunc args ret v)    = TFunc (map (apply s) args) (apply s ret) v
    apply s (TStruct name fields) = TStruct name (Map.map (apply s) fields)
    apply s (TUnion name fields)  = TUnion name (Map.map (apply s) fields)
    apply _ t                     = t

    ftv (TVar a)           = Set.singleton a
    ftv (TPointer t)       = ftv t
    ftv (TFunc args ret _) = Set.unions (map ftv args) `Set.union` ftv ret
    ftv (TStruct _ fields) = ftv (Map.elems fields)
    ftv (TUnion _ fields)  = ftv (Map.elems fields)
    ftv _                  = Set.empty

instance Substitutable Scheme where
    apply s (Forall vars t) = Forall vars (apply (foldr Map.delete s vars) t)
    ftv (Forall vars t)     = ftv t `Set.difference` Set.fromList vars

instance Substitutable a => Substitutable [a] where
    apply = map . apply
    ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable v => Substitutable (Map k v) where
    apply s = Map.map (apply s)
    ftv = ftv . Map.elems

-- Environments

-- | The type environment maps variable names to their type schemes.
type TypeEnv = Map Text Scheme
-- | The struct environment maps struct names to their field definitions.
type StructEnv = Map Text (Map Text Type)
-- | The union environment maps union names to their field definitions.
type UnionEnv = Map Text (Map Text Type)
-- | The typedef environment maps typedef names to their underlying types.
type TypedefEnv = Map Text Type

-- Linter State

-- | The state of the type checker.
data LinterState = LinterState
    { typeEnv      :: TypeEnv
    , structEnv    :: StructEnv
    , unionEnv     :: UnionEnv
    , typedefEnv   :: TypedefEnv
    , subst        :: Subst
    , freshCounter :: Int
    , currentFile  :: FilePath
    }

-- | The linter monad.
type LinterM a = StateT LinterState (State [Text]) a

-- | A local error handler.
catchError :: LinterM a -> (Text -> LinterM a) -> LinterM a
catchError action handler = do
    st <- get
    let res = runState (evalStateT action st) []
    case res of
        (val, []) -> return val
        (_, errs) -> handler (Text.unlines errs)


-- Monad Helpers

-- | Generate a fresh type variable.
fresh :: LinterM Type
fresh = do
    s <- get
    let i = freshCounter s
    put s { freshCounter = i + 1 }
    return $ TVar (TV i)

-- | Add a type error to the list of diagnostics.
addError :: HasLocation a => a -> Text -> LinterM ()
addError loc msg = do
    file <- gets currentFile
    lift $ warn file loc msg

-- | Run a computation in a modified type environment.
withEnv :: (TypeEnv -> TypeEnv) -> LinterM a -> LinterM a
withEnv f m = do
    oldEnv <- gets typeEnv
    modify $ \s -> s { typeEnv = f oldEnv }
    val <- m
    modify $ \s -> s { typeEnv = oldEnv }
    pure val

-- Unification

-- | Compose two substitutions.
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- | Unify two types.
unify :: HasLocation a => a -> Text -> Type -> Type -> LinterM ()
unify loc context t1 t2 = do
    s <- gets subst
    rT1 <- resolveType (apply s t1)
    rT2 <- resolveType (apply s t2)
    case (rT1, rT2) of
        (TPointer (TVar a), TPointer t) -> unifyVar loc context a t
        (TPointer t, TPointer (TVar a)) -> unifyVar loc context a t
        (TVar a, t) -> unifyVar loc context a t
        (t, TVar a) -> unifyVar loc context a t
        (TNullPtr, TPointer _) -> return ()
        (TPointer _, TNullPtr) -> return ()
        (TPointer t, t') | t == t' -> return ()
        (t, TPointer t') | t == t' -> return ()
        (TFunc args1 ret1 _, TPointer (TFunc args2 ret2 _)) -> unify loc context (TFunc args1 ret1 False) (TFunc args2 ret2 False)
        (TPointer (TFunc args1 ret1 _), TFunc args2 ret2 _) -> unify loc context (TFunc args1 ret1 False) (TFunc args2 ret2 False)
        (TFunc args1 ret1 v1, TFunc args2 ret2 _v2) -> do
            if v1
            then do
                when (length args1 > length args2) $
                    addError loc ("mismatched number of arguments in " <> context <>
                                   ": expected at least " <> Text.pack (show (length args1)) <>
                                   ", but got " <> Text.pack (show (length args2)))
                let (args2_fixed, _args2_variadic) = splitAt (length args1) args2
                zipWithM_ (\(t1', i) t2' -> unify loc (context <> " (argument " <> Text.pack (show (i :: Int)) <> ")") t1' t2') (zip args1 [1..]) args2_fixed
            else do
                when (length args1 /= length args2) $
                    addError loc ("mismatched number of arguments in " <> context <>
                                   ": expected " <> Text.pack (show (length args1)) <>
                                   ", but got " <> Text.pack (show (length args2)))
                zipWithM_ (\(t1', i) t2' -> unify loc (context <> " (argument " <> Text.pack (show (i :: Int)) <> ")") t1' t2') (zip args1 [1..]) args2
            unify loc (context <> " (return type)") ret1 ret2
        (TUnion n1 _, TUnion n2 _) | n1 == n2 -> return ()
        (t1'', t2'') | t1'' == t2'' -> return ()
        _ -> addError loc $ "type mismatch in " <> context <> ": expected " <> Text.pack (show rT1) <> ", but got " <> Text.pack (show rT2)

-- | Unify a type variable with a type.
unifyVar :: HasLocation a => a -> Text -> TVar -> Type -> LinterM ()
unifyVar loc context a t = do
    s <- gets subst
    if | Just t' <- Map.lookup a s -> unify loc context t' t
       | TVar a == t -> return ()
       | a `Set.member` ftv t -> addError loc $ "occurs check fails in " <> context <> ": " <> Text.pack (show a) <> " vs " <> Text.pack (show t)
       | otherwise -> do
            let newSubst = Map.singleton a t
            modify $ \st -> st { subst = composeSubst newSubst (subst st) }

-- Type Inference

-- | Generalize a type into a type scheme.
generalize :: TypeEnv -> Type -> LinterM Scheme
generalize env t = do
    s <- gets subst
    let t' = apply s t
    let env' = apply s env
    return $ Forall (Set.toList $ ftv t' `Set.difference` ftv env') t'

-- | Instantiate a type scheme into a type.
instantiate :: Scheme -> LinterM Type
instantiate (Forall vars t) = do
    s <- gets subst
    freshSubst <- fmap Map.fromList $ forM vars $ \v -> do
        v' <- fresh
        return (v, v')
    return $ apply (composeSubst freshSubst s) t

-- | Convert a Cimple AST type to a linter type.
cimpleToType :: Node (Lexeme Text) -> LinterM Type
cimpleToType (Fix node) = case node of
    C.TyStd (L _ _ "void")   -> return TUnit
    C.TyStd (L _ _ "float")  -> return TFloat
    C.TyStd (L _ _ "int")    -> return TInt
    C.TyStd (L _ _ "long")   -> return TInt
    C.TyStd (L _ _ "long int") -> return TInt
    C.TyStd (L _ _ "long signed int") -> return TInt
    C.TyStd (L _ _ "signed int") -> return TInt
    C.TyStd (L _ _ "unsigned") -> return TInt
    C.TyStd (L _ _ "unsigned int") -> return TInt
    C.TyStd (L _ _ "unsigned long") -> return TInt
    C.TyStd (L _ _ "unsigned long long") -> return TInt
    C.TyStd (L _ _ "uint8_t") -> return TInt
    C.TyStd (L _ _ "uint16_t") -> return TInt
    C.TyStd (L _ _ "uint32_t") -> return TInt
    C.TyStd (L _ _ "uint64_t") -> return TInt
    C.TyStd (L _ _ "int8_t") -> return TInt
    C.TyStd (L _ _ "int16_t") -> return TInt
    C.TyStd (L _ _ "int32_t") -> return TInt
    C.TyStd (L _ _ "int64_t") -> return TInt
    C.TyStd (L _ _ "size_t") -> return TInt
    C.TyStd (L _ _ "char")   -> return TChar
    C.TyStd (L _ _ "bool")   -> return TBool
    C.TyPointer (Fix (C.TyStd (L _ _ "void"))) -> TPointer <$> fresh
    C.TyPointer t            -> TPointer <$> cimpleToType t
    C.TyConst t              -> cimpleToType t
    C.TyOwner t              -> cimpleToType t
    C.TyNullable t           -> cimpleToType t
    C.TyNonnull t            -> cimpleToType t
    C.TyUserDefined (L _ _ name) -> return $ TUserDefined name
    C.TyStruct (L _ _ name)  -> do
        senv <- gets structEnv
        return $ TStruct name (Map.findWithDefault Map.empty name senv)
    C.TyUnion (L _ _ name) -> do
        uenv <- gets unionEnv
        return $ TUnion name (Map.findWithDefault Map.empty name uenv)
    C.TyFunc (L _ _ name)    -> return $ TUserDefined name
    _                        -> fresh

-- | Resolve a type by looking up typedefs.
resolveType :: Type -> LinterM Type
resolveType (TUserDefined name) = do
    tenv <- gets typedefEnv
    case Map.lookup name tenv of
        Just t  -> resolveType t
        Nothing -> do
            senv <- gets structEnv
            case Map.lookup name senv of
                Just fields -> return $ TStruct name fields
                Nothing     -> do
                    uenv <- gets unionEnv
                    case Map.lookup name uenv of
                        Just fields -> return $ TUnion name fields
                        Nothing     -> return $ TUserDefined name
resolveType (TStruct name _) = do
    senv <- gets structEnv
    case Map.lookup name senv of
        Just fields -> return $ TStruct name fields
        Nothing     -> return $ TStruct name Map.empty
resolveType (TUnion name _) = do
    uenv <- gets unionEnv
    case Map.lookup name uenv of
        Just fields -> return $ TUnion name fields
        Nothing     -> return $ TUnion name Map.empty
resolveType (TPointer t) = TPointer <$> resolveType t
resolveType (TFunc args ret v) = TFunc <$> mapM resolveType args <*> resolveType ret <*> pure v
resolveType t = return t

-- | Infer the type of an expression.
inferExpr :: Node (Lexeme Text) -> LinterM Type
inferExpr n@(Fix node) = case node of
    C.MacroBodyStmt e -> inferExpr e
    C.MacroBodyFunCall e -> inferExpr e
    C.CompoundStmt stmts -> do
        retType <- fresh
        checkStmts stmts retType
        return TUnit
    C.DoWhileStmt body cond -> do
        retType <- fresh
        checkStmt body retType
        checkCond cond
        return TUnit
    C.InitialiserList es -> do
        elemType <- fresh
        forM_ es $ \e -> do
            t <- inferExpr e
            unify e "initialiser list element" elemType t
        return $ TPointer elemType
    C.CommentExpr e _ -> inferExpr e
    C.CompoundExpr _ _ -> fresh
    C.MacroParam l@(L _ _ name) -> do
        mScheme <- gets (Map.lookup name . typeEnv)
        case mScheme of
            Just scheme -> instantiate scheme
            Nothing     -> addError l ("Unbound macro parameter: " <> name) >> fresh
    C.LiteralExpr C.Int (L _ _ txt)
        | "." `Text.isInfixOf` txt -> return TFloat
        | otherwise -> return TInt
    C.LiteralExpr C.Bool _     -> return TBool
    C.LiteralExpr C.Char _     -> return TChar
    C.LiteralExpr C.String _   -> return $ TPointer TChar
    C.LiteralExpr C.ConstId l@(L _ _ name)
        | name == "NULL" || name == "nullptr" -> return TNullPtr
        | otherwise -> do
            mScheme <- gets (Map.lookup name . typeEnv)
            case mScheme of
                Just scheme -> instantiate scheme
                Nothing     -> addError l ("Unbound constant: " <> name) >> fresh
    C.VarExpr l@(L _ _ name) -> do
        mScheme <- gets (Map.lookup name . typeEnv)
        case mScheme of
            Just scheme -> instantiate scheme
            Nothing     -> addError l ("Unbound variable: " <> name) >> fresh
    C.ParenExpr e -> inferExpr e
    C.UnaryExpr op e -> inferUnary op e
    C.BinaryExpr e1 op e2 -> inferBinary e1 op e2
    C.AssignExpr lhs op rhs -> do
        t1 <- inferExpr lhs
        t2 <- inferExpr rhs
        case (op, t1, t2) of
            (AopEq, _, _) -> unify n "assignment" t1 t2
            (AopPlus, TPointer _, TInt) -> return ()
            (AopMinus, TPointer _, TInt) -> return ()
            (AopBitXor, TInt, TInt) -> return ()
            (_, TInt, TInt) -> return ()
            _ -> addError n $ "invalid assignment operation: " <> Text.pack (show op) <> " with types " <> Text.pack (show t1) <> " and " <> Text.pack (show t2)
        return t1
    C.TernaryExpr c t e -> do
        checkCond c
        t1 <- inferExpr t
        t2 <- inferExpr e
        unify n "ternary expression" t1 t2
        return t1
    C.FunctionCall f args -> do
        fType <- inferExpr f
        argTypes <- mapM inferExpr args
        retType <- fresh
        unify f "function call" fType (TFunc argTypes retType False)
        return retType
    C.PointerAccess e l@(L _ _ member) -> do
        eType <- inferExpr e
        rType <- resolveType eType
        case rType of
            TPointer (TStruct name fields) ->
                case Map.lookup member fields of
                    Just memberType -> return memberType
                    Nothing -> addError l ("Struct " <> name <> " has no member: " <> member) >> fresh
            TPointer (TUnion name fields) ->
                case Map.lookup member fields of
                    Just memberType -> return memberType
                    Nothing -> addError l ("Union " <> name <> " has no member: " <> member) >> fresh
            TPointer (TUserDefined name) -> do
                senv <- gets structEnv
                case Map.lookup name senv of
                    Just fields -> case Map.lookup member fields of
                        Just memberType -> return memberType
                        Nothing -> addError l ("Struct " <> name <> " has no member: " <> member) >> fresh
                    Nothing -> do
                        uenv <- gets unionEnv
                        case Map.lookup name uenv of
                            Just ufields -> case Map.lookup member ufields of
                                Just memberType -> return memberType
                                Nothing -> addError l ("Union " <> name <> " has no member: " <> member) >> fresh
                            Nothing -> addError e ("Accessing member of incomplete type: " <> name) >> fresh
            TPointer (TVar _) -> do
                memberType <- fresh
                let fields = Map.singleton member memberType
                unify e "pointer access" (TPointer (TStruct "unknown" fields)) eType
                return memberType
            _ -> addError e ("Pointer access on non-pointer-to-struct type: " <> Text.pack (show rType)) >> fresh
    C.MemberAccess e l@(L _ _ member) -> do
        eType <- inferExpr e
        rType <- resolveType eType
        case rType of
            TStruct name fields ->
                case Map.lookup member fields of
                    Just memberType -> return memberType
                    Nothing -> addError l ("Struct " <> name <> " has no member: " <> member) >> fresh
            TUnion name fields ->
                case Map.lookup member fields of
                    Just memberType -> return memberType
                    Nothing -> addError l ("Union " <> name <> " has no member: " <> member) >> fresh
            TUserDefined name -> do
                senv <- gets structEnv
                case Map.lookup name senv of
                    Just fields -> case Map.lookup member fields of
                        Just memberType -> return memberType
                        Nothing -> addError l ("Struct " <> name <> " has no member: " <> member) >> fresh
                    Nothing -> do
                        uenv <- gets unionEnv
                        case Map.lookup name uenv of
                            Just ufields -> case Map.lookup member ufields of
                                Just memberType -> return memberType
                                Nothing -> addError l ("Union " <> name <> " has no member: " <> member) >> fresh
                            Nothing -> addError e ("Accessing member of incomplete type: " <> name) >> fresh
            _ -> addError e ("Member access on non-struct type: " <> Text.pack (show rType)) >> fresh
    C.ArrayAccess e i -> do
        eType <- inferExpr e
        iType <- inferExpr i
        unify i "array index" TInt iType
        case eType of
            TPointer elemType -> return elemType
            _ -> do
                elemType <- fresh
                unify e "array access" (TPointer elemType) eType
                return elemType
    C.CastExpr ty e -> do
        t <- cimpleToType ty
        e_t <- inferExpr e
        unify e "cast expression" t e_t
        return t
    C.SizeofExpr _ -> return TInt
    C.SizeofType _ -> return TInt
    C.CompoundLiteral ty _ -> cimpleToType ty
    _ -> addError n ("unhandled expression: " <> Text.pack (show node)) >> fresh

-- | Infer the type of a unary expression.
inferUnary :: UnaryOp -> Node (Lexeme Text) -> LinterM Type
inferUnary op e = do
    t <- inferExpr e
    case op of
        UopNot     -> checkCond e >> return TBool
        UopNeg     -> unify e "negation" TInt t >> return TInt
        UopMinus   -> unify e "unary minus" TInt t >> return TInt
        UopAddress -> TPointer <$> inferExpr e
        UopDeref   -> do
            case t of
                TPointer t' -> return t'
                _           -> do
                    ptrType <- fresh
                    unify e "dereference" (TPointer ptrType) t
                    return ptrType
        UopIncr    -> unify e "increment" TInt t >> return TInt
        UopDecr    -> unify e "decrement" TInt t >> return TInt

-- | Infer the type of a binary expression.
inferBinary :: Node (Lexeme Text) -> BinaryOp -> Node (Lexeme Text) -> LinterM Type
inferBinary e1 op e2 = do
    t1 <- inferExpr e1
    t2 <- inferExpr e2
    let opCtx = "binary operator " <> Text.pack (show op)
    case op of
        BopPlus -> case (t1, t2) of
            (TPointer pt, TInt) -> return $ TPointer pt
            (TInt, TPointer pt) -> return $ TPointer pt
            (TInt, TInt) -> return TInt
            (TFloat, TFloat) -> return TFloat
            (TInt, TFloat) -> return TFloat
            (TFloat, TInt) -> return TFloat
            v -> addError e1 ("invalid operands for +: " <> Text.pack (show v)) >> return TInt
        BopMinus -> case (t1, t2) of
            (TPointer pt, TInt) -> return $ TPointer pt
            (TPointer pt1, TPointer pt2) -> unify e1 opCtx pt1 pt2 >> return TInt
            (TInt, TInt) -> return TInt
            (TFloat, TFloat) -> return TFloat
            (TInt, TFloat) -> return TFloat
            (TFloat, TInt) -> return TFloat
            v -> addError e1 ("invalid operands for -: " <> Text.pack (show v)) >> return TInt
        BopMul -> do
           unify e1 opCtx TInt t1
           unify e2 opCtx TInt t2
           return TInt
        BopDiv -> do
           unify e1 opCtx TInt t1
           unify e2 opCtx TInt t2
           return TInt
        BopMod -> do
           unify e1 opCtx TInt t1
           unify e2 opCtx TInt t2
           return TInt
        BopBitAnd -> do
            unify e1 opCtx TInt t1
            unify e2 opCtx TInt t2
            return TInt
        BopBitOr -> do
            unify e1 opCtx TInt t1
            unify e2 opCtx TInt t2
            return TInt
        BopBitXor -> do
            unify e1 opCtx TInt t1
            unify e2 opCtx TInt t2
            return TInt
        BopLsh -> do
            unify e1 opCtx TInt t1
            unify e2 opCtx TInt t2
            return TInt
        BopRsh -> do
            unify e1 opCtx TInt t1
            unify e2 opCtx TInt t2
            return TInt
        BopEq -> do
           unify e1 opCtx t1 t2
           return TBool
        BopNe -> do
           unify e1 opCtx t1 t2
           return TBool
        BopLt -> do
           unify e1 opCtx t1 t2
           return TBool
        BopLe -> do
           unify e1 opCtx t1 t2
           return TBool
        BopGt -> do
           unify e1 opCtx t1 t2
           return TBool
        BopGe -> do
           unify e1 opCtx t1 t2
           return TBool
        BopAnd -> do
           checkCond e1
           checkCond e2
           return TBool
        BopOr -> do
           checkCond e1
           checkCond e2
           return TBool

-- | Check if an expression is a valid condition.
checkCond :: Node (Lexeme Text) -> LinterM ()
checkCond cond = do
    condType <- inferExpr cond
    s <- gets subst
    let condType' = apply s condType
    case condType' of
        TBool      -> return ()
        TInt       -> return ()
        TPointer _ -> return ()
        _          -> unify cond "condition" TBool condType'

-- | Type check a list of statements.
checkStmts :: [Node (Lexeme Text)] -> Type -> LinterM ()
checkStmts [] _ = return ()
checkStmts ((Fix (C.VarDeclStmt (Fix (C.VarDecl ty (L _ _ name) declSpecArrays)) mInit)):stmts) expectedRetType = do
    varType <- cimpleToType ty
    let finalVarType = foldr (\_ acc -> TPointer acc) varType declSpecArrays
    case mInit of
        Just initExpr@(Fix (C.InitialiserList es)) -> do
            rVarType <- resolveType finalVarType
            case rVarType of
                TStruct _ fields -> do
                    let fieldTypes = Map.elems fields
                    when (length fieldTypes == length es) $
                        zipWithM_ (\fieldType e -> do
                            eType <- inferExpr e
                            unify e ("initialisation of " <> name) fieldType eType
                        ) fieldTypes es
                _ -> do
                    initType <- inferExpr initExpr
                    unify initExpr ("initialisation of " <> name) finalVarType initType
        _ -> forM_ mInit $ \initExpr -> do
            initType <- inferExpr initExpr
            unify initExpr ("initialisation of " <> name) finalVarType initType

    let scheme = Forall [] finalVarType
    withEnv (Map.insert name scheme) $ checkStmts stmts expectedRetType
checkStmts ((Fix (C.VLA ty (L _ _ name) sizeExpr)):stmts) expectedRetType = do
    baseType <- cimpleToType ty
    let varType = TPointer baseType
    sizeType <- inferExpr sizeExpr
    unify sizeExpr "VLA size" TInt sizeType
    let scheme = Forall [] varType
    withEnv (Map.insert name scheme) $ checkStmts stmts expectedRetType
checkStmts (stmt:stmts) expectedRetType = do
    checkStmt stmt expectedRetType >> checkStmts stmts expectedRetType

-- | Type check a single statement.
checkStmt :: Node (Lexeme Text) -> Type -> LinterM ()
checkStmt n@(Fix node) expectedRetType = case node of
    C.CompoundStmt stmts -> checkStmts stmts expectedRetType
    C.Return (Just expr) -> do
        actualRetType <- inferExpr expr
        unify expr "return value" expectedRetType actualRetType
    C.Return Nothing -> unify n "return from void function" expectedRetType TUnit
    C.IfStmt cond thenB mElseB -> do
        checkCond cond
        checkStmt thenB expectedRetType
        maybe (return ()) (`checkStmt` expectedRetType) mElseB
    C.WhileStmt cond body -> do
        checkCond cond
        checkStmt body expectedRetType
    C.ForStmt init' cond next body -> do
        withEnv id $ do
            case init' of
                Fix (C.VarDeclStmt (Fix (C.VarDecl ty (L _ _ name) _)) mInit) -> do
                    varType <- cimpleToType ty
                    forM_ mInit $ \initExpr -> do
                        initType <- inferExpr initExpr
                        unify initExpr ("for loop initialisation of " <> name) varType initType
                    env <- gets typeEnv
                    scheme <- generalize env varType
                    withEnv (Map.insert name scheme) $ do
                        checkCond cond
                        void $ inferExpr next
                        checkStmt body expectedRetType
                _ -> do
                    void $ inferExpr init'
                    checkCond cond
                    void $ inferExpr next
                    checkStmt body expectedRetType
    C.DoWhileStmt body cond -> do
        checkStmt body expectedRetType
        checkCond cond
    C.SwitchStmt expr cases -> do
        exprType <- inferExpr expr
        unify expr "switch expression" TInt exprType `catchError` \_ -> unify expr "switch expression" exprType exprType
        forM_ cases $ \case
            Fix (C.Case caseExpr body) -> do
                caseExprType <- inferExpr caseExpr
                unify caseExpr "case expression" TInt caseExprType `catchError` \_ -> unify caseExpr "case expression" exprType caseExprType
                checkStmt body expectedRetType
            Fix (C.Default body) -> checkStmt body expectedRetType
            _ -> return ()
    C.ExprStmt e -> void $ inferExpr e
    _ -> return ()

-- Analysis Passes

type TranslationUnit = (FilePath, [Node (Lexeme Text)])

-- | Collect all global definitions (structs, typedefs, functions) in a file.
collectGlobals :: TranslationUnit -> LinterM ()
collectGlobals (file, nodes) = do
    modify $ \s -> s { currentFile = file }
    traverseAst collector (file, nodes)
  where
    handleEnumerator :: Type -> Node (Lexeme Text) -> LinterM ()
    handleEnumerator enumType (Fix (C.Enumerator (L _ _ ename) _)) = do
        let scheme = Forall [] enumType
        modify $ \s -> s { typeEnv = Map.insert ename scheme (typeEnv s) }
    handleEnumerator enumType (Fix (C.Commented _ n)) = handleEnumerator enumType n
    handleEnumerator _ _ = return ()

    collector = astActions
        { doNode = \_ node continuation -> do
            case unFix node of
                C.AggregateDecl (Fix (C.Struct (L _ _ sname) members)) -> do
                    memberDecls <- fmap concat . forM members $ \case
                        Fix (C.MemberDecl (Fix (C.VarDecl ty (L _ _ mname) declSpecArrays)) _) -> do
                            t <- cimpleToType ty
                            let finalT = foldr (\_ acc -> TPointer acc) t declSpecArrays
                            return [(mname, finalT)]
                        _ -> return []
                    let fieldEnv = Map.fromList memberDecls
                    modify $ \s -> s { structEnv = Map.insert sname fieldEnv (structEnv s) }
                C.AggregateDecl (Fix (C.Union (L _ _ uname) members)) -> do
                    memberDecls <- fmap concat . forM members $ \case
                        Fix (C.MemberDecl (Fix (C.VarDecl ty (L _ _ mname) declSpecArrays)) _) -> do
                            t <- cimpleToType ty
                            let finalT = foldr (\_ acc -> TPointer acc) t declSpecArrays
                            return [(mname, finalT)]
                        _ -> return []
                    let fieldEnv = Map.fromList memberDecls
                    modify $ \s -> s { unionEnv = Map.insert uname fieldEnv (unionEnv s) }
                C.Typedef (Fix (C.Struct (L _ _ sname) members)) (L _ _ tname) -> do
                    memberDecls <- fmap concat . forM members $ \case
                        Fix (C.MemberDecl (Fix (C.VarDecl ty (L _ _ mname) declSpecArrays)) _) -> do
                            t <- cimpleToType ty
                            let finalT = foldr (\_ acc -> TPointer acc) t declSpecArrays
                            return [(mname, finalT)]
                        _ -> return []
                    let fieldEnv = Map.fromList memberDecls
                    modify $ \s -> s { structEnv = Map.insert sname fieldEnv (structEnv s) }
                    modify $ \s -> s { typedefEnv = Map.insert tname (TStruct sname fieldEnv) (typedefEnv s) }
                C.Typedef (Fix (C.Union (L _ _ uname) members)) (L _ _ tname) -> do
                    memberDecls <- fmap concat . forM members $ \case
                        Fix (C.MemberDecl (Fix (C.VarDecl ty (L _ _ mname) declSpecArrays)) _) -> do
                            t <- cimpleToType ty
                            let finalT = foldr (\_ acc -> TPointer acc) t declSpecArrays
                            return [(mname, finalT)]
                        _ -> return []
                    let fieldEnv = Map.fromList memberDecls
                    modify $ \s -> s { unionEnv = Map.insert uname fieldEnv (unionEnv s) }
                    modify $ \s -> s { typedefEnv = Map.insert tname (TUnion uname fieldEnv) (typedefEnv s) }
                C.Typedef (Fix (C.EnumDecl _ enumerators (L _ _ tname))) (L _ _ _) -> do
                    let enumType = TInt
                    modify $ \s -> s { typedefEnv = Map.insert tname enumType (typedefEnv s) }
                    forM_ enumerators $ \e -> handleEnumerator enumType e
                C.TypedefFunction (Fix (C.FunctionPrototype retType (L _ _ tname) params)) -> do
                    ret_t <- cimpleToType retType
                    let (isVariadic, filteredParams) = case params of
                            [] -> (False, [])
                            ps -> let lastParam = last ps
                                  in case unFix lastParam of
                                      C.Ellipsis -> (True, init ps)
                                      _          -> (False, ps)
                    param_ts <- forM (filter (not . isVoidParam) filteredParams) $ \case
                        Fix (C.VarDecl ty _ _) -> cimpleToType ty
                        _                      -> fresh
                    let ftype = TFunc param_ts ret_t isVariadic
                    modify $ \s -> s { typedefEnv = Map.insert tname ftype (typedefEnv s) }
                C.Typedef ty (L _ _ name) -> do
                    t <- cimpleToType ty
                    modify $ \s -> s { typedefEnv = Map.insert name t (typedefEnv s) }
                C.EnumDecl _ enumerators (L _ _ name) -> do
                    let enumType = TInt
                    modify $ \s -> s { typedefEnv = Map.insert name enumType (typedefEnv s) }
                    forM_ enumerators $ \e -> handleEnumerator enumType e
                C.EnumConsts maybe_name enumerators -> do
                    let enumType = TInt -- Anonymous enums are compatible with int.
                    forM_ maybe_name $ \(L _ _ name) -> do
                        let scheme = Forall [] enumType
                        modify $ \s -> s { typeEnv = Map.insert name scheme (typeEnv s) }
                    forM_ enumerators $ \e -> handleEnumerator enumType e
                C.FunctionDecl _ (Fix (C.FunctionPrototype retType name params)) ->
                    addFuncSig name retType params
                C.FunctionDefn _ (Fix (C.FunctionPrototype retType name params)) _ ->
                    addFuncSig name retType params
                C.PreprocDefineConst (L _ _ name) valNode -> do
                    ty <- inferExpr valNode `catchError` \_ -> fresh
                    env <- gets typeEnv
                    scheme <- generalize env ty
                    modify $ \s -> s { typeEnv = Map.insert name scheme (typeEnv s) }
                C.PreprocDefineMacro (L _ _ name) params body -> do
                    paramTypes <- forM params $ \_ -> fresh
                    let getParamName (Fix (C.MacroParam (L _ _ n))) = Just n
                        getParamName _                              = Nothing
                    let paramNames = mapMaybe getParamName params
                    let paramSchemes = map (Forall []) paramTypes
                    let paramEnv = Map.fromList $ zip paramNames paramSchemes

                    vaType <- fresh
                    let vaEnv = Map.singleton "__VA_ARGS__" (Forall [] vaType)

                    bodyType <- withEnv (Map.union paramEnv . Map.union vaEnv) (inferExpr body)

                    s <- gets subst
                    finalParamTypes <- forM paramTypes $ \pt -> return (apply s pt)

                    let funcType = TFunc finalParamTypes bodyType True
                    env <- gets typeEnv
                    scheme <- generalize env funcType
                    modify $ \st -> st { typeEnv = Map.insert name scheme (typeEnv st) }
                _ -> return ()
            continuation
        }

    addFuncSig name retType params = do
        ret_t <- cimpleToType retType
        let (isVariadic, filteredParams) = case params of
                [] -> (False, [])
                ps -> let lastParam = last ps
                      in case unFix lastParam of
                          C.Ellipsis -> (True, init ps)
                          _          -> (False, ps)
        param_ts <- forM (filter (not . isVoidParam) filteredParams) $ \case
            Fix (C.VarDecl ty _ _) -> cimpleToType ty
            _                      -> fresh
        let ftype = TFunc param_ts ret_t isVariadic
        env <- gets typeEnv
        scheme <- generalize env ftype
        modify $ \s -> s { typeEnv = Map.insert (lexemeText name) scheme (typeEnv s) }

-- | Type check a file.
typeCheckFile :: TranslationUnit -> LinterM ()
typeCheckFile (file, nodes) = do
    modify $ \s -> s { currentFile = file }
    traverseAst collector (file, nodes)
  where
    collector = astActions
        { doNode = \_ node continuation -> do
            case unFix node of
                C.FunctionDefn _ proto body -> typeCheckFunc proto body
                _                           -> return ()
            continuation
        }

isVoidParam :: Node (Lexeme Text) -> Bool
isVoidParam (Fix (C.VarDecl ty _ _)) = isVoidType ty
isVoidParam node                     = isVoidType node

isVoidType :: Node (Lexeme Text) -> Bool
isVoidType (Fix (C.TyStd (L _ _ "void"))) = True
isVoidType (Fix (C.TyConst t))            = isVoidType t
isVoidType (Fix (C.TyOwner t))            = isVoidType t
isVoidType (Fix (C.TyNullable t))         = isVoidType t
isVoidType (Fix (C.TyNonnull t))          = isVoidType t
isVoidType _                              = False

-- | Type check a function.
typeCheckFunc :: Node (Lexeme Text) -> Node (Lexeme Text) -> LinterM ()
typeCheckFunc (Fix (C.FunctionPrototype retType (L _ _ _) params)) body = do
    modify $ \s -> s { subst = Map.empty }
    expectedRetType <- cimpleToType retType
    paramSchemes <- forM (filter (not . isVoidParam) params) $ \case
        Fix (C.VarDecl ty (L _ _ name) declSpecArrays) -> do
            t <- cimpleToType ty
            let finalT = foldr (\_ acc -> TPointer acc) t declSpecArrays
            return (name, Forall [] finalT)
        _ -> return ("", Forall [] TUnit)
    let paramEnv = Map.fromList $ filter (not . Text.null . fst) paramSchemes
    withEnv (Map.union paramEnv) $ checkStmt body expectedRetType
typeCheckFunc _ _ = return ()

-- Linter Entry

-- | The main analysis function.
analyse :: [TranslationUnit] -> [Text]
analyse tus =
    let
        initialState = LinterState Map.empty Map.empty Map.empty Map.empty Map.empty 0 ""
        linterM = do
            mapM_ collectGlobals tus
            mapM_ typeCheckFile tus
    in
        reverse $ snd $ runState (evalStateT linterM initialState) []

descr :: ([TranslationUnit] -> [Text], (Text, Text))
descr = (analyse, ("type-check", Text.unlines
    [ "A Hindley-Milner based type checker for Cimple."
    , "It checks for type consistency in expressions, assignments, and function calls."
    ]))
