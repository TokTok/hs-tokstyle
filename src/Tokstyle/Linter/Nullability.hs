{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Tokstyle.Linter.Nullability (descr) where

import           Control.Monad               (forM_)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Foldable               (traverse_)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (BinaryOp (..), Lexeme (..), Node,
                                              UnaryOp (..))
import qualified Language.Cimple             as C
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)

data Nullability
    = NullableVar
    | NonNullVar
    deriving (Show, Eq, Ord)

type VarInfo = (Nullability, Maybe (Node (Lexeme Text)))

type TypeEnv = Map Text VarInfo

data LinterState = LinterState
    { typeEnv     :: TypeEnv
    , structDefs  :: Map Text TypeEnv
    , nonNullSet  :: Set Text
    , currentFile :: FilePath
    }

type LinterM = State.StateT LinterState (State [Text])

isNullable :: Node (Lexeme Text) -> Bool
isNullable = \case
    Fix (C.TyNullable _) -> True
    Fix (C.TyPointer t)  -> isNullable t
    Fix (C.TyConst t)    -> isNullable t
    _                    -> False

isNonnull :: Node (Lexeme Text) -> Bool
isNonnull = \case
    Fix (C.TyNonnull _) -> True
    Fix (C.TyPointer t) -> isNonnull t
    Fix (C.TyConst t)   -> isNonnull t
    _                   -> False

exprToText :: Node (Lexeme Text) -> Maybe Text
exprToText (Fix node) = case node of
    C.VarExpr (C.L _ _ name) -> Just name
    C.PointerAccess e (C.L _ _ member) -> do
        base <- exprToText e
        Just $ base <> "->" <> member
    C.MemberAccess e (C.L _ _ member) -> do
        base <- exprToText e
        Just $ base <> "." <> member
    C.UnaryExpr C.UopDeref e -> do
        base <- exprToText e
        Just $ "*" <> base
    C.ParenExpr e -> exprToText e
    _ -> Nothing

getParamTypes :: Node (Lexeme Text) -> TypeEnv
getParamTypes (Fix (C.FunctionPrototype _ _ params)) = Map.fromList . concatMap getVarDecls $ params
  where
    getVarDecls (Fix (C.VarDecl ty (C.L _ _ name) _)) =
        let nullability = if isNullable ty then NullableVar else NonNullVar
        in [(name, (nullability, Just ty))]
    getVarDecls _ = []
getParamTypes _ = Map.empty

getStructName :: Node (Lexeme Text) -> Maybe Text
getStructName (Fix node) = case node of
    C.TyPointer t                  -> getStructName t
    C.TyConst t                    -> getStructName t
    C.TyNonnull t                  -> getStructName t
    C.TyStruct (C.L _ _ name)      -> Just name
    C.TyUserDefined (C.L _ _ name) -> Just name
    _                              -> Nothing

getNullability :: Text -> LinterState -> Maybe Nullability
getNullability name st =
    case Text.splitOn "->" name of
        [var] -> fst <$> Map.lookup var (typeEnv st)
        [base, member] -> do
            (_, baseTypeM) <- Map.lookup base (typeEnv st)
            baseType <- baseTypeM
            structName <- getStructName baseType
            structDef <- Map.lookup structName (structDefs st)
            (memberNullability, _) <- Map.lookup member structDef
            Just memberNullability
        _ -> Nothing

isExprNonNull :: Node (Lexeme Text) -> LinterM Bool
isExprNonNull (Fix node) = case node of
    C.UnaryExpr C.UopAddress _ -> return True
    _ -> do
        st <- State.get
        case exprToText (Fix node) of
            Just name -> return $ Set.member name (nonNullSet st) || getNullability name st == Just NonNullVar
            Nothing   -> return False

checkCondition :: Node (Lexeme Text) -> (Set Text, Set Text)
checkCondition e = go e
  where
    go n@(Fix node) = case node of
        C.BinaryExpr lhs BopNe (Fix (C.LiteralExpr C.ConstId (C.L _ _ "NULL"))) ->
            (fromText $ exprToText lhs, Set.empty)
        C.BinaryExpr (Fix (C.LiteralExpr C.ConstId (C.L _ _ "NULL"))) BopNe rhs ->
            (fromText $ exprToText rhs, Set.empty)
        C.BinaryExpr lhs BopEq (Fix (C.LiteralExpr C.ConstId (C.L _ _ "NULL"))) ->
            (Set.empty, fromText $ exprToText lhs)
        C.BinaryExpr (Fix (C.LiteralExpr C.ConstId (C.L _ _ "NULL"))) BopEq rhs ->
            (Set.empty, fromText $ exprToText rhs)
        C.BinaryExpr lhs BopNe (Fix (C.LiteralExpr _ (C.L _ _ "nullptr"))) ->
            (fromText $ exprToText lhs, Set.empty)
        C.BinaryExpr (Fix (C.LiteralExpr _ (C.L _ _ "nullptr"))) BopNe rhs ->
            (fromText $ exprToText rhs, Set.empty)
        C.BinaryExpr lhs BopEq (Fix (C.LiteralExpr _ (C.L _ _ "nullptr"))) ->
            (Set.empty, fromText $ exprToText lhs)
        C.BinaryExpr (Fix (C.LiteralExpr _ (C.L _ _ "nullptr"))) BopEq rhs ->
            (Set.empty, fromText $ exprToText rhs)

        C.UnaryExpr UopNot inner ->
            let (thenSet, elseSet) = checkCondition inner
            in (elseSet, thenSet)

        C.BinaryExpr lhs BopAnd rhs ->
            let (then1, else1) = checkCondition lhs
                (then2, else2) = checkCondition rhs
            in (then1 `Set.union` then2, else1 `Set.union` else2)

        C.BinaryExpr lhs BopOr rhs ->
            let (then1, else1) = checkCondition lhs
                (then2, else2) = checkCondition rhs
            in (then1 `Set.union` then2, else1 `Set.union` else2)

        C.ParenExpr inner -> checkCondition inner

        C.VarExpr {} -> (fromText $ exprToText n, Set.empty)
        C.PointerAccess {} -> (fromText $ exprToText n, Set.empty)
        C.MemberAccess {} -> (fromText $ exprToText n, Set.empty)
        C.UnaryExpr C.UopDeref _ -> (fromText $ exprToText n, Set.empty)

        _ -> (Set.empty, Set.empty)

    fromText Nothing  = Set.empty
    fromText (Just t) = Set.singleton t

hasReturn :: Node (Lexeme Text) -> Bool
hasReturn = \case
    Fix (C.Return _)            -> True
    Fix (C.IfStmt _ t (Just e)) -> hasReturn t && hasReturn e
    Fix (C.CompoundStmt stmts)  -> any hasReturn stmts
    _                           -> False

analyseStmts :: [Node (Lexeme Text)] -> LinterM ()
analyseStmts = mapM_ analyseExpr

analyseExpr :: Node (Lexeme Text) -> LinterM ()
analyseExpr (Fix fixNode) = case fixNode of
    C.CastExpr toType fromExpr -> do
        st <- State.get
        let fromNameM = exprToText fromExpr
        forM_ fromNameM $ \fromName ->
            forM_ (getNullability fromName st) $ \nullability ->
                if isNonnull toType && nullability == NullableVar && not (Set.member fromName (nonNullSet st))
                then State.lift . warn (currentFile st) fromExpr $
                        "expression `" <> fromName
                        <> "` is nullable and has not been checked before this cast"
                else return ()
        traverse_ analyseExpr fixNode

    C.AssignExpr lhs _ rhs -> do
        analyseExpr lhs
        analyseExpr rhs
        let lhsNameM = exprToText lhs
        rhsIsNonNull <- isExprNonNull rhs
        forM_ lhsNameM $ \lhsName ->
            if rhsIsNonNull
                then State.modify $ \s -> s { nonNullSet = Set.insert lhsName (nonNullSet s) }
                else State.modify $ \s -> s { nonNullSet = Set.delete lhsName (nonNullSet s) }

    C.BinaryExpr lhs C.BopAnd rhs -> do
        analyseExpr lhs
        let (nonNullInThen, _) = checkCondition lhs
        st <- State.get
        State.put $ st { nonNullSet = nonNullSet st `Set.union` nonNullInThen }
        analyseExpr rhs
        State.put st

    C.IfStmt condition thenBranch elseBranchM -> do
        analyseExpr condition
        let (nonNullInThen, nonNullInElse) = checkCondition condition

        initialState <- State.get

        State.put $ initialState { nonNullSet = nonNullSet initialState `Set.union` nonNullInThen }
        analyseStmts' thenBranch
        let thenReturns = hasReturn thenBranch
        stateAfterThen <- State.get

        (stateAfterElse, elseReturns) <- case elseBranchM of
            Nothing -> do
                let s = initialState { nonNullSet = nonNullSet initialState `Set.union` nonNullInElse }
                return (s, False)
            Just elseBranch -> do
                State.put $ initialState { nonNullSet = nonNullSet initialState `Set.union` nonNullInElse }
                analyseStmts' elseBranch
                s <- State.get
                return (s, hasReturn elseBranch)

        let finalSet = if thenReturns && not elseReturns
                         then nonNullSet stateAfterElse
                         else if not thenReturns && elseReturns
                         then nonNullSet stateAfterThen
                         else nonNullSet stateAfterThen `Set.intersection` nonNullSet stateAfterElse

        State.put $ stateAfterThen { nonNullSet = finalSet }

    C.TernaryExpr condition thenBranch elseBranch -> do
        analyseExpr condition
        let (nonNullInThen, nonNullInElse) = checkCondition condition

        initialState <- State.get

        State.put $ initialState { nonNullSet = nonNullSet initialState `Set.union` nonNullInThen }
        analyseExpr thenBranch
        stateAfterThen <- State.get

        State.put $ initialState { nonNullSet = nonNullSet initialState `Set.union` nonNullInElse }
        analyseExpr elseBranch
        stateAfterElse <- State.get

        State.put $ stateAfterThen { nonNullSet = nonNullSet stateAfterThen `Set.intersection` nonNullSet stateAfterElse }

    C.VarDeclStmt (Fix (C.VarDecl ty (C.L _ _ name) _)) initM -> do
        let nullability = if isNullable ty then NullableVar else NonNullVar
        State.modify $ \s -> s { typeEnv = Map.insert name (nullability, Just ty) (typeEnv s) }
        forM_ initM $ \i -> do
            analyseExpr i
            isInitNonNull <- isExprNonNull i
            if isInitNonNull
                then State.modify $ \s -> s { nonNullSet = Set.insert name (nonNullSet s) }
                else return ()

    _ -> traverse_ analyseExpr fixNode

analyseStmts' :: Node (Lexeme Text) -> LinterM ()
analyseStmts' (Fix (C.CompoundStmt stmts)) = analyseStmts stmts
analyseStmts' node                         = analyseExpr node

collectStructs :: AstActions (State (Map Text TypeEnv)) Text
collectStructs = astActions
    { doNode = \_ node act ->
        case unFix node of
            C.Typedef (Fix (C.Struct _ members)) structName -> do
                let fieldEnv = Map.fromList . concatMap getFieldDecls $ members
                State.modify (Map.insert (C.lexemeText structName) fieldEnv)
                act
            _ -> act
    }
  where
    getFieldDecls (Fix (C.MemberDecl (Fix (C.VarDecl ty name _)) _)) =
        let nullability = if isNullable ty then NullableVar else NonNullVar
        in [(C.lexemeText name, (nullability, Just ty))]
    getFieldDecls _ = []

linter :: Map Text TypeEnv -> AstActions (State [Text]) Text
linter defs = astActions
    { doNode = \file node act ->
        case unFix node of
            C.FunctionDefn _ proto body ->
                let tenv = getParamTypes proto
                    initialNonNulls = Map.keysSet . Map.filter ((== NonNullVar) . fst) $ tenv
                    initialState = LinterState tenv defs initialNonNulls file
                in State.evalStateT (analyseStmts' body) initialState
            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse input =
    let defs = State.execState (traverseAst collectStructs input) Map.empty
    in reverse . flip State.execState [] . traverseAst (linter defs) $ input

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("nullability", Text.unlines
    [ "Warns when a _Nullable pointer is cast to a _Nonnull pointer without a null check."
    , ""
    , "**Reason:** Casting a nullable pointer to a non-null pointer without ensuring it's not"
    , "null can lead to null pointer dereferences and crashes."
    ]))
