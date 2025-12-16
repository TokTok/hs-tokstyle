{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module implements the Scope Binding pass.
--
-- This pass traverses the AST and replaces all variable names (Text) with
-- unique identifiers (ScopedId). This eliminates any ambiguity from name
-- shadowing and is a prerequisite for a correct and precise points-to analysis.
module Tokstyle.Analysis.Scope
    ( ScopedId(..)
    , ScopeState(..)
    , runScopePass
    , initialScopeState
    , dummyScopedId
    ) where

import           Control.Monad              (forM, forM_, msum, when)
import           Control.Monad.State.Strict (State, get, gets, modify, put,
                                             runState)
import           Data.Fix                   (Fix (..), unFix)
import           Data.Hashable              (Hashable (..))
import           Data.List                  (permutations)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, fromMaybe, mapMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Debug.Trace                (trace)
import qualified Language.Cimple            as C
import           Language.Cimple.Pretty     (showNodePlain)
import           Prettyprinter              (Pretty (..), (<>))
import           Text.Groom                 (groom)

debugging :: Bool
debugging = False

dtrace :: String -> a -> a
dtrace msg x = if debugging then trace msg x else x

-- | A unique identifier for a variable, including its original name and scope info.
data ScopedId = ScopedId
    { sidUniqueId :: Int    -- ^ The globally unique ID.
    , sidName     :: Text   -- ^ The original name, for debugging.
    , sidScope    :: C.Scope -- ^ The scope it was defined in (Global or Static).
    } deriving (Show)

instance Eq ScopedId where
    a == b = sidUniqueId a == sidUniqueId b

instance Ord ScopedId where
    compare a b = compare (sidUniqueId a) (sidUniqueId b)

instance Hashable ScopedId where
    hashWithSalt salt sid = hashWithSalt salt (sidUniqueId sid)

instance Pretty ScopedId where
    pretty sid | sidUniqueId sid == 0 = pretty (sidName sid)
               | otherwise            = pretty (sidName sid) <> "_" <> pretty (sidUniqueId sid)


-- | A stack of symbol tables, one for each scope.
type SymbolTable = [Map Text ScopedId]

-- | The state for the scope analysis traversal.
data ScopeState = ScopeState
    { ssTable        :: SymbolTable -- ^ The stack of symbol tables.
    , ssNextId       :: Int         -- ^ The next available unique ID.
    , ssCurrentScope :: C.Scope     -- ^ The scope of the current function.
    , ssErrors       :: [String]    -- ^ A list of errors encountered.
    , ssFuncParamIds :: Map Text [ScopedId]
    } deriving (Show)

-- | The initial state for the scope analysis.
initialScopeState :: ScopeState
initialScopeState = ScopeState [Map.empty] 1 C.Global [] Map.empty

-- | Runs the scope binding pass on a list of translation units.
runScopePass :: [C.Node (C.Lexeme Text)] -> ([C.Node (C.Lexeme ScopedId)], ScopeState)
runScopePass tu = runState (transformToplevels tu) initialScopeState

-- | Helper to push a new scope onto the symbol table stack.
pushScope :: State ScopeState ()
pushScope = do
    st <- get
    let newSt = st { ssTable = Map.empty : ssTable st }
    dtrace ("pushScope: new depth = " ++ show (length (ssTable newSt))) $ put newSt

-- | Helper to pop a scope from the symbol table stack.
popScope :: State ScopeState ()
popScope = do
    st <- get
    let newSt = st { ssTable = tail (ssTable st) }
    dtrace ("popScope: new depth = " ++ show (length (ssTable newSt))) $ put newSt

-- | Adds a new variable to the current scope.
addVarToScope :: Text -> State ScopeState ScopedId
addVarToScope name = do
    st <- get
    let newId = ssNextId st
    let scope = if length (ssTable st) == 1 then C.Global else C.Local
    let scopedId = ScopedId newId name scope
    let newTable = case ssTable st of
            []             -> error "Symbol table stack is empty"
            (current:rest) -> Map.insert name scopedId current : rest
    dtrace ("addVarToScope: " ++ groom name ++ " -> " ++ groom scopedId ++ " in scope " ++ show scope ++ "\n  TABLE_BEFORE: " ++ groom (ssTable st) ++ "\n  TABLE_AFTER: " ++ groom newTable) $
        put $ st { ssTable = newTable, ssNextId = newId + 1 }
    return scopedId

addScopedIdToScope :: Text -> ScopedId -> State ScopeState ()
addScopedIdToScope name scopedId = do
    st <- get
    let newTable = case ssTable st of
            []             -> error "Symbol table stack is empty"
            (current:rest) -> Map.insert name scopedId current : rest
    put $ st { ssTable = newTable }

-- | Adds a variable to the global scope (the last element in the symbol table stack)
addVarToGlobalScope :: C.Scope -> Text -> State ScopeState ScopedId
addVarToGlobalScope scope name = do
    st <- get
    let newId = ssNextId st
    let scopedId = ScopedId newId name scope
    let (globals:locals) = reverse (ssTable st)
    let newGlobals = Map.insert name scopedId globals
    let newTable = reverse (newGlobals:locals)
    dtrace ("addVarToGlobalScope: " ++ groom name ++ " -> " ++ groom scopedId ++ "\n  TABLE_BEFORE: " ++ groom (ssTable st) ++ "\n  TABLE_AFTER: " ++ groom newTable) $
        put $ st { ssTable = newTable, ssNextId = newId + 1 }
    return scopedId

-- | Looks up a variable only in the global scope
lookupVarInGlobalScope :: Text -> State ScopeState (Maybe ScopedId)
lookupVarInGlobalScope name = do
    st <- get
    let result = Map.lookup name (last (ssTable st))
    dtrace ("lookupVarInGlobalScope: " ++ groom name ++ " -> " ++ groom result) $ return result

-- | Finds an existing ScopedId for a toplevel name or creates a new one.
findOrCreateToplevelId :: C.Scope -> Text -> State ScopeState ScopedId
findOrCreateToplevelId scope name = do
    dtrace ("findOrCreateToplevelId: " ++ groom name) $ do
        mSid <- lookupVarInGlobalScope name
        case mSid of
            Just sid -> dtrace ("  found existing: " ++ groom sid) $ return sid
            Nothing  -> dtrace "  not found, creating new." $ addVarToGlobalScope scope name

-- | Looks up a variable in the symbol table stack.
lookupVar :: Text -> State ScopeState ScopedId
lookupVar name = do
    st <- get
    let result = msum $ map (Map.lookup name) (ssTable st)
    dtrace ("lookupVar: " ++ groom name ++ " in table " ++ groom (ssTable st) ++ " -> " ++ groom result) $
        case result of
            Just scopedId -> return scopedId
            Nothing       -> do
                let err = "Undeclared variable: " ++ show name
                put $ st { ssErrors = ssErrors st ++ [err] }
                return $ dummyScopedId name

-- | Creates a dummy ScopedId for non-variable identifiers like struct fields.
dummyScopedId :: Text -> ScopedId
dummyScopedId name = ScopedId 0 name C.Global

transformToplevels :: [C.Node (C.Lexeme Text)] -> State ScopeState [C.Node (C.Lexeme ScopedId)]
transformToplevels = mapM transformNode

transformLexeme :: C.Lexeme Text -> State ScopeState (C.Lexeme ScopedId)
transformLexeme (C.L pos cls text) = return $ C.L pos cls (dummyScopedId text)

transformComment :: C.Comment (C.Lexeme Text) -> State ScopeState (C.Comment (C.Lexeme ScopedId))
transformComment (Fix commentNode) = Fix <$> case commentNode of
    C.DocComment as -> C.DocComment <$> mapM transformComment as
    C.DocAttention -> return C.DocAttention
    C.DocBrief -> return C.DocBrief
    C.DocDeprecated -> return C.DocDeprecated
    C.DocExtends l -> C.DocExtends <$> transformLexeme l
    C.DocFile -> return C.DocFile
    C.DocImplements l -> C.DocImplements <$> transformLexeme l
    C.DocNote -> return C.DocNote
    C.DocParam ml l -> C.DocParam <$> traverse transformLexeme ml <*> transformLexeme l
    C.DocReturn -> return C.DocReturn
    C.DocRetval -> return C.DocRetval
    C.DocSection l -> C.DocSection <$> transformLexeme l
    C.DocSecurityRank l ml' l' -> C.DocSecurityRank <$> transformLexeme l <*> traverse transformLexeme ml' <*> transformLexeme l'
    C.DocSee l -> C.DocSee <$> transformLexeme l
    C.DocSubsection l -> C.DocSubsection <$> transformLexeme l
    C.DocPrivate -> return C.DocPrivate
    C.DocLine as -> C.DocLine <$> mapM transformComment as
    C.DocCode l as l' -> C.DocCode <$> transformLexeme l <*> mapM transformComment as <*> transformLexeme l'
    C.DocWord l -> C.DocWord <$> transformLexeme l
    C.DocRef l -> C.DocRef <$> transformLexeme l
    C.DocP l -> C.DocP <$> transformLexeme l

transformNode :: C.Node (C.Lexeme Text) -> State ScopeState (C.Node (C.Lexeme ScopedId))
transformNode (Fix node) = dtrace ("transformNode: " ++ Text.unpack (showNodePlain (Fix node))) $ Fix <$> case node of
    C.FunctionDefn fScope (Fix (C.FunctionPrototype ty (C.L pos cls name) params)) body -> do
        funcSid <- findOrCreateToplevelId C.Global name
        modify $ \st -> st { ssCurrentScope = fScope }
        pushScope
        mParamIds <- gets (Map.lookup name . ssFuncParamIds)
        case mParamIds of
            Just pids -> do
                let namedParams = mapMaybe (\case (Fix (C.VarDecl _ (C.L _ _ paramName) _)) -> Just paramName; _ -> Nothing) params
                when (length pids /= length namedParams) $
                    error $ "Function " ++ show name ++ " has multiple definitions with different number of parameters."
                forM_ (zip namedParams pids) $ \(paramName, pid) -> do
                    addScopedIdToScope paramName pid
            Nothing -> do
                newPids <- forM params $ \paramNode -> do
                    case unFix paramNode of
                        C.VarDecl _ (C.L _ _ paramName) _ -> Just <$> addVarToScope paramName
                        _                                 -> return Nothing
                modify $ \st -> st { ssFuncParamIds = Map.insert name (catMaybes newPids) (ssFuncParamIds st) }
        transformedParams <- mapM transformNode params
        transformedBody <- transformNode body
        popScope
        modify $ \st -> st { ssCurrentScope = C.Global }
        transformedTy <- transformNode ty
        let transformedProto = C.FunctionPrototype transformedTy (C.L pos cls funcSid) transformedParams
        return (C.FunctionDefn fScope (Fix transformedProto) transformedBody)

    C.FunctionDecl scope childNode -> do
        let transformedNode = case unFix childNode of
                C.FunctionPrototype ty (C.L pos cls name) params -> do
                    funcSid <- findOrCreateToplevelId scope name
                    pushScope
                    transformedParams <- mapM transformNode params
                    popScope
                    transformedTy <- transformNode ty
                    let transformedProto = C.FunctionPrototype transformedTy (C.L pos cls funcSid) transformedParams
                    return (Fix transformedProto)
                _ -> transformNode childNode
        C.FunctionDecl scope <$> transformedNode

    C.CompoundStmt stmts -> do
        pushScope
        transformedStmts <- mapM transformNode stmts
        popScope
        return (C.CompoundStmt transformedStmts)

    C.ForStmt init' cond next body -> do
        pushScope
        transformedInit <- transformNode init'
        transformedCond <- transformNode cond
        transformedNext <- transformNode next
        transformedBody <- transformNode body
        popScope
        return (C.ForStmt transformedInit transformedCond transformedNext transformedBody)

    C.VarDecl ty (C.L pos cls name) arr -> do
        st <- get
        let currentScope = head (ssTable st)
        scopedId <- case Map.lookup name currentScope of
            Just sid -> return sid
            Nothing  -> addVarToScope name
        C.VarDecl <$> transformNode ty
                   <*> pure (C.L pos cls scopedId)
                   <*> mapM transformNode arr

    C.VarDeclStmt decl mInit -> do
        transformedDecl <- transformNode decl
        transformedMInit <- traverse transformNode mInit
        return (C.VarDeclStmt transformedDecl transformedMInit)

    C.VarExpr (C.L pos cls name) -> do
        scopedId <- lookupVar name
        return $ C.VarExpr (C.L pos cls scopedId)

    C.IfStmt cond thenB mElseB -> do
        transformedCond <- transformNode cond
        transformedThenB <- transformNode thenB
        transformedMElseB <- traverse transformNode mElseB
        return (C.IfStmt transformedCond transformedThenB transformedMElseB)

    C.ConstDefn scope ty (C.L pos cls name) val -> do
        scopedId <- addVarToScope name
        C.ConstDefn scope <$> transformNode ty
                           <*> pure (C.L pos cls scopedId)
                           <*> transformNode val

    C.ConstDecl ty (C.L pos cls name) -> do
        scopedId <- addVarToGlobalScope C.Global name
        C.ConstDecl <$> transformNode ty
                     <*> pure (C.L pos cls scopedId)

    C.Typedef ty (C.L pos cls name) -> do
        -- We don't need to store typedefs in the variable symbol table.
        C.Typedef <$> transformNode ty <*> pure (C.L pos cls (dummyScopedId name))

    C.AggregateDecl decl -> C.AggregateDecl <$> transformNode decl

    C.Struct (C.L pos cls name) members -> do
        -- We don't need to store struct names in the variable symbol table.
        C.Struct (C.L pos cls (dummyScopedId name)) <$> mapM transformNode members

    C.Union (C.L pos cls name) members -> do
        -- We don't need to store union names in the variable symbol table.
        C.Union (C.L pos cls (dummyScopedId name)) <$> mapM transformNode members

    C.EnumDecl (C.L pos cls name) enums (C.L pos' cls' tyName) -> do
        -- We don't need to store enum type names in the variable symbol table.
        -- However, the enumerators themselves are constants and should be added.
        transformedEnums <- mapM transformNode enums
        return (C.EnumDecl (C.L pos cls (dummyScopedId name)) transformedEnums (C.L pos' cls' (dummyScopedId tyName)))

    C.EnumConsts mName enums -> do
        -- Enum constants are added to the global scope.
        mScopedId <- forM mName $ \(C.L pos cls name) -> do
            scopedId <- addVarToGlobalScope C.Global name
            return (C.L pos cls scopedId)
        transformedEnums <- mapM transformNode enums
        return (C.EnumConsts mScopedId transformedEnums)

    C.Enumerator (C.L pos cls name) mVal -> do
        -- Each enumerator is a constant in the global scope.
        scopedId <- addVarToGlobalScope C.Global name
        C.Enumerator (C.L pos cls scopedId) <$> traverse transformNode mVal

    C.MemberDecl decl mBits -> C.MemberDecl <$> transformNode decl <*> traverse transformLexeme mBits

    C.TypedefFunction (Fix (C.FunctionPrototype ty (C.L pos cls name) params)) -> do
        -- The typedef name itself is a type, not a variable.
        -- The parameters are in a temporary scope for the declaration.
        pushScope
        transformedParams <- mapM transformNode params
        popScope
        transformedTy <- transformNode ty
        let transformedProtoNode = C.FunctionPrototype transformedTy (C.L pos cls (dummyScopedId name)) transformedParams
        return (C.TypedefFunction (Fix transformedProtoNode))

    C.FunctionCall fun args -> C.FunctionCall <$> transformNode fun <*> mapM transformNode args
    C.Label (C.L pos cls name) stmt -> C.Label (C.L pos cls (dummyScopedId name)) <$> transformNode stmt
    C.Goto (C.L pos cls name) -> return $ C.Goto (C.L pos cls (dummyScopedId name))
    C.SwitchStmt cond body -> C.SwitchStmt <$> transformNode cond <*> mapM transformNode body
    C.WhileStmt cond body -> C.WhileStmt <$> transformNode cond <*> transformNode body
    C.DoWhileStmt body cond -> C.DoWhileStmt <$> transformNode body <*> transformNode cond
    C.Return mExpr -> C.Return <$> traverse transformNode mExpr
    C.ExprStmt expr -> C.ExprStmt <$> transformNode expr
    C.AssignExpr lhs op rhs -> C.AssignExpr <$> transformNode lhs <*> pure op <*> transformNode rhs
    C.MemberAccess base (C.L pos cls field) -> C.MemberAccess <$> transformNode base <*> pure (C.L pos cls (dummyScopedId field))
    C.PointerAccess base (C.L pos cls field) -> C.PointerAccess <$> transformNode base <*> pure (C.L pos cls (dummyScopedId field))
    C.ArrayAccess base idx -> C.ArrayAccess <$> transformNode base <*> transformNode idx
    C.UnaryExpr op expr -> C.UnaryExpr op <$> transformNode expr
    C.BinaryExpr lhs op rhs -> C.BinaryExpr <$> transformNode lhs <*> pure op <*> transformNode rhs
    C.TernaryExpr cond thenExpr elseExpr -> C.TernaryExpr <$> transformNode cond <*> transformNode thenExpr <*> transformNode elseExpr
    C.ParenExpr expr -> C.ParenExpr <$> transformNode expr
    C.CastExpr ty expr -> C.CastExpr <$> transformNode ty <*> transformNode expr
    C.SizeofExpr expr -> C.SizeofExpr <$> transformNode expr
    C.SizeofType ty -> C.SizeofType <$> transformNode ty
    C.LiteralExpr C.ConstId (C.L pos cls name) -> do
        scopedId <- lookupVar name
        return $ C.VarExpr (C.L pos cls scopedId)
    C.LiteralExpr ty l -> return $ C.LiteralExpr ty (fmap dummyScopedId l)
    C.TyStd l -> return $ C.TyStd (fmap dummyScopedId l)
    C.TyPointer ty -> C.TyPointer <$> transformNode ty
    C.TyStruct l -> return $ C.TyStruct (fmap dummyScopedId l)
    C.TyUnion l -> return $ C.TyUnion (fmap dummyScopedId l)
    C.TyUserDefined l -> return $ C.TyUserDefined (fmap dummyScopedId l)
    C.Break -> return C.Break
    C.Continue -> return C.Continue
    C.Case cond stmt -> C.Case <$> transformNode cond <*> transformNode stmt
    C.Default stmt -> C.Default <$> transformNode stmt
    C.InitialiserList exprs -> C.InitialiserList <$> mapM transformNode exprs
    C.TyConst ty -> C.TyConst <$> transformNode ty
    C.TyFunc l -> return $ C.TyFunc (fmap dummyScopedId l)
    C.Ellipsis -> return C.Ellipsis

    C.PreprocIf cond thenNodes elseNode -> C.PreprocIf <$> transformNode cond <*> mapM transformNode thenNodes <*> transformNode elseNode
    C.PreprocIfdef (C.L pos cls name) thenNodes elseNode -> C.PreprocIfdef . C.L pos cls <$> lookupVar name <*> mapM transformNode thenNodes <*> transformNode elseNode
    C.PreprocIfndef (C.L pos cls name) thenNodes elseNode -> C.PreprocIfndef . C.L pos cls <$> lookupVar name <*> mapM transformNode thenNodes <*> transformNode elseNode
    C.PreprocElse nodes -> C.PreprocElse <$> mapM transformNode nodes

    C.Commented c e -> C.Commented <$> transformNode c <*> transformNode e
    C.Comment style start contents end -> C.Comment style <$> transformLexeme start <*> mapM transformLexeme contents <*> transformLexeme end
    C.Group nodes -> C.Group <$> mapM transformNode nodes
    C.ExternC nodes -> C.ExternC <$> mapM transformNode nodes

    C.LicenseDecl l nodes -> C.LicenseDecl <$> transformLexeme l <*> mapM transformNode nodes

    C.CopyrightDecl l ml ls -> C.CopyrightDecl <$> transformLexeme l <*> traverse transformLexeme ml <*> mapM transformLexeme ls

    C.PreprocInclude l -> C.PreprocInclude <$> transformLexeme l

    C.PreprocDefineConst (C.L pos cls name) val -> do
        scopedId <- addVarToGlobalScope C.Global name
        C.PreprocDefineConst (C.L pos cls scopedId) <$> transformNode val

    C.DeclSpecArray ma -> C.DeclSpecArray <$> traverse transformNode ma

    C.ArrayDim n a -> C.ArrayDim n <$> transformNode a

    C.PreprocDefine (C.L pos cls name) -> do
        scopedId <- addVarToGlobalScope C.Global name
        return $ C.PreprocDefine (C.L pos cls scopedId)

    C.CommentInfo c -> C.CommentInfo <$> transformComment c

    C.CommentExpr a b -> C.CommentExpr <$> transformNode a <*> transformNode b

    C.VLA ty (C.L pos cls name) size -> do
        scopedId <- addVarToScope name
        C.VLA <$> transformNode ty
              <*> pure (C.L pos cls scopedId)
              <*> transformNode size

    C.CommentSection a as b -> C.CommentSection <$> transformNode a <*> mapM transformNode as <*> transformNode b

    C.CommentSectionEnd l -> C.CommentSectionEnd <$> transformLexeme l

    C.TyNonnull a -> C.TyNonnull <$> transformNode a

    C.TyNullable a -> C.TyNullable <$> transformNode a

    C.StaticAssert a l -> C.StaticAssert <$> transformNode a <*> transformLexeme l

    C.PreprocDefined (C.L pos cls name) -> do
        scopedId <- lookupVar name
        return $ C.PreprocDefined (C.L pos cls scopedId)

    C.PreprocElif a as b -> C.PreprocElif <$> transformNode a <*> mapM transformNode as <*> transformNode b

    C.PreprocScopedDefine a as b -> C.PreprocScopedDefine <$> transformNode a <*> mapM transformNode as <*> transformNode b

    C.PreprocDefineMacro (C.L pos cls name) params body -> do
        scopedId <- addVarToGlobalScope C.Global name
        pushScope
        transformedParams <- mapM transformNode params
        transformedBody <- transformNode body
        popScope
        return $ C.PreprocDefineMacro (C.L pos cls scopedId) transformedParams transformedBody

    C.MacroParam (C.L pos cls name) -> do
        scopedId <- addVarToScope name
        return $ C.MacroParam (C.L pos cls scopedId)

    C.MacroBodyStmt a -> C.MacroBodyStmt <$> transformNode a

    C.PreprocUndef (C.L pos cls name) -> do
        scopedId <- lookupVar name
        return $ C.PreprocUndef (C.L pos cls scopedId)

    C.CallbackDecl typeLexeme (C.L pos cls name) -> do
        scopedId <- lookupVar name
        C.CallbackDecl <$> transformLexeme typeLexeme
                       <*> pure (C.L pos cls scopedId)

    C.CompoundLiteral a b -> C.CompoundLiteral <$> transformNode a <*> transformNode b

    C.TyForce a -> C.TyForce <$> transformNode a

    C.TyBitwise a -> C.TyBitwise <$> transformNode a

    C.AttrPrintf l l' a -> C.AttrPrintf <$> transformLexeme l <*> transformLexeme l' <*> transformNode a

    C.MacroBodyFunCall a -> C.MacroBodyFunCall <$> transformNode a

    other -> error $ "transformNode: Unhandled AST node: " ++ show (fmap (const ()) other)
