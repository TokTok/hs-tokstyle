{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tokstyle.Analysis.VTable
    ( VTableMap
    , resolveVTables
    ) where

import           Control.Monad.State.Strict  (State, execState, gets, modify)
import           Data.Fix                    (Fix (..))
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Language.Cimple             as C
import           Language.Cimple.Pretty      (showNodePlain)
import           Language.Cimple.TraverseAst (AstActions (..), astActions,
                                              traverseAst)
import           Tokstyle.Analysis.Scope     (ScopedId (..))
import           Tokstyle.Common.TypeSystem  (TypeDescr (..), TypeInfo (..),
                                              TypeRef (..), TypeSystem,
                                              lookupType)

-- | A map from a v-table's ScopedId to its field-to-function mapping.
type VTableMap = Map ScopedId (Map Text ScopedId)

data VTableState = VTableState
    { vtsMap        :: VTableMap
    , vtsTypeSystem :: TypeSystem
    }

-- | Traverses the AST to find global v-tables and resolve their function pointers.
resolveVTables :: [C.Node (C.Lexeme ScopedId)] -> TypeSystem -> VTableMap
resolveVTables ast typeSystem =
    let initialState = VTableState Map.empty typeSystem
    in vtsMap $ execState (traverseAst vtableActions ast) initialState

vtableActions :: AstActions (State VTableState) ScopedId
vtableActions = astActions
    { doNode = \_ node act -> do
        case unFix node of
            C.ConstDefn C.Global ty vtableName (Fix (C.InitialiserList initializers)) ->
                handleConstDefn ty vtableName initializers
            C.ConstDefn C.Static ty vtableName (Fix (C.InitialiserList initializers)) ->
                handleConstDefn ty vtableName initializers
            _ -> return ()
        act
    }

handleConstDefn :: C.Node (C.Lexeme ScopedId) -> C.Lexeme ScopedId -> [C.Node (C.Lexeme ScopedId)] -> State VTableState ()
handleConstDefn tyNode (C.L _ _ vtableSid) initializers = do
    typeSystem <- gets vtsTypeSystem
    case getTypeName tyNode of
        Just typeName ->
            case lookupType typeName typeSystem of
                Just (StructDescr _ fields) | not (null fields) && all (isFuncPtrType . snd) fields -> do
                    let funcInitializers = map getInitializerFuncSid initializers
                    let fieldNames = map (C.lexemeText . fst) fields
                    let vtable = Map.fromList $ zip fieldNames funcInitializers
                    modify $ \st -> st { vtsMap = Map.insert vtableSid vtable (vtsMap st) }
                _ -> return () -- Not a struct or not all fields are function pointers
        _ -> return () -- Could not determine type name

isFuncPtrType :: TypeInfo -> Bool
isFuncPtrType (Pointer (TypeRef FuncRef _)) = True
isFuncPtrType (Pointer (ExternalType _))    = True -- For typedef'd function pointers
isFuncPtrType (Const t)                     = isFuncPtrType t
isFuncPtrType (TypeRef FuncRef _)           = True
isFuncPtrType _                             = False

getTypeName :: C.Node (C.Lexeme ScopedId) -> Maybe Text
getTypeName (Fix node) = case node of
    C.TyUserDefined (C.L _ _ sid) -> Just (sidName sid)
    C.TyStruct (C.L _ _ sid)      -> Just (sidName sid)
    C.TyConst t                   -> getTypeName t
    _                             -> Nothing

getInitializerFuncSid :: C.Node (C.Lexeme ScopedId) -> ScopedId
getInitializerFuncSid (Fix (C.VarExpr (C.L _ _ sid))) = sid
getInitializerFuncSid (Fix (C.CastExpr _ expr))       = getInitializerFuncSid expr
getInitializerFuncSid n                               = error $ "VTable initializer is not a function designator: " ++ show (showNodePlain n)
