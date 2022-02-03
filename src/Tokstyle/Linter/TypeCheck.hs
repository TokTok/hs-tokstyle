{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.TypeCheck where

import           Control.Monad(void)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..), foldFixM)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (catMaybes)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (BinaryOp,
                                              Lexeme (..), LiteralType (..),
                                              Node, NodeF (..))
import           Language.Cimple.TraverseAst             (AstActions,
                                              astActions,
                                              doNode, traverseAst)
import           Language.Cimple.Diagnostics (HasDiagnostics (..), warn)

data Env = Env
    { diags :: [Text]
    , types :: Types
    }
    deriving (Show)

instance HasDiagnostics Env where
    addDiagnostic diag env@Env{diags} = env{diags = addDiagnostic diag diags}

addName :: Text -> Type -> State Env ()
addName n t = State.modify $ \env@Env{types} -> env{types = Map.insert n t types}


type Types = Map Text Type
data Type
    -- C types
    = Void
    | NamedType Text
    | PointerType Type
    | ConstType Type
    | ArrayType Type [Type]
    | ArrayDimension (Maybe Type)
    | Name Text
    | Conditional Text [Type]

    -- Variable declaration with a type (could be struct member or function
    -- parameter).
    | VarDeclType Text Type
    | StructDeclType Text [Type]
    | FunctionType Type [Type]

    -- Array dimensions.
    | ComputeInt Int
    | ComputeName Text
    | ComputeBop Type BinaryOp Type
    | ComputeSizeof Type
    deriving (Show)

empty :: Env
empty = Env [] Map.empty


extractType :: FilePath -> Node (Lexeme Text) -> NodeF (Lexeme Text) (Maybe Type) -> State Env (Maybe Type)
extractType file n = \case
    TyStd (L _ _ name)         -> ok $ NamedType name
    TyStruct (L _ _ name)      -> ok $ NamedType name
    TyFunc (L _ _ name)        -> ok $ NamedType name
    TyUserDefined (L _ _ name) -> ok $ NamedType name
    TyPointer ty               -> return $ PointerType <$> ty
    TyConst ty                 -> return $ ConstType <$> ty
    DeclSpecArray Nothing      -> ok $ ArrayDimension Nothing
    DeclSpecArray (Just dim)   -> return dim

    LiteralExpr Int     (L _ _ sz) -> ok $ ComputeInt (read $ Text.unpack sz)
    LiteralExpr ConstId (L _ _ sz) -> ok $ ComputeName sz
    BinaryExpr l o r               -> return $ ComputeBop <$> l <*> pure o <*> r
    SizeofType ty                  -> return $ ComputeSizeof <$> ty

    -- Ignore bit size of struct bitfields.
    MemberDecl decl _ -> return decl

    VarDecl ty (L _ _ name) []   -> return $ VarDeclType name <$> ty
    VarDecl ty (L _ _ name) arrs -> return $ VarDeclType name <$> (ArrayType <$> ty <*> sequence arrs)

    Struct (L _ _ name) members -> ok $ StructDeclType name (catMaybes members)

    Comment{} -> return Nothing

    PreprocIfdef (L _ _ cond) decls Nothing -> ok $ Conditional cond (catMaybes decls)

    PreprocElse [] -> return Nothing
    PreprocElse _ -> do
        warn file n $ "#else is not yet supported within types"
        return Nothing

    x -> do
        warn file n $ Text.pack (show x)
        return Nothing

  where ok = return . Just


getTypes :: AstActions (State Env) Text
getTypes = astActions
    { doNode = \file node act ->
        case unFix node of
            Struct{} -> do
                _ <- foldFixM (extractType file node) node
                act

            Typedef{} -> do
                --_ <- error $ show $ node
                act

            -- TODO(iphydf): Implement the rest of the typecheck (draw the rest
            -- of the owl).

            -- Ignore everything inside functions, we'll type-check them later.
            FunctionDefn{} -> return ()
            _ -> act
    }

checkTypes :: FilePath -> Node (Lexeme Text) -> NodeF (Lexeme Text) Type -> State Env Type
checkTypes _file _n = \case
    TyStd         (L _ _ name) -> return $ NamedType name
    TyStruct      (L _ _ name) -> return $ NamedType name
    TyFunc        (L _ _ name) -> return $ NamedType name
    TyUserDefined (L _ _ name) -> return $ NamedType name
    TyPointer ty               -> return $ PointerType ty
    TyConst   ty               -> return $ ConstType ty

    VarDecl ty (L _ _ name) [] -> do
        addName name ty
        return $ VarDeclType name ty

    FunctionPrototype ret (L _ _ name) params -> do
        let ty = FunctionType ret params
        addName name ty
        return ty

    _x -> do
        --_ <- error (show (_file, _x))
        --warn file n $ Text.pack (show x)
        return Void

linter :: AstActions (State Env) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionDefn{} ->
                void $ foldFixM (checkTypes file node) node

            _ -> act
    }

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse tus =
    reverse $ diags globals ++ diags checked
  where
    globals =
        flip State.execState empty
        . traverseAst getTypes
        $ tus

    checked =
        flip State.execState globals
        . traverseAst linter
        $ tus
