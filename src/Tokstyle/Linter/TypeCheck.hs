{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.TypeCheck where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..), foldFixM)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (BinaryOp, Lexeme (..),
                                              LiteralType (..), Node,
                                              NodeF (..))
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Text.PrettyPrint.ANSI.Leijen (Pretty(..), (<+>), text, colon, vcat)


newtype Env = Env
    { types :: Map Text Type
    }
    deriving (Show)

instance Pretty Env where
    pretty =
        vcat
        . map (\(k, v) -> text (Text.unpack k) <+> colon <+> text (show v))
        . Map.assocs
        . types

empty :: Env
empty = Env Map.empty

addName :: Text -> Type -> State Env ()
addName n t = State.modify $ \env@Env{types} -> env{types = Map.insert n t types}


data Type
    -- C types
    = T_Void
    | T_Name Text
    | T_Ptr Type
    | T_Func Type [Type]
    deriving (Show)

inferTypes :: NodeF (Lexeme Text) Type -> State Env Type
inferTypes = \case
    TyFunc (L _ _ name) -> return $ T_Name name
    TyStd (L _ _ name) -> return $ T_Name name
    TyUserDefined (L _ _ name) -> return $ T_Name name
    TyPointer ty -> return $ T_Ptr ty
    TyConst ty -> return ty
    FunctionPrototype retTy (L _ _ name) args -> do
        let ty = T_Func retTy args
        addName name ty
        return ty
    VarDecl ty (L _ _ name) [] -> do
        addName name ty
        return ty
    x -> do
        env <- State.get
        error (show (x, pretty env))

linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \_file node act ->
        case unFix node of
            FunctionDefn{} ->
                error $ show $ flip State.execState empty $ foldFixM inferTypes node

            _ -> act
    }

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
