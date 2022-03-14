{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.CallocType (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (BinaryOp (..), Lexeme (..), Node,
                                              NodeF (..), removeSloc)
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.Pretty      (showNode)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)


checkTypes :: FilePath -> Node (Lexeme Text) -> Node (Lexeme Text) -> State [Text] ()
checkTypes file castTy sizeofTy = case unFix castTy of
    TyPointer (Fix (TyStd (L _ _ tyName))) | not ("pthread_" `Text.isPrefixOf` tyName) ->
        warn file castTy $
            "`calloc` should not be used for `" <> showNode castTy
            <> "`; use `malloc` instead"
    TyPointer ty1 | removeSloc ty1 == removeSloc sizeofTy -> return ()
    _ -> warn file castTy $
        "`calloc` result is cast to `" <> showNode castTy
        <> "` but allocated type is `" <> showNode sizeofTy <> "`"


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            CastExpr castTy (Fix (FunctionCall (Fix (VarExpr (L _ _ "calloc"))) [_, Fix (BinaryExpr (Fix (SizeofType sizeofTy)) BopPlus _)])) -> do
                checkTypes file castTy sizeofTy

            CastExpr castTy (Fix (FunctionCall (Fix (VarExpr (L _ _ "calloc"))) [_, Fix (SizeofType sizeofTy)])) -> do
                checkTypes file castTy sizeofTy

            FunctionCall (Fix (VarExpr (L _ _ "calloc"))) _ -> do
                warn file node "the result of `calloc` must be cast to its member type"

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
