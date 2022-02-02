{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.CallocType (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (BinaryOp (..), IdentityActions,
                                              Lexeme (..), Node, NodeF (..),
                                              defaultActions, doNode,
                                              removeSloc, traverseAst)
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.Pretty      (showNode)


checkTypes :: FilePath -> Node (Lexeme Text) -> Node (Lexeme Text) -> State [Text] ()
checkTypes file castTy sizeofTy = case unFix castTy of
    TyPointer ty1 | removeSloc ty1 == removeSloc sizeofTy -> return ()
    _ -> warn file castTy $
        "`calloc` result is cast to `" <> showNode castTy
        <> "` but allocated type is `" <> showNode sizeofTy <> "`"


linter :: IdentityActions (State [Text]) Text
linter = defaultActions
    { doNode = \file node act ->
        case unFix node of
            CastExpr castTy (Fix (FunctionCall (Fix (VarExpr (L _ _ "calloc"))) [_, Fix (BinaryExpr (Fix (SizeofType sizeofTy)) BopPlus _)])) -> do
                checkTypes file castTy sizeofTy
                return node

            CastExpr castTy (Fix (FunctionCall (Fix (VarExpr (L _ _ "calloc"))) [_, Fix (SizeofType sizeofTy)])) -> do
                checkTypes file castTy sizeofTy
                return node

            FunctionCall (Fix (VarExpr (L _ _ "calloc"))) _ -> do
                warn file node "the result of `calloc` must be cast to its member type"
                return node

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
