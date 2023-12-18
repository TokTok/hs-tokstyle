{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.MallocType (analyse) where

import           Control.Monad               (unless)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (BinaryOp (..), Lexeme (..), Node,
                                              NodeF (..))
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.Pretty      (showNode)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Tokstyle.Common             (semEq)

supportedTypes :: [Text]
supportedTypes = ["char", "uint8_t", "int16_t"]

isByteSize :: Node (Lexeme Text) -> Bool
isByteSize ty = case unFix ty of
    TyStd (L _ _ "char")    -> True
    TyStd (L _ _ "int8_t")  -> True
    TyStd (L _ _ "uint8_t") -> True
    _                       -> False

checkType :: FilePath -> Node (Lexeme Text) -> State [Text] ()
checkType file castTy = case unFix castTy of
    TyPointer (Fix (TyStd (L _ _ tyName))) | tyName `elem` supportedTypes -> return ()
    _ -> warn file castTy $
        "`mem_balloc` should be used for builtin types only "
        <> "(e.g. `uint8_t *` or `int16_t *`); use `calloc` instead"

checkSize :: FilePath -> Node (Lexeme Text) -> Node (Lexeme Text) -> State [Text] ()
checkSize file castTy@(Fix (TyPointer objTy)) size = case unFix size of
    BinaryExpr _ BopMul r -> checkSize file castTy r
    SizeofType sizeTy ->
        unless (sizeTy `semEq` objTy) $
            warn file size $ "`size` argument in call to `mem_balloc` indicates "
                <> "creation of an array with element type `" <> showNode sizeTy <> "`, "
                <> "but result is cast to `" <> showNode castTy <> "`"
    _ ->
        unless (isByteSize objTy) $
            warn file size "`mem_balloc` result must be cast to a byte-sized type if `sizeof` is omitted"
checkSize file castTy _ =
    warn file castTy "`mem_balloc` result must be cast to a pointer type"


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            -- Windows API weirdness: ignore completely.
            CastExpr (Fix (TyPointer (Fix (TyStd (L _ _ "IP_ADAPTER_INFO"))))) _ -> return ()

            CastExpr castTy (Fix (FunctionCall (Fix (VarExpr (L _ _ "mem_balloc"))) [_, size])) -> do
                checkType file castTy
                checkSize file castTy size

            FunctionCall (Fix (VarExpr (L _ _ "mem_balloc"))) _ ->
                warn file node "the result of `mem_balloc` must be cast; plain `void *` is not supported"

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
