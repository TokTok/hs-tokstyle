{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.MallocType (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (IdentityActions, Lexeme (..),
                                              Node, NodeF (..), defaultActions,
                                              doNode, traverseAst)
import           Language.Cimple.Diagnostics (warn)

supportedTypes :: [Text]
supportedTypes = ["uint8_t", "int16_t", "IP_ADAPTER_INFO"]

checkType :: FilePath -> Node (Lexeme Text) -> State [Text] ()
checkType file castTy = case unFix castTy of
    TyPointer (Fix (TyStd (L _ _ tyName))) | tyName `elem` supportedTypes -> return ()
    _ -> warn file castTy $
        "`malloc' should be used for builtin types only "
        <> "(e.g. `uint8_t *' or `int16_t *'); use `calloc' instead"


linter :: IdentityActions (State [Text]) Text
linter = defaultActions
    { doNode = \file node act ->
        case unFix node of
            CastExpr castTy (Fix (FunctionCall (Fix (VarExpr (L _ _ "malloc"))) _)) -> do
                checkType file castTy
                return node

            FunctionCall (Fix (VarExpr (L _ _ "malloc"))) _ -> do
                warn file node "the result of `malloc' must be cast; plain `void *' is not supported"
                return node

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
