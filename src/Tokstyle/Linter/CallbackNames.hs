{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.CallbackNames (analyse) where

import           Control.Monad               (unless)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)


isValid :: Text -> Bool
isValid name = any (`Text.isSuffixOf` name)
    [ "callback"
    , "cb"
    , "function"
    , "handler"
    ]


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            VarDecl (Fix (TyPointer (Fix TyFunc{}))) (L _ _ varName) _ ->
                unless (isValid varName) $
                    warn file node $ "function pointer `" <> varName <> "` should end in `callback`"

            VarDecl (Fix TyFunc{}) (L _ _ varName) _ ->
                unless (isValid varName) $
                    warn file node $ "function pointer parameter `" <> varName <> "` should end in `callback`"

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
