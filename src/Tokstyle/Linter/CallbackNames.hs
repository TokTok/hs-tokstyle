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
import           Language.Cimple             (IdentityActions, Lexeme (..),
                                              Node, NodeF (..), defaultActions,
                                              doNode, traverseAst)
import           Language.Cimple.Diagnostics (warn)


isValid :: Text -> Bool
isValid name = any (`Text.isSuffixOf` name)
    [ "callback"
    , "cb"
    , "function"
    , "handler"
    ]


linter :: IdentityActions (State [Text]) Text
linter = defaultActions
    { doNode = \file node act ->
        case unFix node of
            VarDecl (Fix (TyPointer (Fix TyFunc{}))) (L _ _ varName) _ -> do
                unless (isValid varName) $ do
                    warn file node $ "function pointer `" <> varName <> "' should end in 'callback'"
                return node

            VarDecl (Fix TyFunc{}) (L _ _ varName) _ -> do
                unless (isValid varName) $ do
                    warn file node $ "function pointer parameter `" <> varName <> "' should end in 'callback'"
                return node

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
