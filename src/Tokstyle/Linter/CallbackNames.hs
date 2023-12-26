{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.CallbackNames (descr) where

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


allowed :: [Text]
allowed =
    [ "callback"
    , "cb"
    , "function"
    , "handler"
    ]

isValid :: Text -> Bool
isValid name = any (`Text.isSuffixOf` name) allowed


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

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("callback-names", Text.unlines
    [ "Checks for naming conventions for callbacks. Callback names should end in"
    , "`callback`, but the following list of suffixes is permitted:"
    , ""
    , Text.intercalate "\n" . map (\x -> "- `" <> x <> "`") $ allowed
    , ""
    , "**Reason:** naming conventions help quickly understand the code."
    ]))
