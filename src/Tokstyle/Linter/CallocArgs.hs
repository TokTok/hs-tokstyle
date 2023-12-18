{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.CallocArgs (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)


checkSize, checkNmemb :: Text -> FilePath -> Node (Lexeme Text) -> State [Text] ()
checkSize funName file size = case unFix size of
    SizeofType{} -> return ()
    _ -> warn file size $ "`size` argument in call to `" <> funName <> "` must be a sizeof expression"

checkNmemb funName file nmemb = case unFix nmemb of
    LiteralExpr{} -> return ()
    VarExpr{} -> return ()
    PointerAccess e _ -> checkNmemb funName file e
    BinaryExpr l _ r -> do
        checkNmemb funName file l
        checkNmemb funName file r

    SizeofType{} ->
        warn file nmemb $ "`sizeof` should not appear in the `nmemb` argument to `" <> funName <> "`"

    _ ->
        warn file nmemb $ "invalid expression in `nmemb` argument to `" <> funName <> "`"


pattern Calloc :: Text -> [Node (Lexeme Text)] -> Node (Lexeme Text)
pattern Calloc funName args <- Fix (FunctionCall (Fix (VarExpr (L _ _ funName))) args)

linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act -> case node of
        Calloc funName@"mem_alloc" [_, size] -> do
            checkSize funName file size
        Calloc funName@"mem_valloc" [_, nmemb, size] -> do
            checkNmemb funName file nmemb
            checkSize funName file size
        Calloc funName@"mem_vrealloc" [_, _, nmemb, size] -> do
            checkNmemb funName file nmemb
            checkSize funName file size

        Calloc "mem_alloc"    _ -> warn file node "invalid `mem_alloc` invocation: 1 arguments after `mem` expected"
        Calloc "mem_valloc"   _ -> warn file node "invalid `mem_valloc` invocation: 2 arguments after `mem` expected"
        Calloc "mem_vrealloc" _ -> warn file node "invalid `mem_vrealloc` invocation: 3 argument after `mem` expected"

        _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
