{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.CallocArgs (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (BinaryOp (BopMul), Lexeme (..),
                                              Node, NodeF (..))
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import qualified Tokstyle.Common             as Common


checkSize, checkNmemb :: Text -> FilePath -> Node (Lexeme Text) -> State [Text] ()
checkSize funName file size = case unFix size of
    SizeofType{} -> return ()
    _ -> warn file size $ "`size` argument in call to `" <> funName <> "` must be a sizeof expression"

checkNmemb funName file nmemb = case unFix nmemb of
    LiteralExpr{}     -> return ()
    VarExpr{}         -> return ()
    ParenExpr e       -> checkNmemb funName file e
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
        Calloc funName@"calloc" [nmemb, size] -> do
            checkNmemb funName file nmemb
            checkSize funName file size
        Calloc funName@"realloc" [_, Fix (BinaryExpr nmemb BopMul size)] -> do
            checkNmemb funName file nmemb
            checkSize funName file size
        Calloc funName@"mem_alloc" [_, size] -> do
            checkSize funName file size
        Calloc funName@"mem_valloc" [_, nmemb, size] -> do
            checkNmemb funName file nmemb
            checkSize funName file size
        Calloc funName@"mem_vrealloc" [_, _, nmemb, size] -> do
            checkNmemb funName file nmemb
            checkSize funName file size

        Calloc "calloc"       _ -> warn file node "invalid `calloc` invocation: 2 arguments expected"
        Calloc "realloc"      _ -> warn file node "invalid `realloc` invocation: 2 arguments expected"
        Calloc "mem_alloc"    _ -> warn file node "invalid `mem_alloc` invocation: 1 argument after `mem` expected"
        Calloc "mem_valloc"   _ -> warn file node "invalid `mem_valloc` invocation: 2 arguments after `mem` expected"
        Calloc "mem_vrealloc" _ -> warn file node "invalid `mem_vrealloc` invocation: 3 argument after `mem` expected"

        _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter . Common.skip
    [ "toxav/rtp.c"
    , "toxcore/list.c"
    , "toxcore/mem.c"
    ]

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("calloc-args", Text.unlines
    [ "Checks that `mem_alloc`, `mem_valloc`, and `mem_vrealloc` are used correctly:"
    , ""
    , "- The `size` argument (e.g. for `mem_alloc`, the second argument) should be a"
    , "  pure `sizeof` expression without additions or multiplications."
    , "- There should be no `sizeof` in the `nmemb` argument of a memory allocation"
    , "  call."
    , ""
    , "**Reason:** we want to avoid arbitrary computations in allocation sizes to"
    , "ensure the allocation size is exactly correct for the type of the object"
    , "being allocated."
    ]))
