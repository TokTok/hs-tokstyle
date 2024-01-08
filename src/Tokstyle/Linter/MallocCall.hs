{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.MallocCall (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              doNodes, traverseAst)
import qualified Tokstyle.Common             as Common
import           Tokstyle.Common             ((>+>))


mallocFuncs :: [Text]
mallocFuncs =
    [ "mem_balloc"
    , "mem_alloc"
    , "mem_valloc"
    , "mem_vrealloc"
    , "malloc"
    , "calloc"
    , "realloc"
    ]

pattern FunCall, FunctionCast :: text -> Node (Lexeme text)
pattern FunCall      name <- Fix (FunctionCall (Fix (VarExpr (L _ _ name))) _)
pattern FunctionCast name <- Fix (CastExpr _ (FunCall name))

pattern MallocVarDecl :: text -> Node (Lexeme text) -> Node (Lexeme text)
pattern MallocVarDecl decl initialiser <- Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ decl) _)) (Just initialiser))

pattern MallocReturn :: Node lexeme -> Node lexeme
pattern MallocReturn initialiser <- Fix (Return (Just initialiser))

lintAssign :: AstActions (State [Text]) Text
lintAssign = astActions
    { doNode = \file node act -> case node of
        MallocVarDecl _ (FunctionCast name) | name `elem` mallocFuncs -> return ()
        MallocReturn    (FunctionCast name) | name `elem` mallocFuncs -> return ()
        -- We check in -Wcalloc-type that casts are done correctly. This avoids
        -- double-warning on non-cast malloc calls.
        MallocVarDecl _ (FunCall      name) | name `elem` mallocFuncs -> return ()
        MallocReturn    (FunCall      name) | name `elem` mallocFuncs -> return ()

        FunCall name | name `elem` mallocFuncs ->
            warn file node $ "allocations using `" <> name
                <> "` must first be assigned to a local variable or "
                <> "returned directly"

        _ -> act
    }

pattern NullCheck :: Text -> Node (Lexeme Text)
pattern NullCheck ref <-
    Fix (IfStmt
        (Fix (BinaryExpr (Fix (VarExpr (L _ _ ref))) _ (Fix (VarExpr (L _ _ "nullptr")))))
        (Fix (CompoundStmt _)) _)

lintCheck :: AstActions (State [Text]) Text
lintCheck = astActions
    { doNodes = \file nodes act -> case nodes of
        (MallocVarDecl decl FunctionCast{}:ss@(NullCheck ref:_)) | decl == ref ->
            traverseAst lintCheck (file, ss)
        (MallocVarDecl decl (FunctionCast name):s:_) ->
            warn file s $ "`" <> decl <> "`, assigned from `" <> name
                <> "` must immediately be checked against `nullptr`"

        _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . (traverseAst lintCheck >+> traverseAst lintAssign)
    -- TODO(iphydf): Refactor after the toxav PR.
    . Common.skip
        [ "toxav/audio.c"
        , "toxav/groupav.c"
        , "toxav/msi.c"
        , "toxav/ring_buffer.c"
        , "toxav/rtp.c"
        , "toxav/toxav.c"
        , "toxav/video.c"
        ]

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("malloc-call", Text.unlines
    [ "Checks that allocation functions like `mem_balloc` are always first assigned to"
    , "a local variable. The exception is in a return statement, e.g. in simple typed"
    , "allocation functions like `logger_new()`. If the allocation is stored in a local"
    , "variable, that variable must immediately be checked against `nullptr` before"
    , "doing anything else."
    , ""
    , "Invalid code:"
    , ""
    , "```c"
    , "ob->mem = (My_Struct *)mem_alloc(mem, sizeof(My_Struct));"
    , "```"
    , ""
    , "Valid code:"
    , ""
    , "```c"
    , "My_Struct *tmp = (My_Struct *)mem_alloc(mem, sizeof(My_Struct))"
    , "if (tmp == nullptr) {"
    , "  return false;"
    , "}"
    , "ob->mem = tmp;"
    , "```"
    , ""
    , "**Reason:** This avoids accidentally putting `nullptr` into a location without"
    , "checking first. Putting `nullptr` somewhere may be ok, but we must do it"
    , "intentionally."
    ]))
