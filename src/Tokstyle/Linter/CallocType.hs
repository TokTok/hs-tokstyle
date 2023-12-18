{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.CallocType (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.Pretty      (showNode)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Tokstyle.Common             (semEq)


checkTypes :: Text -> FilePath -> Node (Lexeme Text) -> Node (Lexeme Text) -> State [Text] ()
checkTypes funName file castTy sizeofTy = case unFix castTy of
    TyPointer (Fix (TyStd (L _ _ tyName))) | not ("pthread_" `Text.isPrefixOf` tyName) ->
        warn file castTy $
            "`" <> funName <> "` should not be used for `" <> showNode castTy
            <> "`; use `mem_balloc` instead"
    TyPointer ty1 | ty1 `semEq` sizeofTy -> return ()
    _ -> warn file castTy $
        "`" <> funName <> "` result is cast to `" <> showNode castTy
        <> "` but allocated type is `" <> showNode sizeofTy <> "`"


pattern Calloc :: Text -> [Node (Lexeme Text)] -> Node (Lexeme Text)
pattern Calloc funName args <- Fix (FunctionCall (Fix (VarExpr (L _ _ funName))) args)

isCalloc :: Text -> Bool
isCalloc "mem_alloc"    = True
isCalloc "mem_valloc"   = True
isCalloc "mem_vrealloc" = True
isCalloc _              = False

linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act -> case node of
        Fix (CastExpr castTy (Calloc funName@"mem_alloc" [_, Fix (SizeofType sizeofTy)])) ->
            checkTypes funName file castTy sizeofTy
        Fix (CastExpr castTy (Calloc funName@"mem_valloc" [_, _, Fix (SizeofType sizeofTy)])) ->
            checkTypes funName file castTy sizeofTy
        Fix (CastExpr castTy (Calloc funName@"mem_vrealloc" [_, _, _, Fix (SizeofType sizeofTy)])) ->
            checkTypes funName file castTy sizeofTy

        Calloc funName _ | isCalloc funName ->
            warn file node $ "the result of `" <> funName <> "` must be cast to its member type"

        _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
