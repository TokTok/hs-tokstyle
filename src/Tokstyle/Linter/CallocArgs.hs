{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.CallocArgs (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (BinaryOp (..), 
                                              Lexeme (..), LiteralType (..),
                                              Node, NodeF (..))
import           Language.Cimple.TraverseAst             (AstActions,
                                              astActions,
                                              doNode, traverseAst)
import           Language.Cimple.Diagnostics (warn)


checkSize, checkNmemb, checkFlexibleCalloc :: FilePath -> Node (Lexeme Text) -> State [Text] ()
checkSize file size = case unFix size of
    SizeofType{} -> return ()
    _ -> warn file size $ "`size` argument in call to `calloc` must be a sizeof expression"

checkNmemb file nmemb = case unFix nmemb of
    LiteralExpr{} -> return ()
    VarExpr{} -> return ()
    PointerAccess e _ -> checkNmemb file e
    BinaryExpr l _ r -> do
        checkNmemb file l
        checkNmemb file r

    SizeofType{} ->
        warn file nmemb $ "`sizeof` should not appear in the first argument to `calloc`"

    _ ->
        warn file nmemb $ "invalid expression in `nmemb` argument to `calloc`"

checkFlexibleCalloc file nmemb = case unFix nmemb of
    LiteralExpr Int (L _ _ "1") -> return ()
    _ -> warn file nmemb $ "in call to `calloc`: `nmemb` must be 1 if `size` is not a pure sizeof expression"


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            -- Special support for flexible array member. We should get rid of
            -- this, but for now it's allowed.
            FunctionCall (Fix (VarExpr (L _ _ "calloc"))) [nmemb, Fix (BinaryExpr (Fix SizeofType{}) BopPlus _)] -> do
                checkFlexibleCalloc file nmemb

            FunctionCall (Fix (VarExpr (L _ _ "calloc"))) [nmemb, size] -> do
                checkNmemb file nmemb
                checkSize file size

            FunctionCall (Fix (VarExpr (L _ _ "calloc"))) _ -> do
                warn file node $ "invalid `calloc` invocation: 2 arguments expected"

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
