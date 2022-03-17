{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.BooleanReturn where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..), foldFix)
import qualified Data.List                   as List
import qualified Data.Maybe                  as Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), LiteralType (..),
                                              Node, NodeF (..), UnaryOp (..),
                                              lexemeText)
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)


data Value a
    = Const a
    | NonConst
    | Returned (Value a)
    deriving (Show, Functor)

emptyIfAnyNonConst :: [Value a] -> [Value a]
emptyIfAnyNonConst values =
    if any isNonConst values then [] else values
  where
    isNonConst (Returned NonConst) = True
    isNonConst _                   = False

returnedConstValues :: Node (Lexeme Text) -> [Text]
returnedConstValues = List.sort . List.nub . Maybe.mapMaybe returnedConst . emptyIfAnyNonConst . foldFix go
  where
    go (LiteralExpr Int (L _ _ value)) = [Const value]
    go (Return (Just value))           = map Returned value
    go (UnaryExpr op e)                = map (fmap (uopToken op <>)) e

    go n                               = map toNonConst $ foldr (++) [NonConst] n

    returnedConst (Returned (Const value)) = Just value
    returnedConst _                        = Nothing

    toNonConst Const{} = NonConst
    toNonConst v       = v

    uopToken UopMinus = "-"
    uopToken op       = error (show op)


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionDefn _ (Fix (FunctionPrototype _ name _)) _ | isEligible name ->
                case returnedConstValues node of
                  [v1, v2] -> warn file name $
                      "function `" <> lexemeText name <> "` only ever returns two values `"
                      <> v1 <> "` and `" <> v2 <> "`; it can return `bool`"
                  _ -> return ()

            _ -> act
    }
  where
    -- Ignore event handlers named something with "handle" in the name.
    isEligible = not . ("handle" `Text.isInfixOf`) . lexemeText

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
