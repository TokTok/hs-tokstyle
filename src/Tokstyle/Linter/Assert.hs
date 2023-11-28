{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.Assert (analyse) where

import           Control.Monad               (unless)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (Diagnostics)
import qualified Language.Cimple.Diagnostics as Diagnostics
import           Language.Cimple.Pretty      (showNode)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)


checkAssertArg :: FilePath -> Lexeme Text -> Node (Lexeme Text) -> Diagnostics ()
checkAssertArg file name expr =
    case unFix expr of
      LiteralExpr{}     -> return ()
      SizeofExpr{}      -> return ()
      SizeofType{}      -> return ()
      VarExpr{}         -> return ()
      CastExpr _ e      -> checkAssertArg file name e
      ParenExpr e       -> checkAssertArg file name e
      PointerAccess e _ -> checkAssertArg file name e
      UnaryExpr _ e     -> checkAssertArg file name e
      ArrayAccess e i   -> do
          checkAssertArg file name e
          checkAssertArg file name i
      BinaryExpr lhs _ rhs -> do
          checkAssertArg file name lhs
          checkAssertArg file name rhs
      TernaryExpr cond thenB elseB -> do
          checkAssertArg file name cond
          checkAssertArg file name thenB
          checkAssertArg file name elseB
      FunctionCall _ [] -> return ()  -- no arguments = constant function
      FunctionCall (Fix (VarExpr (L _ _ func))) args -> do
          mapM_ (checkAssertArg file name) args
          unless (func `elem` exemptions) $
              Diagnostics.warn file name $
                  "non-pure function `" <> func <> "` cannot be called inside `assert()`"
      _ -> Diagnostics.warn file name $
          "invalid expression in assert: `" <> showNode expr <> "` is not a pure function"

  where
    -- Known const/pure functions.
    exemptions =
        [ "make_family"
        , "memcmp"
        , "shared_key_is_empty"
        ]


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionCall (Fix (VarExpr name@(L _ _ "assert"))) [arg] ->
                checkAssertArg file name arg

            _ -> act
    }


analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
