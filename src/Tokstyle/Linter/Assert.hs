{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.Assert (descr) where

import           Control.Monad               (unless)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
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
      MemberAccess e _  -> checkAssertArg file name e
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

-- Known const/pure functions.
exemptions :: [Text]
exemptions =
    [ "make_family"
    , "memcmp"
    , "shared_key_is_empty"
    , "tox_events_get_size"
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

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("assert", Text.unlines
    [ "Checks whether `assert` is side-effect-free. Only pure expressions"
    , "(no function calls, no assignments) and an allowlist of exemptions are permitted"
    , "within `assert`. The current list of exemptions is:"
    , ""
    , Text.intercalate "\n" . map (\x -> "- `" <> x <> "`") $ exemptions
    , ""
    , "**Reason:** `assert` is compiled out in `NDEBUG` builds, so should not influence"
    , "logic of the code in debug modes to avoid different behaviours in different"
    , "compilation modes."
    ]))
