{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
module Tokstyle.Linter.UnsafeFunc (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)

forbidden :: [(Text, (Text, Maybe Text))]
forbidden =
    [ ("atexit"  , ("creates global state that should be avoided"            , Nothing))
    , ("atof"    , ("does not perform error checking"                        , Just "`strtod`"))
    , ("atoi"    , ("does not perform error checking"                        , Just "`strtol`"))
    , ("atoll"   , ("does not perform error checking"                        , Just "`strtoll`"))
    , ("atol"    , ("does not perform error checking"                        , Just "`strtol`"))
    , ("gets"    , ("performs unbounded writes to buffers"                   , Just "`fgets`"))
    , ("sprintf" , ("has no way of bounding the number of characters written", Just "`snprintf`"))
    , ("strerror", ("is not thread safe"                                     , Just "`strerror_r` or `net_new_strerror`"))
    , ("strcat"  , ("has no way of bounding the number of characters written", Just "`snprintf`"))
    , ("strcpy"  , ("has no way of bounding the number of characters written", Just "`snprintf` or `strlen` and `memcpy`"))
    , ("strncpy" , ("may not null-terminate the target string"               , Just "`snprintf` or `strlen` and `memcpy`"))
    , ("strdup"  , ("is non-portable"                                        , Just "`mem_balloc` followed by `memcpy`"))
    , ("strtok"  , ("is not thread-safe"                                     , Nothing))
    , ("vsprintf", ("has no way of bounding the number of characters written", Just "`vsnprintf`"))
    ]

checkName :: Text -> Maybe (Text, (Text, Maybe Text))
checkName name = (name,) <$> lookup name forbidden

linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionCall (Fix (VarExpr (L _ _ (checkName -> Just (name, (msg, replacement)))))) _ ->
                warn file node $ "function `" <> name <> "` should not be used, because it " <> msg
                    <> maybe "" (\r -> "; use " <> r <> " instead") replacement

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("unsafe-func", Text.unlines
    [ "Explicitly forbids the use of some C functions considered unsafe:"
    , ""
    , Text.intercalate "\n" . map mkWhy $ forbidden
    , ""
    , "**Reason:** ."
    ]))
  where
    mkWhy (name, (msg, replacement)) =
        "- `" <> name <> "`, because it " <> msg <> "."
        <> maybe "" (\r -> "\n  " <> r <> " should be used, instead.") replacement
