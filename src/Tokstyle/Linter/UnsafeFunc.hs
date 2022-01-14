{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
module Tokstyle.Linter.UnsafeFunc (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import           Language.Cimple             (IdentityActions, Lexeme (..),
                                              Node, NodeF (..), defaultActions,
                                              doNode, traverseAst)
import           Language.Cimple.Diagnostics (warn)

forbidden :: [(Text, (Text, Maybe Text))]
forbidden =
    [ ("atexit"  , ("creates global state that should be avoided"            , Nothing))
    , ("atof"    , ("does not perform error checking"                        , Just "strtod"))
    , ("atoi"    , ("does not perform error checking"                        , Just "strtol"))
    , ("atoll"   , ("does not perform error checking"                        , Just "strtoll"))
    , ("atol"    , ("does not perform error checking"                        , Just "strtol"))
    , ("gets"    , ("performs unbounded writes to buffers"                   , Just "fgets"))
    , ("sprintf" , ("has no way of bounding the number of characters written", Just "snprintf"))
    , ("strerror", ("is not thread safe"                                     , Just "strerror_r or net_new_strerror"))
    , ("strcat"  , ("has no way of bounding the number of characters written", Just "snprintf"))
    , ("strcpy"  , ("has no way of bounding the number of characters written", Just "snprintf or strlen and memcpy"))
    , ("strncpy" , ("may not null-terminate the target string"               , Just "snprintf or strlen and memcpy"))
    , ("strdup"  , ("is non-portable"                                        , Just "malloc followed by memcpy"))
    , ("strtok"  , ("is not thread-safe"                                     , Nothing))
    , ("vsprintf", ("has no way of bounding the number of characters written", Just "vsnprintf"))
    ]

checkName :: Text -> Maybe (Text, (Text, Maybe Text))
checkName name = (name,) <$> lookup name forbidden

linter :: IdentityActions (State [Text]) Text
linter = defaultActions
    { doNode = \file node act ->
        case unFix node of
            FunctionCall (Fix (VarExpr (L _ _ (checkName -> Just (name, (msg, replacement)))))) _ -> do
                warn file node $ "function `" <> name <> "' should not be used, because it " <> msg
                    <> fromMaybe "" ((\r -> "; use " <> r <> " instead") <$> replacement)
                return node

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
