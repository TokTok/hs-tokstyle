{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
module Tokstyle.Linter.UnsafeFunc (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (IdentityActions, Lexeme (..),
                                              Node, NodeF (..), defaultActions,
                                              doNode, traverseAst)
import           Language.Cimple.Diagnostics (warn)

forbidden :: [(Text, Text)]
forbidden =
    [ ("atexit", "this creates global state that should be avoided")
    , ("atoi", "use `strtol' instead")
    , ("sprintf", "use `snprintf' instead")
    , ("strcat", "use `snprintf' instead")
    , ("strchr", "use `memchr' instead")
    , ("strcmp", "use `memcmp' instead")
    , ("strcpy", "use `snprintf' instead")
    , ("strdup", "use `malloc' followed by `memcpy' instead")
    ]

checkName :: Text -> Maybe (Text, Text)
checkName name = (name,) <$> lookup name forbidden

linter :: IdentityActions (State [Text]) Text
linter = defaultActions
    { doNode = \file node act ->
        case unFix node of
            FunctionCall (Fix (VarExpr (L _ _ (checkName -> Just (name, msg))))) _ -> do
                warn file node $ "function `" <> name <> "' should not be used; " <> msg
                return node

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
