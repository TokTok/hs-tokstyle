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

forbidden :: [(Text, Maybe Text)]
forbidden =
    [ ("atexit", Nothing)
    , ("atoi", Just "use `strtol' instead")
    , ("strcpy", Just "use `snprintf' instead")
    ]

checkName :: Text -> Maybe (Text, Maybe Text)
checkName name = (name,) <$> lookup name forbidden

linter :: IdentityActions (State [Text]) Text
linter = defaultActions
    { doNode = \file node act ->
        case unFix node of
            FunctionCall (Fix (VarExpr (L _ _ (checkName -> Just (name, msg))))) _ -> do
                warn file node $ "function `" <> name <> "' should not be used" <> fromMaybe "" (fmap ("; " <>) msg)
                return node

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
