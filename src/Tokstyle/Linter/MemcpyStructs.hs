{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.MemcpyStructs (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (IdentityActions, Lexeme (..),
                                              Node, NodeF (..), defaultActions,
                                              doNode, traverseAst)
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.Pretty      (showNode)

exemptions :: [Text]
exemptions =
    [ "IP_Port"
    , "IP4"
    , "IP6"
    ]

checkSize :: Text -> Text -> FilePath -> Node (Lexeme Text) -> State [Text] ()
checkSize fname instead file size = case unFix size of
    SizeofType ty@(Fix (TyUserDefined (L _ _ name))) | not $ name `elem` exemptions ->
        warn file size $
            "`" <> fname <> "` should not be used for structs like `"
            <> showNode ty <> "` - use " <> instead <> " instead"

    _ -> return ()


linter :: IdentityActions (State [Text]) Text
linter = defaultActions
    { doNode = \file node act ->
        case unFix node of
            FunctionCall (Fix (VarExpr (L _ _ "memset"))) [_, _, size] -> do
                checkSize "memset" "`(Type) {0}`" file size
                return node
            FunctionCall (Fix (VarExpr (L _ _ "memcpy"))) [_, _, size] -> do
                checkSize "memcpy" "assignment" file size
                return node

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
