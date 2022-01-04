{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.TypeCheck where

import qualified Control.Monad.State.Strict as State
import           Data.Fix                   (Fix (..))
import           Data.Text                  (Text)
import           Language.Cimple            (AstActions', Lexeme, Node,
                                             NodeF (..), defaultActions',
                                             doNode, traverseAst)

linter :: AstActions' [Text]
linter = defaultActions'
    { doNode = \_file node act ->
        case node of
            -- Ignore everything inside functions, we'll type-check them later.
            Fix FunctionDefn{} ->
                return node

            -- TODO(iphydf): Implement the rest of the typecheck (draw the rest
            -- of the owl).

            _ -> act
    }

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
