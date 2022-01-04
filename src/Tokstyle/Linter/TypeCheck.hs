{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.TypeCheck where

import qualified Control.Monad.State.Strict as State
import           Data.Fix                   (Fix (..))
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import           Language.Cimple            (AstActions', Lexeme, Node,
                                             NodeF (..), defaultActions',
                                             doNode, traverseAst)

newtype Env = Env
    { types :: Types
    }
    deriving (Show)

type Types = Map Text Type
data Type
    = Aggregate Tag Members
    | Builtin Text
    deriving (Show)

data Tag
    = StructTag
    | UnionTag
    deriving (Show)

type Members = Map Text Member
data Member = Member Text Type
    deriving (Show)

empty :: Env
empty = Env Map.empty

getTypes :: AstActions' Env
getTypes = defaultActions'
    { doNode = \_file node act ->
        case unFix node of
            Typedef{} -> do
                --_ <- error $ show $ node
                return node

            -- TODO(iphydf): Implement the rest of the typecheck (draw the rest
            -- of the owl).

            -- Ignore everything inside functions, we'll type-check them later.
            FunctionDefn{} -> return node
            _ -> act
    }

linter :: Env -> AstActions' [Text]
linter _ = defaultActions'
    { doNode = \_file node act ->
        case unFix node of
            FunctionDefn{} -> return node
            _              -> act
    }

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse tus =
    reverse
    . flip State.execState []
    . traverseAst (linter env)
    $ tus
  where
    env =
        flip State.execState empty
        . traverseAst getTypes
        $ tus
