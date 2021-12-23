{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.VarUnusedInScope (analyse) where

import           Control.Monad.State.Lazy    (State)
import qualified Control.Monad.State.Lazy    as State
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node (..))
import           Language.Cimple.Diagnostics (HasDiagnostics (..), at, warn)
import           Language.Cimple.TraverseAst (AstActions (..), defaultActions,
                                              traverseAst)


data Linter = Linter
    { diags :: [Text]
    , scope :: [Text]
    , stack :: [[Text]]
    }

empty :: Linter
empty = Linter [] [] []

instance HasDiagnostics Linter where
    addDiagnostic diag l@Linter{diags} = l{diags = addDiagnostic diag diags}


pushScope :: State Linter ()
pushScope = State.modify $ \l@Linter{scope, stack} -> l
    { scope = []
    , stack = scope:stack
    }

popScope :: State Linter [Text]
popScope = do
    l@Linter{stack} <- State.get
    case stack of
        []         -> return []
        scope:rest -> State.put l{stack = rest} >> return scope


linter :: AstActions (State Linter) () Text
linter = defaultActions
    { doNode = \file node act ->
        case node of
            FunctionDefn _ (FunctionPrototype _ _ _) _ -> do
                pushScope
                r <- act
                _ <- popScope
                return r

            ForStmt{} -> do
                warn file (at node) $ Text.pack $ show node
                act

            _ -> act
    }

analyse :: FilePath -> [Node () (Lexeme Text)] -> [Text]
analyse = curry $ reverse . diags . flip State.execState empty . traverseAst linter
