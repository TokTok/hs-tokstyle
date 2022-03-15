{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.DocComments (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (Lexeme (..), LexemeClass (..),
                                              Node, NodeF (..), removeSloc)
import           Language.Cimple.Diagnostics (HasDiagnostics (..), warn)
import           Language.Cimple.Pretty      (ppTranslationUnit, render)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)


data Linter = Linter
    { diags :: [Text]
    , docs  :: [(Text, (FilePath, Node (Lexeme Text)))]
    }

empty :: Linter
empty = Linter [] []

instance HasDiagnostics Linter where
    addDiagnostic diag l@Linter{diags} = l{diags = addDiagnostic diag diags}


-- | Extract the name of a function, possibly inside an attribute node.
--
-- Non-function nodes result in 'Nothing'.
functionName :: Show a => Node (Lexeme a) -> Maybe a
functionName (Fix (FunctionPrototype _ (L _ IdVar name) _)) = Just name
functionName (Fix (FunctionDecl _ proto  )) = functionName proto
functionName (Fix (FunctionDefn _ proto _)) = functionName proto
functionName (Fix (AttrPrintf _ _ entity))  = functionName entity
functionName (Fix (NonNull _ _ entity))     = functionName entity
functionName _                              = Nothing


linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            Commented doc entity -> do
                case functionName entity of
                  Nothing   -> return ()
                  Just name -> checkCommentEquals file doc name
                act

            FunctionDefn{} -> return ()
            _ -> act
    }
  where
    checkCommentEquals file doc fname = do
        l@Linter{docs} <- State.get
        case lookup fname docs of
            Nothing -> State.put l{docs = (fname, (file, doc)):docs}
            Just (_, doc') | removeSloc doc == removeSloc doc' -> return ()
            Just (file', doc') -> do
                warn file doc $ "comment on definition of `" <> fname
                    <> "` does not match declaration:\n"
                    <> render (ppTranslationUnit [doc])
                warn file' doc' $ "mismatching comment found here:\n"
                    <> render (ppTranslationUnit [doc'])

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse = reverse . diags . flip State.execState empty . traverseAst linter . reverse
