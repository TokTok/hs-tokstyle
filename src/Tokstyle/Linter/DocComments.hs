{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.DocComments (descr) where

import           Control.Monad               (forM_)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (CommentStyle (..), Lexeme (..),
                                              Node, NodeF (..))
import           Language.Cimple.Diagnostics (HasDiagnostics (..), warn)
import           Language.Cimple.Pretty      (ppTranslationUnit, render)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Tokstyle.Common             (functionName, semEq)


data Linter = Linter
    { diags :: [Text]
    , docs  :: [(Text, (FilePath, Node (Lexeme Text)))]
    }

empty :: Linter
empty = Linter [] []

instance HasDiagnostics Linter where
    addDiagnostic diag l@Linter{diags} = l{diags = addDiagnostic diag diags}


linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            Commented doc entity -> do
                forM_ (functionName entity) $
                    checkCommentEquals file doc
                act

            FunctionDefn{} -> return ()
            _ -> act
    }
  where
    checkCommentEquals file doc fname = do
        l@Linter{docs} <- State.get
        case lookup fname docs of
            Nothing -> State.put l{docs = (fname, (file, doc)):docs}
            Just (_, doc') | semEq doc doc' -> return ()
            Just (file', doc') -> do
                warn file doc $ "comment on definition of `" <> fname
                    <> "` does not match declaration:\n"
                    <> render (ppTranslationUnit [doc])
                warn file' doc' $ "mismatching comment found here:\n"
                    <> render (ppTranslationUnit [doc'])

associateComments :: [Node (Lexeme Text)] -> [Node (Lexeme Text)]
associateComments [] = []
associateComments (doc@(Fix c) : nextNode : rest)
    | isFunc (unFix nextNode) && isDocComment c =
        Fix (Commented doc nextNode) : associateComments rest
  where
    isFunc FunctionDecl{} = True
    isFunc FunctionDefn{} = True
    isFunc _              = False
    isDocComment (Comment Doxygen _ _ _) = True
    isDocComment _                       = False
associateComments (x:xs) = x : associateComments xs

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse files =
    let processedFiles = map (fmap associateComments) files
    in reverse . diags . flip State.execState empty . traverseAst linter . reverse $ processedFiles

descr :: ([(FilePath, [Node (Lexeme Text)])] -> [Text], (Text, Text))
descr = (analyse, ("doc-comments", Text.unlines
    [ "Checks that doc comments on function definitions match the ones on their"
    , "corresponding declarations."
    , ""
    , "**Reason:** ideally, documentation should be only in one place, but if it is"
    , "duplicated, it should not be different."
    ]))
