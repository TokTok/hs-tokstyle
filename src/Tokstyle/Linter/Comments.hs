{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.Comments (analyse) where

import           Control.Monad               (when)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (CommentF (..), Lexeme (..), Node,
                                              NodeF (..), lexemeText)
import           Language.Cimple.Diagnostics (Diagnostics, warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doComment,
                                              doNode, traverseAst)


isGendered :: Text -> Bool
isGendered = flip elem
    [ "she"
    , "her"
    , "herself"
    , "he"
    , "him"
    , "his"
    , "himself"
    ]

checkCommentLexeme :: FilePath -> Lexeme Text -> Diagnostics ()
checkCommentLexeme file w =
    when (isGendered $ lexemeText w) $
        warn file w $ "inappropriately gendered pronoun: " <> lexemeText w

linter :: AstActions (State [Text]) Text
linter = astActions
    { doComment = \file comment act ->
        case unFix comment of
            DocWord w -> checkCommentLexeme file w
            _         -> act
    , doNode = \file node act ->
        case unFix node of
            Comment _ _ ws _ -> mapM_ (checkCommentLexeme file) ws
            _                -> act

    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
