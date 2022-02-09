{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.MissingNonNull (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (Lexeme (..), Node, NodeF (..),
                                              Scope (..), lexemeText)
import           Language.Cimple.Diagnostics (HasDiagnostics (..), warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           System.FilePath             (takeFileName)
import           Tokstyle.Common             (isPointer)


data Linter = Linter
    { diags   :: [Text]
    , statics :: [(Text, Lexeme Text)]
    }

empty :: Linter
empty = Linter [] []

instance HasDiagnostics Linter where
    addDiagnostic diag l@Linter{diags} = l{diags = addDiagnostic diag diags}


linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            NonNull _ _ (Fix (FunctionDecl Static (Fix (FunctionPrototype _ name _)))) ->
                addStatic name
            NonNull _ _ (Fix (FunctionDefn Static (Fix (FunctionPrototype _ name _)) _)) ->
                addStatic name

            NonNull{} -> return ()

            FunctionDecl Global (Fix (FunctionPrototype _ name args)) | any isPointer args ->
               warn file name "global function has no non_null or nullable annotation"

            FunctionDefn Static (Fix (FunctionPrototype _ name args)) _ | any isPointer args -> do
                Linter{statics} <- State.get
                case lookup (lexemeText name) statics of
                    Just{}  -> return ()
                    Nothing -> warn file name "static function must have nullability annotation"

            _ -> act
    }
  where
    addStatic :: Lexeme Text -> State Linter ()
    addStatic name = State.modify $ \l@Linter{statics} -> l{statics = (lexemeText name, name) : statics}

exemptions :: [FilePath]
exemptions =
    -- toxav is exempt for now, until we have the refactoring merged.
    [ "audio.c"
    , "audio.h"
    , "bwcontroller.c"
    , "bwcontroller.h"
    , "groupav.c"
    , "groupav.h"
    , "msi.c"
    , "msi.h"
    , "ring_buffer.c"
    , "ring_buffer.h"
    , "rtp.c"
    , "rtp.h"
    , "toxav.c"
    , "video.c"
    , "video.h"

    -- public(ish) API headers are exempt.
    , "tox.h"
    , "tox_dispatch.h"
    , "tox_events.h"
    , "tox_private.h"
    , "toxav.h"
    , "toxencryptsave.h"
    ]

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse tu@(path, _)
  | takeFileName path `elem` exemptions = []
  | otherwise = reverse . diags . flip State.execState empty . traverseAst linter $ tu
