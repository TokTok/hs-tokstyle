{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.MissingNonNull (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
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


hasPerParamAnnotation :: Node (Lexeme Text) -> Bool
hasPerParamAnnotation (Fix (NonNullParam _))  = True
hasPerParamAnnotation (Fix (NullableParam _)) = True
hasPerParamAnnotation _                       = False


linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            NonNull _ _ (Fix (FunctionDecl Static (Fix (FunctionPrototype _ name _)))) ->
                addStatic name
            NonNull _ _ (Fix (FunctionDefn Static (Fix (FunctionPrototype _ name _)) _)) ->
                addStatic name

            NonNull{} -> return ()

            FunctionDecl Static (Fix (FunctionPrototype _ name args))
                | any hasPerParamAnnotation args -> addStatic name
            FunctionDecl _ (Fix (FunctionPrototype _ _ args))
                | any hasPerParamAnnotation args -> return ()

            FunctionDecl Global (Fix (FunctionPrototype _ name args)) | any isPointer args ->
               warn file name "global function has no non_null or nullable annotation"

            FunctionDefn _ (Fix (FunctionPrototype _ _ args)) _ | any hasPerParamAnnotation args ->
                return ()

            FunctionDecl Static (Fix (FunctionPrototype _ name args))
                | any isPointer args && not (any hasPerParamAnnotation args) ->
                    warn file name "static function must have nullability annotation"
            FunctionDefn Static (Fix (FunctionPrototype _ name args)) _
                | any hasPerParamAnnotation args -> addStatic name
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
    , "tox_options.h"
    , "tox_private.h"
    , "toxav.h"
    , "toxencryptsave.h"

    -- cmp is exempt.
    , "cmp.c"
    , "cmp.h"
    ]

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse tu@(path, _)
  | takeFileName path `elem` exemptions = []
  | otherwise = reverse . diags . flip State.execState empty . traverseAst linter $ tu

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("missing-non-null", Text.unlines
    [ "Checks that all function declarations have nullability annotations (`non_null`"
    , "and/or `nullable`)."
    , ""
    , "**Reason:** in TokTok code, we want to be explicit about which pointer"
    , "parameters can be passed a NULL pointer. This forces the developer to think"
    , "about nullability and allows static analysers to ensure that all possibly-NULL"
    , "pointers are checked before being dereferenced or passed to a non-NULL parameter."
    ]))
