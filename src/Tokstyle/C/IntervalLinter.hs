{-# LANGUAGE OverloadedStrings #-}
-- | What the linters built on the interval analysis share.
module Tokstyle.C.IntervalLinter
    ( analysedFunctions
    , exprDoc
    , valuesDoc
    , recordWrap
    ) where

import           Language.C.Analysis.SemRep    (FunDef)
import           Language.C.Analysis.TravMonad (Trav, getUserState)
import           Language.C.Data.Node          (NodeInfo, posOfNode)
import           Language.C.Data.Position      (isSourcePos)
import qualified Language.C.Pretty             as C
import           Language.C.Syntax.AST         (CExpr, annotation)
import           Language.Cimple.Diagnostics   (DiagnosticSpan (..))
import           Prettyprinter                 (Doc, pretty, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle)
import           Tokstyle.C.Analysis.CFG       (Node)
import           Tokstyle.C.Analysis.Interval  (Facts, Range (..), Term (..))
import           Tokstyle.C.Analysis.Program   (analysedWith)
import           Tokstyle.C.Env                (DiagnosticLevel (..), Env (..),
                                                posAndLen, recordRichError)
import           Tokstyle.C.TravUtils          (backticks)

-- | The functions of the translation unit a linter looks at, with the facts
-- before each of their nodes.
analysedFunctions :: (FunDef -> Bool) -> Trav Env [(FunDef, [(Node, Facts, [Term])])]
analysedFunctions looksAt = do
    env <- getUserState
    return [ (fd, analysedWith (program env) a) | a@(fd, _, _) <- analysed env, looksAt fd ]

exprDoc :: CExpr -> Doc a
exprDoc = backticks . pretty . show . C.pretty

valuesDoc :: Range -> Doc a
valuesDoc (Range l h _)
    | l == h = pretty l
    | otherwise = "values from" <+> pretty l <+> "to" <+> pretty h

-- | Records an error at a term, pointing at where its value wrapped around if
-- that was elsewhere.
recordWrap :: Term -> NodeInfo -> Doc AnsiStyle -> Trav Env ()
recordWrap t site msg = recordRichError ni ErrorLevel msg spans []
  where
    ni = annotation (termExpr t)
    spans =
        [ DiagnosticSpan pos len ["wraps around here"]
        | posOfNode site /= posOfNode ni, isSourcePos (posOfNode site)
        , let (pos, len) = posAndLen site ]
