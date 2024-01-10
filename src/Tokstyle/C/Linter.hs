{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter
    ( analyse
    , allWarnings
    ) where

import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Language.C.Analysis.AstAnalysis  (analyseAST)
import           Language.C.Analysis.SemRep       (GlobalDecls)
import           Language.C.Analysis.TravMonad    (CLanguage (..), Trav,
                                                   TravOptions (..),
                                                   modifyOptions, runTrav)
import           Language.C.Syntax.AST            (CTranslUnit)
import           Tokstyle.C.Env                   (Env, defaultEnv)

import qualified Tokstyle.C.Linter.BoolConversion as BoolConversion
import qualified Tokstyle.C.Linter.Cast           as Cast
import qualified Tokstyle.C.Linter.Conversion     as Conversion
import qualified Tokstyle.C.Linter.Memset         as Memset
import qualified Tokstyle.C.Linter.SizeArg        as SizeArg
import qualified Tokstyle.C.Linter.Sizeof         as Sizeof
import qualified Tokstyle.C.Linter.VoidCall       as VoidCall


linters :: [(Text, GlobalDecls -> Trav Env ())]
linters =
    [ ("bool-conversion"    , BoolConversion.analyse   )
    , ("cast"               , Cast.analyse             )
    , ("conversion"         , Conversion.analyse       )
    , ("memset"             , Memset.analyse           )
    , ("size-arg"           , SizeArg.analyse          )
    , ("sizeof"             , Sizeof.analyse           )
    , ("void-call"          , VoidCall.analyse         )
    ]

runLinters :: [Text] -> GlobalDecls -> Trav Env ()
runLinters flags tu =
    mapM_ (\(_, f) -> f tu) . filter ((`elem` flags) . fst) $ linters


analyse :: [Text] -> CTranslUnit -> [Text]
analyse enabled tu =
    case analysis of
        Left errs -> map (Text.pack . show) errs
        Right _   -> []
  where
    analysis = runTrav defaultEnv $ do
        modifyOptions (\opts -> opts { language = GNU99 })
        decls <- analyseAST tu
        runLinters enabled decls


allWarnings :: [Text]
allWarnings = map fst linters
