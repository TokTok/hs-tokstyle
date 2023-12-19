{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter
    ( analyse
    , allWarnings
    ) where

import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Language.C                       (CTranslUnit)
import           Language.C.Analysis.AstAnalysis  (analyseAST)
import           Language.C.Analysis.SemRep       (GlobalDecls)
import           Language.C.Analysis.TravMonad    (CLanguage (..), Trav,
                                                   TravOptions (..),
                                                   modifyOptions, runTrav)
import           Tokstyle.C.Env                   (Env, defaultEnv)

import qualified Tokstyle.C.Linter.BoolConversion as BoolConversion
import qualified Tokstyle.C.Linter.Cast           as Cast
import qualified Tokstyle.C.Linter.Conversion     as Conversion
import qualified Tokstyle.C.Linter.Sizeof         as Sizeof


linters :: [(Text, GlobalDecls -> Trav Env ())]
linters =
    [ ("bool-conversion"    , BoolConversion.analyse   )
    , ("cast"               , Cast.analyse             )
    , ("conversion"         , Conversion.analyse       )
    , ("sizeof"             , Sizeof.analyse           )
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
