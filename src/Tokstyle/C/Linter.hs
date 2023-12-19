{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter
    ( analyse
    , allWarnings
    ) where

import           Data.Text                        (Text)
import           Language.C.Analysis.SemRep       (GlobalDecls)
import           Language.C.Analysis.TravMonad    (Trav)
import           Tokstyle.C.Env                   (Env)

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

analyse :: [Text] -> GlobalDecls -> Trav Env ()
analyse flags tu =
    mapM_ (\(_, f) -> f tu) . filter ((`elem` flags) . fst) $ linters

allWarnings :: [Text]
allWarnings = map fst linters
