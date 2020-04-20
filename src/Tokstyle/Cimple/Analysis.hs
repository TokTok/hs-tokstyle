module Tokstyle.Cimple.Analysis (analyse) where

import           Data.Text                                (Text)
import           Language.Cimple                          (Lexeme, Node (..))

import qualified Tokstyle.Cimple.Analysis.FuncPrototypes  as FuncPrototypes
import qualified Tokstyle.Cimple.Analysis.FuncScopes      as FuncScopes
import qualified Tokstyle.Cimple.Analysis.GlobalFuncs     as GlobalFuncs
import qualified Tokstyle.Cimple.Analysis.LoggerCalls     as LoggerCalls
import qualified Tokstyle.Cimple.Analysis.LoggerNoEscapes as LoggerNoEscapes
import qualified Tokstyle.Cimple.Analysis.NameSpelling    as NameSpelling

analyse :: FilePath -> [Node (Lexeme Text)] -> [Text]
analyse file ast = concatMap (\f -> f file ast)
    [ FuncPrototypes.analyse
    , FuncScopes.analyse
    , GlobalFuncs.analyse
    , LoggerCalls.analyse
    , LoggerNoEscapes.analyse
    , NameSpelling.analyse
    ]
