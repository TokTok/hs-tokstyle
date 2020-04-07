module Tokstyle.Cimple.Analysis (analyse) where

import           Data.Text                                (Text)
import           Tokstyle.Cimple.AST                      (Node (..))
import           Tokstyle.Cimple.Lexer                    (Lexeme)

import qualified Tokstyle.Cimple.Analysis.FuncScopes      as FuncScopes
import qualified Tokstyle.Cimple.Analysis.GlobalFuncs     as GlobalFuncs
import qualified Tokstyle.Cimple.Analysis.LoggerCalls     as LoggerCalls
import qualified Tokstyle.Cimple.Analysis.LoggerNoEscapes as LoggerNoEscapes

analyse :: FilePath -> [Node (Lexeme Text)] -> [Text]
analyse file ast = concatMap (\f -> f file ast)
    [ FuncScopes.analyse
    , GlobalFuncs.analyse
    , LoggerCalls.analyse
    , LoggerNoEscapes.analyse
    ]
