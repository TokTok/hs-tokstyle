module Tokstyle.Linter
    ( analyse
    , analyseGlobal
    ) where

import           Data.Text                        (Text)
import           Language.Cimple                  (Lexeme, Node)

import qualified Tokstyle.Linter.ForLoops         as ForLoops
import qualified Tokstyle.Linter.FuncPrototypes   as FuncPrototypes
import qualified Tokstyle.Linter.FuncScopes       as FuncScopes
import qualified Tokstyle.Linter.GlobalFuncs      as GlobalFuncs
import qualified Tokstyle.Linter.LoggerCalls      as LoggerCalls
import qualified Tokstyle.Linter.LoggerNoEscapes  as LoggerNoEscapes
import qualified Tokstyle.Linter.VarUnusedInScope as VarUnusedInScope

import qualified Tokstyle.Linter.DeclaredOnce     as DeclaredOnce
import qualified Tokstyle.Linter.DeclsHaveDefns   as DeclsHaveDefns
import qualified Tokstyle.Linter.DocComments      as DocComments


type TranslationUnit = (FilePath, [Node (Lexeme Text)])

analyse :: TranslationUnit -> [Text]
analyse tu = concatMap ($ tu)
    [ ForLoops.analyse
    , FuncPrototypes.analyse
    , FuncScopes.analyse
    , GlobalFuncs.analyse
    , LoggerCalls.analyse
    , LoggerNoEscapes.analyse
    , VarUnusedInScope.analyse
    ]

analyseGlobal :: [TranslationUnit] -> [Text]
analyseGlobal tus = concatMap ($ tus)
    [ DeclaredOnce.analyse
    , DeclsHaveDefns.analyse
    , DocComments.analyse
    ]
