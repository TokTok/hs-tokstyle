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
import qualified Tokstyle.Linter.TypedefName      as TypedefName
import qualified Tokstyle.Linter.VarUnusedInScope as VarUnusedInScope

import qualified Tokstyle.Linter.DeclaredOnce     as DeclaredOnce
import qualified Tokstyle.Linter.DeclsHaveDefns   as DeclsHaveDefns
import qualified Tokstyle.Linter.DocComments      as DocComments
import qualified Tokstyle.Linter.TypeCheck        as TypeCheck


type TranslationUnit = (FilePath, [Node (Lexeme Text)])

analyse :: TranslationUnit -> [Text]
analyse tu = concatMap ($ tu)
    [ ForLoops.analyse
    , FuncPrototypes.analyse
    , FuncScopes.analyse
    , GlobalFuncs.analyse
    , LoggerCalls.analyse
    , LoggerNoEscapes.analyse
    , TypedefName.analyse
    , VarUnusedInScope.analyse
    ]

analyseGlobal :: [TranslationUnit] -> [Text]
analyseGlobal tus = concatMap ($ tus)
    [ DeclaredOnce.analyse
    , DeclsHaveDefns.analyse
    , DocComments.analyse
    , TypeCheck.analyse
    ]
