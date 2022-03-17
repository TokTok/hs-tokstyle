{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter
    ( analyse
    , analyseGlobal
    , allWarnings
    ) where

import           Data.Text                         (Text)
import           Language.Cimple                   (Lexeme, Node)

import qualified Tokstyle.Linter.Assert            as Assert
import qualified Tokstyle.Linter.BooleanReturn     as BooleanReturn
import qualified Tokstyle.Linter.Booleans          as Booleans
import qualified Tokstyle.Linter.CallbackNames     as CallbackNames
import qualified Tokstyle.Linter.Callgraph         as Callgraph
import qualified Tokstyle.Linter.CallocArgs        as CallocArgs
import qualified Tokstyle.Linter.CallocType        as CallocType
import qualified Tokstyle.Linter.CompoundInit      as CompoundInit
import qualified Tokstyle.Linter.Constness         as Constness
import qualified Tokstyle.Linter.EnumNames         as EnumNames
import qualified Tokstyle.Linter.FuncPrototypes    as FuncPrototypes
import qualified Tokstyle.Linter.FuncScopes        as FuncScopes
import qualified Tokstyle.Linter.GlobalFuncs       as GlobalFuncs
import qualified Tokstyle.Linter.LargeStructParams as LargeStructParams
import qualified Tokstyle.Linter.LoggerCalls       as LoggerCalls
import qualified Tokstyle.Linter.LoggerConst       as LoggerConst
import qualified Tokstyle.Linter.LoggerNoEscapes   as LoggerNoEscapes
import qualified Tokstyle.Linter.MallocType        as MallocType
import qualified Tokstyle.Linter.MemcpyStructs     as MemcpyStructs
import qualified Tokstyle.Linter.MissingNonNull    as MissingNonNull
import qualified Tokstyle.Linter.NonNull           as NonNull
import qualified Tokstyle.Linter.Parens            as Parens
import qualified Tokstyle.Linter.TypedefName       as TypedefName
import qualified Tokstyle.Linter.UnsafeFunc        as UnsafeFunc
import qualified Tokstyle.Linter.VarUnusedInScope  as VarUnusedInScope

import qualified Tokstyle.Linter.DeclaredOnce      as DeclaredOnce
import qualified Tokstyle.Linter.DeclsHaveDefns    as DeclsHaveDefns
import qualified Tokstyle.Linter.DocComments       as DocComments
import qualified Tokstyle.Linter.TypeCheck         as TypeCheck


type TranslationUnit = (FilePath, [Node (Lexeme Text)])

run :: [(Text, t -> [Text])] -> [Text] -> t -> [Text]
run linters flags tu =
    concatMap apply $ filter ((`elem` flags) . fst) linters
  where
    apply (flag, f) = map (<> " [-W" <> flag <> "]") $ f tu

localLinters :: [(Text, TranslationUnit -> [Text])]
localLinters =
    [ ("assert"             , Assert.analyse           )
    , ("booleans"           , Booleans.analyse         )
    , ("boolean-return"     , BooleanReturn.analyse    )
    , ("callback-names"     , CallbackNames.analyse    )
    , ("calloc-args"        , CallocArgs.analyse       )
    , ("calloc-type"        , CallocType.analyse       )
    , ("compound-init"      , CompoundInit.analyse     )
    , ("constness"          , Constness.analyse        )
    , ("enum-names"         , EnumNames.analyse        )
    , ("func-prototypes"    , FuncPrototypes.analyse   )
    , ("func-scopes"        , FuncScopes.analyse       )
    , ("global-funcs"       , GlobalFuncs.analyse      )
    , ("large-struct-params", LargeStructParams.analyse)
    , ("logger-calls"       , LoggerCalls.analyse      )
    , ("logger-const"       , LoggerConst.analyse      )
    , ("logger-no-escapes"  , LoggerNoEscapes.analyse  )
    , ("malloc-type"        , MallocType.analyse       )
    , ("memcpy-structs"     , MemcpyStructs.analyse    )
    , ("missing-non-null"   , MissingNonNull.analyse   )
    , ("non-null"           , NonNull.analyse          )
    , ("parens"             , Parens.analyse           )
    , ("typedef-name"       , TypedefName.analyse      )
    , ("unsafe-func"        , UnsafeFunc.analyse       )
    , ("var-unused-in-scope", VarUnusedInScope.analyse )
    ]

globalLinters :: [(Text, [TranslationUnit] -> [Text])]
globalLinters =
    [ ("callgraph"          , Callgraph.analyse        )
    , ("declared-once"      , DeclaredOnce.analyse     )
    , ("decls-have-defns"   , DeclsHaveDefns.analyse   )
    , ("doc-comments"       , DocComments.analyse      )
    , ("type-check"         , TypeCheck.analyse        )
    ]

analyse :: [Text] -> TranslationUnit -> [Text]
analyse = run localLinters

analyseGlobal :: [Text] -> [TranslationUnit] -> [Text]
analyseGlobal = run globalLinters

allWarnings :: [Text]
allWarnings = map fst localLinters ++ map fst globalLinters
