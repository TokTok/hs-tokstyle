{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Tokstyle.Linter
    ( analyse
    , analyseLocal
    , analyseGlobal
    , allWarnings
    , markdown
    ) where

import           Control.Parallel.Strategies       (parMap, rpar)
import qualified Data.List                         as List
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Language.Cimple                   (Lexeme, Node)

import qualified Tokstyle.Linter.Assert            as Assert
import qualified Tokstyle.Linter.BooleanReturn     as BooleanReturn
import qualified Tokstyle.Linter.Booleans          as Booleans
import qualified Tokstyle.Linter.CallbackNames     as CallbackNames
import qualified Tokstyle.Linter.CallocArgs        as CallocArgs
import qualified Tokstyle.Linter.CallocType        as CallocType
import qualified Tokstyle.Linter.CompoundInit      as CompoundInit
import qualified Tokstyle.Linter.Constness         as Constness
import qualified Tokstyle.Linter.EnumDefines       as EnumDefines
import qualified Tokstyle.Linter.EnumNames         as EnumNames
import qualified Tokstyle.Linter.FuncPrototypes    as FuncPrototypes
import qualified Tokstyle.Linter.FuncScopes        as FuncScopes
import qualified Tokstyle.Linter.GlobalFuncs       as GlobalFuncs
import qualified Tokstyle.Linter.LargeStructParams as LargeStructParams
import qualified Tokstyle.Linter.LoggerCalls       as LoggerCalls
import qualified Tokstyle.Linter.LoggerConst       as LoggerConst
import qualified Tokstyle.Linter.LoggerNoEscapes   as LoggerNoEscapes
import qualified Tokstyle.Linter.MallocCall        as MallocCall
import qualified Tokstyle.Linter.MallocType        as MallocType
import qualified Tokstyle.Linter.MemcpyStructs     as MemcpyStructs
import qualified Tokstyle.Linter.MissingNonNull    as MissingNonNull
import qualified Tokstyle.Linter.Nesting           as Nesting
import qualified Tokstyle.Linter.NonNull           as NonNull
import qualified Tokstyle.Linter.Parens            as Parens
import qualified Tokstyle.Linter.SwitchIf          as SwitchIf
import qualified Tokstyle.Linter.TypedefName       as TypedefName
import qualified Tokstyle.Linter.UnsafeFunc        as UnsafeFunc
import qualified Tokstyle.Linter.VarUnusedInScope  as VarUnusedInScope

import qualified Tokstyle.Linter.Callgraph         as Callgraph
import qualified Tokstyle.Linter.DeclaredOnce      as DeclaredOnce
import qualified Tokstyle.Linter.DeclsHaveDefns    as DeclsHaveDefns
import qualified Tokstyle.Linter.DocComments       as DocComments
import qualified Tokstyle.Linter.TypeCheck         as TypeCheck
import qualified Tokstyle.SemFmt.EnumFromInt       as EnumFromInt
import qualified Tokstyle.SemFmt.EnumToString      as EnumToString
import qualified Tokstyle.SemFmt.EnumUnpack        as EnumUnpack


type TranslationUnit = (FilePath, [Node (Lexeme Text)])

run :: [(t -> [Text], (Text, Text))] -> [Text] -> t -> [Text]
run linters flags tu =
    concat . parMap rpar apply . filter ((`elem` flags) . fst . snd) $ linters
  where
    apply (f, (flag, _)) = map (<> " [-W" <> flag <> "]") $ f tu

type LocalLinter = (TranslationUnit -> [Text], (Text, Text))

localLinters :: [LocalLinter]
localLinters =
    [ Assert.descr
    , Booleans.descr
    , BooleanReturn.descr
    , CallbackNames.descr
    , CallocArgs.descr
    , CallocType.descr
    , CompoundInit.descr
    , Constness.descr
    , EnumDefines.descr
    , EnumNames.descr
    , FuncPrototypes.descr
    , FuncScopes.descr
    , GlobalFuncs.descr
    , LargeStructParams.descr
    , LoggerCalls.descr
    , LoggerConst.descr
    , LoggerNoEscapes.descr
    , MallocCall.descr
    , MallocType.descr
    , MemcpyStructs.descr
    , MissingNonNull.descr
    , Nesting.descr
    , NonNull.descr
    , Parens.descr
    , SwitchIf.descr
    , TypedefName.descr
    , UnsafeFunc.descr
    , VarUnusedInScope.descr
    ]

type GlobalLinter = ([TranslationUnit] -> [Text], (Text, Text))

globalLinters :: [GlobalLinter]
globalLinters =
    [ Callgraph.descr
    , DeclaredOnce.descr
    , DeclsHaveDefns.descr
    , DocComments.descr
    , TypeCheck.descr
    -- Semantic formatters:
    , EnumFromInt.descr
    , EnumToString.descr
    , EnumUnpack.descr
    ]

analyseLocal :: [Text] -> TranslationUnit -> [Text]
analyseLocal = run localLinters

analyseGlobal :: [Text] -> [TranslationUnit] -> [Text]
analyseGlobal = run globalLinters

analyse :: [Text] -> [TranslationUnit] -> [Text]
analyse linters tus = concat $ analyseGlobal linters tus : parMap rpar (analyseLocal linters) tus

allWarnings :: [Text]
allWarnings = map (fst . snd) localLinters ++ map (fst . snd) globalLinters

class LinterType a where
    linterType :: a -> Text

instance LinterType LocalLinter where linterType = const ""
instance LinterType GlobalLinter where linterType = const " (global)"

markdown :: Text
markdown = Text.intercalate "\n" . (prelude ++) . map snd . List.sort $ map mkDoc localLinters ++ map mkDoc globalLinters
  where
    prelude =
        [ "# Cimple-based linters (`check-cimple`)"
        , ""
        , "There are currently " <> Text.pack (show $ length localLinters + length globalLinters) <> " linters implemented,"
          <> " out of which " <> Text.pack (show $ length globalLinters) <> " perform global analyses."
        , "In the list below, the global ones are marked specially."
        , ""
        ]
    mkDoc lnt =
        let (flag, doc) = snd lnt in
        (flag, "## `-W" <> flag <> "`" <> linterType lnt <> "\n\n" <> doc)
