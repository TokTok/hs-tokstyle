{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.EnumDefines (descr) where

import           Control.Monad               (when)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), LiteralType (..),
                                              Node, NodeF (..))
import           Language.Cimple.Diagnostics (HasDiagnostics (..), warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Text.Casing                 (fromHumps, fromSnake, toPascal,
                                              toSnake, unIdentifier)
import           Text.Read                   (readMaybe)


-- | A sequence of #defines must be at least this long before we think it
-- should be an enum, instead.
minSequence :: Int
minSequence = 5

-- | The common prefix must have at least this many components (e.g. FOO is
-- not a sufficient common prefix, but FOO_BAR is).
minComponents :: Int
minComponents = 2

-- | We only enforce enums for small integers, so we skip floats and large
-- integers that might not fit into the underlying enum type (int). This also
-- skips sequences of `MAX_SOMETHING_...` that tend to be larger numbers.
maxSmallInt :: Int
maxSmallInt = 0xff

data Linter = Linter
    { diags :: [Text]
    , defs  :: [Text]
    }

instance HasDiagnostics Linter where
    addDiagnostic diag l@Linter{diags} = l{diags = addDiagnostic diag diags}

empty :: Linter
empty = Linter [] []

addDef :: Text -> Linter -> Linter
addDef def l@Linter{defs} = l{defs = def:defs}

clearDefs :: Linter -> Linter
clearDefs l = l{defs = []}

commonPrefix :: [String] -> String
commonPrefix [] = ""
commonPrefix (first:rest) = foldl go first rest
  where
    go _ [] = []
    go [] _ = []
    go (x:xs) (y:ys)
      | x == y    = x : go xs ys
      | otherwise = []


checkEnumDefs :: FilePath -> Node (Lexeme Text) -> State Linter ()
checkEnumDefs file node = do
    Linter{defs} <- State.get
    let cp = commonPrefix $ map Text.unpack defs
    -- Warn exactly once (hence == instead of >=).
    when (length defs == minSequence && numComponents cp >= minComponents) $
        warn file node $ "sequence of `#define`s longer than " <> Text.pack (show minSequence)
            <> " could be written as `enum " <> toEnumName cp <> "`"

  where
    numComponents = length . unIdentifier . fromSnake
    toEnumName = Text.pack . toSnake . fromHumps . toPascal . fromSnake


isSmallInt :: Text -> Bool
isSmallInt txt =
    case readMaybe $ Text.unpack txt of
        Nothing  -> False
        Just num -> num <= maxSmallInt


linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            PreprocDefineConst (L _ _ name) (Fix (LiteralExpr Int (L _ _ num))) | isSmallInt num -> do
                State.modify $ addDef name
                checkEnumDefs file node

            -- Skip comments, don't clear defs.
            Comment{} -> act

            _ -> do
                act  -- Recurse first, check defs later.
                checkEnumDefs file node
                -- Clear defs whenever we see a new kind of node (not comment or #define).
                State.modify clearDefs
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . diags . flip State.execState empty . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("enum-defines", Text.unlines
    [ "Suggests using `enum` instead of a sequence of `#define`s for enumerations."
    , "Only matches sequences of `#define`s longer than " <> Text.pack (show minSequence)
      <> " to avoid some false positives."
    , "Also, the sequence must have a common prefix of at least " <> Text.pack (show minComponents)
      <> " components. I.e."
    , "`" <> mkPrefix 1 <> "` is not a sufficient common prefix, but `" <> mkPrefix 0 <> "` is."
    , "Lastly, we only require enums for small-int enums, i.e. all enumerators have a"
    , "constant int expression value less than or equal to " <> Text.pack (show maxSmallInt) <> "."
    , ""
    , "**Reason:** `enum` constants are safer, and can potentially be type-checked"
    , "more thoroughly."
    ]))
  where
    mkPrefix n =
        Text.intercalate "_" . map (Text.pack . replicate 3 . fst) . zip ['A'..] $ [1..minComponents-n]
