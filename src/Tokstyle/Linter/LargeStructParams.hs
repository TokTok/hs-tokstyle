{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.LargeStructParams (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)

exemptions :: [Text]
exemptions =
    [ "Family"
    , "IP4"
    , "Logger_Level"  -- enum
    , "Net_Packet_Type"  -- enum
    , "Onion_Connection_Status"  -- enum
    , "Packet"
    , "Packet_Direction"  -- this is an enum
    , "Socket"
    , "State_Type"  -- enum
    ]

exemptionPrefixes :: [Text]
exemptionPrefixes =
    -- These are probably enums.
    [ "Group_"
    , "MSI"
    , "Tox_"
    , "Toxav_"
    ]

isExempt :: Text -> Bool
isExempt name = any (`Text.isPrefixOf` name) exemptionPrefixes || name `elem` exemptions

checkParam :: FilePath -> Node (Lexeme Text) -> State [Text] ()
checkParam file param = case unFix param of
    VarDecl (Fix (TyUserDefined (L _ _ name))) _ _ | not $ isExempt name ->
        warn file param $
            "`" <> name <> "` is a large struct and should be passed as pointer-to-const"

    _ -> return ()

linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionPrototype _ _ params ->
                mapM_ (checkParam file) params

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("large-struct-params", Text.unlines
    [ "Checks that large structs are passed by pointer rather than by value."
    , ""
    , "Exemptions are enums and some well-known small structs:"
    , ""
    , Text.intercalate "\n" . map (\x -> "- `" <> x <> "`") $ exemptions
    , ""
    , "and anything with one of the following prefixes, which are probably enums:"
    , ""
    , Text.intercalate "\n" . map (\x -> "- `" <> x <> "`") $ exemptionPrefixes
    , ""
    , "**Reason:** some structs in toxcore are up to 5MB in size, which would cause"
    , "stack overflows. Since we can't currently measure the size, we avoid any struct"
    , "passing altogether apart from some well-known exemptions."
    ]))
