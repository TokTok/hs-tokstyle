{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.LargeStructParams (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text, isPrefixOf)
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)

exemptions :: [Text]
exemptions =
    [ "Family"
    , "IP4"
    , "Logger_Level"
    , "Onion_Connection_Status"
    , "Packet"
    , "Packet_Direction"  -- this is an enum
    , "Socket"
    , "State_Type"
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
isExempt name = any (`isPrefixOf` name) exemptionPrefixes || name `elem` exemptions

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
