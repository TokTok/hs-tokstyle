{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.LargeStructParams (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text, isPrefixOf)
import           Language.Cimple             (IdentityActions, Lexeme (..),
                                              Node, NodeF (..), defaultActions,
                                              doNode, traverseAst)
import           Language.Cimple.Diagnostics (warn)

exemptions :: [Text]
exemptions =
    [ "Family"
    , "IP4"
    , "Logger_Level"
    , "Packet"
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

linter :: IdentityActions (State [Text]) Text
linter = defaultActions
    { doNode = \file node act ->
        case unFix node of
            FunctionPrototype _ _ params -> do
                mapM_ (checkParam file) params
                return node

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
