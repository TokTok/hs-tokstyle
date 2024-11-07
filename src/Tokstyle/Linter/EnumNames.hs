{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.EnumNames (descr) where

import           Control.Monad               (unless)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Maybe                  (maybeToList)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (HasDiagnostics (..), warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)


data Linter = Linter
    { diags    :: [Text]
    , enumName :: Text
    , prefix   :: Text
    }

instance HasDiagnostics Linter where
    addDiagnostic diag l@Linter{diags} = l{diags = addDiagnostic diag diags}

empty :: Linter
empty = Linter [] "" ""

exemptions :: [Text]
exemptions =
    [ "Connection_Status"
    , "Crypto_Conn_State"
    , "Friend_Add_Error"
    , "Friend_Status"
    , "GC_Conn_State"
    , "Group_Broadcast_Type"
    , "Groupchat_Connection_Type"
    , "Group_Exit_Type"
    , "Group_Handshake_Join_Type"
    , "Group_Handshake_Packet_Type"
    , "Group_Handshake_Request_Type"
    , "Group_Invite_Message_Type"
    , "Group_Join_Rejected"
    , "Group_Message_Ack_Type"
    , "Group_Message_Id"
    , "Group_Message_Type"
    , "Group_Moderation_Event"
    , "Group_Packet_Type"
    , "Group_Peer_Status"
    , "Group_Privacy_State"
    , "Group_Role"
    , "Group_Sync_Flags"
    , "Group_Topic_Lock"
    , "Group_Voice_State"
    , "Invite_Id"
    , "Mod_Sanction_Type"
    , "MSICallbackID"
    , "MSICallState"
    , "MSICapabilities"
    , "MSIError"
    , "MSIHeaderID"
    , "MSIRequest"
    , "Net_Packet_Type"
    , "Peer_Id"
    , "RTPFlags"
    , "Self_UDP_Status"
    , "TCP_Client_Status"
    ]

linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            EnumConsts (Just (L _ _ enumName)) _
                | enumName `elem` exemptions -> return ()
                | otherwise -> do
                    let prefix = Text.toUpper enumName <> "_"
                    State.withState (\s -> s{enumName, prefix}) act

            EnumDecl (L _ _ enumName) _ _
                | enumName `elem` exemptions -> return ()
                | otherwise -> do
                    let prefix = Text.toUpper enumName <> "_"
                    State.withState (\s -> s{enumName, prefix}) act

            Enumerator (L _ _ name) _ -> do
                Linter{enumName, prefix} <- State.get
                let prefixes = stripType prefix
                unless (any (`Text.isPrefixOf` name) prefixes) $
                    warn file node $
                        "enumerator `" <> name <> "` in enum `" <> enumName
                        <> "` should start with "
                        <> Text.intercalate " or " (map (\x -> "`" <> x <> "`") prefixes)

            _ -> act
    }
  where
    stripType :: Text -> [Text]
    stripType name =
        [name]
        ++ allowSuffix "_TYPE_"
        ++ allowSuffix "_T_"
        ++ allowSuffix "_E_"  -- for cmp
      where
        allowSuffix s = maybeToList ((<>"_") <$> Text.stripSuffix s name)

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . diags . flip State.execState empty . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("enum-names", Text.unlines
    [ "Checks that `enum` value constants have the same prefix as the `enum` type,"
    , "except they should be SCREAMING_CASE instead of Camel_Snake. There are currently"
    , Text.pack (show $ length exemptions) <> " exemptions to this rule. "
      <> "New enums should follow the naming convention."
    , ""
    , "**Reason:** this naming convention helps identify the type of an `enum` constant"
    , "at first glance."
    ]))
