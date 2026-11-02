{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.PartialFailureSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldWarn, testC)


shouldWarn' :: [(FilePath, [Text])] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["partial-failure"]


shouldAccept' :: [(FilePath, [Text])] -> IO ()
shouldAccept' = shouldAccept ["partial-failure"]


spec :: Spec
spec = do
    it "warns about an array length lowered by a removal that then fails" $ do
        shouldWarn' (testC
            [ "bool peer_delete(Chat *chat, uint32_t peer_number) {"
            , "  --chat->numpeers;"
            , "  Peer *tmp = (Peer *)mem_vrealloc(chat->mem, chat->group, chat->numpeers, sizeof(Peer));"
            , "  if (tmp == nullptr) {"
            , "    return false;"
            , "  }"
            , "  chat->group = tmp;"
            , "  return true;"
            , "}"
            ])
            [[ "error: `chat->numpeers` is changed here, and `peer_delete` then fails at line 5 without changing it back [-Wpartial-failure]"
             , "   --> test.c:2:11"
             , "    |"
             , "   2|   --chat->numpeers;"
             , "    |           ^^^^^^^^"
             ]]

    it "accepts a count that is not the length of an array" $ do
        shouldAccept' (testC
            [ "int send_message(Chat *chat) {"
            , "  ++chat->message_number;"
            , "  if (send_all(chat) == 0) {"
            , "    return -1;"
            , "  }"
            , "  return 0;"
            , "}"
            ])

    it "warns about a failure that skips the cleanup earlier failures do" $ do
        shouldWarn' (testC
            [ "void group_leave(Session *c, Chat *chat) {"
            , "  free(chat->peers);"
            , "}"
            , "int accept_invite(Session *c, const uint8_t *id) {"
            , "  const int group_number = create_group(c);"
            , "  if (group_number == -1) {"
            , "    return -1;"
            , "  }"
            , "  Chat *chat = get_group(c, group_number);"
            , "  if (chat == nullptr) {"
            , "    return -1;"
            , "  }"
            , "  if (!set_id(chat, id)) {"
            , "    group_leave(c, chat);"
            , "    return -2;"
            , "  }"
            , "  if (!set_password(chat)) {"
            , "    group_leave(c, chat);"
            , "    return -3;"
            , "  }"
            , "  if (send_accept(chat) != 0) {"
            , "    return -4;"
            , "  }"
            , "  return group_number;"
            , "}"
            ])
            [[ "error: `accept_invite` fails here without calling `group_leave` on `chat`, which it does when it fails before this [-Wpartial-failure]"
             , "   --> test.c:22:13"
             , "    |"
             , "  22|     return -4;"
             , "    |             ^"
             ]]

    it "accepts a failure where the status a pointer came with says it is null" $ do
        shouldAccept'
            [ ("jbuf.c",
                [ "Packet *jbuf_read(Jbuf *q, int *success) {"
                , "  if (q->top == q->bottom) {"
                , "    *success = 0;"
                , "    return nullptr;"
                , "  }"
                , "  *success = 1;"
                , "  return q->packet;"
                , "}"
                ])
            , ("av.c",
                [ "void packet_free(Packet *pk) {"
                , "  free(pk);"
                , "}"
                , "int decode(Jbuf *q) {"
                , "  int success;"
                , "  Packet *pk = jbuf_read(q, &success);"
                , "  if (success == 1) {"
                , "    if (pk->length == 0) {"
                , "      packet_free(pk);"
                , "      return -1;"
                , "    }"
                , "    if (play(pk) != 0) {"
                , "      packet_free(pk);"
                , "      return -1;"
                , "    }"
                , "    packet_free(pk);"
                , "    return 0;"
                , "  } else {"
                , "    return -1;"
                , "  }"
                , "}"
                ])
            ]

    it "accepts a failure after what it would destroy was handed over" $ do
        shouldAccept' (testC
            [ "void onion_kill(Onion *onion) {"
            , "  free(onion);"
            , "}"
            , "int start(Session *s) {"
            , "  Onion *onion = new_onion(s);"
            , "  if (onion == nullptr) {"
            , "    return -1;"
            , "  }"
            , "  if (!onion_bind(onion)) {"
            , "    onion_kill(onion);"
            , "    return -1;"
            , "  }"
            , "  if (!onion_listen(onion)) {"
            , "    onion_kill(onion);"
            , "    return -1;"
            , "  }"
            , "  s->onion = onion;"
            , "  if (!session_start(s)) {"
            , "    return -1;"
            , "  }"
            , "  return 0;"
            , "}"
            ])

    it "accepts a failure that skips setting what earlier failures set" $ do
        shouldAccept' (testC
            [ "void chat_set_name(Chat *chat, uint8_t *name) {"
            , "  free(chat->name);"
            , "  chat->name = name;"
            , "}"
            , "int start(Session *s, uint8_t *name) {"
            , "  Chat *chat = chat_new(s);"
            , "  if (chat == nullptr) {"
            , "    return -1;"
            , "  }"
            , "  if (!chat_bind(chat)) {"
            , "    chat_set_name(chat, name);"
            , "    return -1;"
            , "  }"
            , "  if (!chat_listen(chat)) {"
            , "    chat_set_name(chat, name);"
            , "    return -1;"
            , "  }"
            , "  if (!chat_start(chat)) {"
            , "    return -1;"
            , "  }"
            , "  s->chat = chat;"
            , "  return 0;"
            , "}"
            ])
