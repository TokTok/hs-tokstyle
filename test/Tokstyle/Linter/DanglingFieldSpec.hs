{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.DanglingFieldSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [(FilePath, [Text])] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["dangling-field"]


shouldAccept' :: [(FilePath, [Text])] -> IO ()
shouldAccept' = shouldAccept ["dangling-field"]


bwc :: (FilePath, [Text])
bwc = ("bwc.c",
    [ "void bwc_kill(Bwc *bwc) {"
    , "  free(bwc);"
    , "}"
    ])


spec :: Spec
spec = do
    it "warns about a field left pointing at what a failure path freed" $ do
        shouldWarn'
            [ bwc
            , ("av.c",
                [ "static bool prepare(Call *call) {"
                , "  call->bwc = bwc_new();"
                , "  if (start(call->bwc) != 0) {"
                , "    bwc_kill(call->bwc);"
                , "    return false;"
                , "  }"
                , "  return true;"
                , "}"
                , "bool answer(Av *av, uint32_t friend_number) {"
                , "  Call *call = call_get(av, friend_number);"
                , "  return prepare(call);"
                , "}"
                ])
            ]
            [[ "error: `call->bwc` is freed here, and `call->bwc` still points at it when `answer` returns [-Wdangling-field]"
             , "   --> av.c:4:5"
             , "    |"
             , "   4|     bwc_kill(call->bwc);"
             , "    |     ^^^^^^^^"
             ]]

    it "accepts a field set to null after it is freed" $ do
        shouldAccept'
            [ bwc
            , ("av.c",
                [ "bool answer(Av *av, uint32_t friend_number) {"
                , "  Call *call = call_get(av, friend_number);"
                , "  bwc_kill(call->bwc);"
                , "  call->bwc = nullptr;"
                , "  return true;"
                , "}"
                ])
            ]

    it "accepts a field whose struct is freed after it" $ do
        shouldAccept'
            [ bwc
            , ("av.c",
                [ "static void call_kill(Call *call) {"
                , "  bwc_kill(call->bwc);"
                , "  free(call);"
                , "}"
                , "void hangup(Av *av, uint32_t friend_number) {"
                , "  Call *call = call_get(av, friend_number);"
                , "  call_kill(call);"
                , "}"
                ])
            ]

    it "accepts a caller that removes the struct after a cleanup leaves its fields freed" $ do
        shouldAccept'
            [ bwc
            , ("av.c",
                [ "static void stop(Call *call) {"
                , "  bwc_kill(call->bwc);"
                , "}"
                , "void hangup(Av *av, uint32_t friend_number) {"
                , "  Call *call = call_get(av, friend_number);"
                , "  stop(call);"
                , "  free(call);"
                , "}"
                ])
            ]

    it "accepts a field freed by a call that failed and so freed nothing" $ do
        shouldAccept'
            [ bwc
            , ("av.c",
                [ "static int hangup(Bwc *bwc) {"
                , "  if (bwc_busy(bwc)) {"
                , "    return -1;"
                , "  }"
                , "  bwc_kill(bwc);"
                , "  return 0;"
                , "}"
                , "int cancel(Av *av, uint32_t friend_number) {"
                , "  Call *call = call_get(av, friend_number);"
                , "  if (hangup(call->bwc) != 0) {"
                , "    return -1;"
                , "  }"
                , "  call->bwc = nullptr;"
                , "  return 0;"
                , "}"
                ])
            ]

    it "accepts a field freed on paths an error flag says free the struct too" $ do
        shouldAccept'
            [ bwc
            , ("av.c",
                [ "Av *av_new(void) {"
                , "  Err rc = ERR_OK;"
                , "  Av *av = av_alloc();"
                , "  if (av == nullptr) {"
                , "    rc = ERR_MALLOC;"
                , "    goto RETURN;"
                , "  }"
                , "  av->bwc = bwc_new();"
                , "  if (start(av->bwc) != 0) {"
                , "    bwc_kill(av->bwc);"
                , "    rc = ERR_START;"
                , "    goto RETURN;"
                , "  }"
                , "RETURN:"
                , "  if (rc != ERR_OK) {"
                , "    if (av != nullptr) {"
                , "      free(av);"
                , "      av = nullptr;"
                , "    }"
                , "  }"
                , "  return av;"
                , "}"
                ])
            ]

    it "accepts a struct freed through the pointer it was cast from" $ do
        shouldAccept'
            [ bwc
            , ("av.c",
                [ "void peer_delete(void *object) {"
                , "  Peer *peer = (Peer *)object;"
                , "  bwc_kill(peer->bwc);"
                , "  free(object);"
                , "}"
                , "void on_leave(Av *av) {"
                , "  void *object = peers_first(av);"
                , "  peer_delete(object);"
                , "}"
                ])
            ]

    it "accepts a slot wiped through the container and index it was got with" $ do
        shouldAccept'
            [ bwc
            , ("av.c",
                [ "Conn *get_conn(Conns *c, int n) {"
                , "  return &c->conns[n];"
                , "}"
                , "int kill_relay(Conns *c, int n) {"
                , "  Conn *con = get_conn(c, n);"
                , "  bwc_kill(con->bwc);"
                , "  return wipe_conn(c, n);"
                , "}"
                , "void drop(Conns *c) {"
                , "  kill_relay(c, 0);"
                , "}"
                ])
            ]

    it "accepts a field freed and not reset only where it is found null" $ do
        shouldAccept'
            [ ("conn.c",
                [ "void conn_reset(Session *s) {"
                , "  Conn *c = s->conn;"
                , "  free(c->buf);"
                , "  if (c->buf == nullptr) {"
                , "    return;"
                , "  }"
                , "  c->buf = nullptr;"
                , "}"
                ])
            ]
