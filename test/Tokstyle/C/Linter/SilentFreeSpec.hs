{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.SilentFreeSpec (spec) where

import           Data.Text             (Text)
import           Test.Hspec            (Spec, it)

import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["silent-free"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["silent-free"]


prelude :: [Text]
prelude =
    [ "void free(void *ptr);"
    , "void *calloc(unsigned long nmemb, unsigned long size);"
    , "void abort(void);"
    , "typedef enum State { IDLE, ACTIVE } State;"
    , "typedef struct Call { State state; void *user_data; } Call;"
    , "typedef void error_cb(void *user_data, Call *call);"
    , "typedef struct Session { error_cb *on_error; void *user_data; Call *call; } Session;"
    , "static void kill_call(Call *call) { free(call); }"
    , "static void tell_error(Session *s, Call *call) { s->on_error(s->user_data, call); }"
    , "static int start(Call *call);"
    ]


spec :: Spec
spec = do
    it "warns about a call freed on a failure path before its owner hears of it" $ do
        shouldWarn' (prelude ++
            [ "static void handle(Session *s, Call *call) {"
            , "  if (start(call) != 0) {"
            , "    kill_call(call);"
            , "  }"
            , "}"
            , "void handle_packet(Session *s) {"
            , "  Call *call = s->call;"
            , "  handle(s, call);"
            , "}"
            ])
            [[ "error: `call` is freed through `handle` in `handle_packet` without first being passed to a callback [-Wsilent-free]"
             , "   --> test.c:18:3"
             , "    |"
             , "   8| static void kill_call(Call *call) { free(call); }"
             , "    |                                     ^^^^^^^^^^"
             , "    |                                     |"
             , "    |                                     then `free`"
             , "  13|     kill_call(call);"
             , "    |     ^^^^^^^^^^^^^^^"
             , "    |     |"
             , "    |     then `kill_call`"
             , "  18|   handle(s, call);"
             , "    |   ^^^^^^^^^^^^^^^"
             , "    = note: callbacks take pointers to this object, and its owner may still hold one"
             ]]

    it "accepts a call passed to a callback before it is freed" $ do
        shouldAccept' (prelude ++
            [ "static void handle(Session *s, Call *call) {"
            , "  if (start(call) != 0) {"
            , "    tell_error(s, call);"
            , "    kill_call(call);"
            , "  }"
            , "}"
            , "void handle_packet(Session *s) {"
            , "  Call *call = s->call;"
            , "  handle(s, call);"
            , "}"
            ])

    it "accepts a call its caller hands over to be freed" $ do
        shouldAccept' (prelude ++
            [ "void hangup(Call *call) {"
            , "  kill_call(call);"
            , "}"
            ])

    it "accepts freeing everything in a function that frees its own object" $ do
        shouldAccept' (prelude ++
            [ "void session_kill(Session *s) {"
            , "  Call *call = s->call;"
            , "  kill_call(call);"
            , "  free(s);"
            , "}"
            ])

    it "accepts freeing a call it made itself" $ do
        shouldAccept' (prelude ++
            [ "Call *call_new(void) {"
            , "  Call *call = (Call *)calloc(1, sizeof(Call));"
            , "  if (call == 0 || start(call) != 0) {"
            , "    kill_call(call);"
            , "    return 0;"
            , "  }"
            , "  return call;"
            , "}"
            ])

    it "accepts a switch with a case for every state that tells the owner in each" $ do
        shouldAccept' (prelude ++
            [ "void handle_packet(Session *s) {"
            , "  Call *call = s->call;"
            , "  switch (call->state) {"
            , "    case IDLE:"
            , "      abort();"
            , "    case ACTIVE:"
            , "      tell_error(s, call);"
            , "      break;"
            , "  }"
            , "  kill_call(call);"
            , "}"
            ])
