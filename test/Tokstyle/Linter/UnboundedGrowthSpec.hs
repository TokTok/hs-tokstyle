{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.UnboundedGrowthSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldWarn, testC)


queue :: [Text] -> [Text]
queue addCheck =
    [ "typedef struct Item Item;"
    , "struct Item {"
    , "  Item *next;"
    , "};"
    , "typedef struct Queue {"
    , "  Item *head;"
    , "  uint32_t length;"
    , "} Queue;"
    , "static bool queue_add(Queue *q) {"
    ] ++ addCheck ++
    [ "  Item *item = (Item *)calloc(1, sizeof(Item));"
    , "  if (item == nullptr) {"
    , "    return false;"
    , "  }"
    , "  item->next = q->head;"
    , "  q->head = item;"
    , "  ++q->length;"
    , "  return true;"
    , "}"
    ]

receive :: [Text] -> [Text]
receive callCheck =
    [ "static int sock_read(int sock, uint8_t *buf, uint32_t len) {"
    , "  return recv(sock, buf, len, 0);"
    , "}"
    , "void queue_poll(Queue *q, int sock) {"
    , "  uint8_t data[16];"
    , "  if (sock_read(sock, data, sizeof(data)) <= 0) {"
    , "    return;"
    , "  }"
    ] ++ callCheck ++
    [ "  queue_add(q);"
    , "}"
    ]

spec :: Spec
spec = do
    it "warns about a list packets make grow with nothing limiting it" $ do
        shouldWarn ["unbounded-growth"] (testC (queue [] ++ receive []))
            [[ "error: nothing limits how far `q->head` grows; reached from `queue_poll` [-Wunbounded-growth]"
             , "   --> test.c:15:6"
             , "    |"
             , "  15|   q->head = item;"
             , "    |      ^^^^"
             , "    |      |"
             , "    |      grows here"
             , "  22| void queue_poll(Queue *q, int sock) {"
             , "    |      ^^^^^^^^^^"
             , "    |      |"
             , "    |      packets come in here"
             , "    = note: through `queue_poll` -> `queue_add`"
             ]]

    it "accepts a list with a limit where it grows" $ do
        shouldAccept ["unbounded-growth"] (testC (queue
            [ "  if (q->length >= 64) {"
            , "    return false;"
            , "  }"
            ] ++ receive []))

    it "accepts a list a caller limits" $ do
        shouldAccept ["unbounded-growth"] (testC (queue [] ++ receive
            [ "  if (q->length >= 64) {"
            , "    return;"
            , "  }"
            ]))

    it "doesn't take a check against the list's own capacity as a limit" $ do
        shouldWarn ["unbounded-growth"] (testC (queue [] ++ receive
            [ "  if (q->length == q->capacity) {"
            , "    return;"
            , "  }"
            ]))
            [[ "error: nothing limits how far `q->head` grows; reached from `queue_poll` [-Wunbounded-growth]"
             , "   --> test.c:15:6"
             , "    |"
             , "  15|   q->head = item;"
             , "    |      ^^^^"
             , "    |      |"
             , "    |      grows here"
             , "  22| void queue_poll(Queue *q, int sock) {"
             , "    |      ^^^^^^^^^^"
             , "    |      |"
             , "    |      packets come in here"
             , "    = note: through `queue_poll` -> `queue_add`"
             ]]

    it "accepts a list only the program's own calls make grow" $ do
        shouldAccept ["unbounded-growth"] (testC (queue [] ++
            [ "void queue_fill(Queue *q, uint32_t n) {"
            , "  for (uint32_t i = 0; i < n; ++i) {"
            , "    queue_add(q);"
            , "  }"
            , "}"
            ]))
