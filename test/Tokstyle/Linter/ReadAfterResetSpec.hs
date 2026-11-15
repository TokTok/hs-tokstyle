{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.ReadAfterResetSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldWarn, testC)


shouldWarn' :: [(FilePath, [Text])] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["read-after-reset"]


shouldAccept' :: [(FilePath, [Text])] -> IO ()
shouldAccept' = shouldAccept ["read-after-reset"]


clearEntry :: [Text]
clearEntry =
    [ "static void clear_entry(Entry *entry) {"
    , "  free(entry->data);"
    , "  *entry = (Entry){nullptr};"
    , "}"
    ]


spec :: Spec
spec = do
    it "warns about a field read after a call cleared its object" $ do
        shouldWarn' (testC $ clearEntry ++
            [ "bool handle(Chat *chat, Entry *entry) {"
            , "  clear_entry(entry);"
            , "  if (chat == nullptr) {"
            , "    return false;"
            , "  }"
            , "  send_ack(chat, entry->id);"
            , "  return true;"
            , "}"
            ])
            [[ "error: `entry->id` is read after `clear_entry` overwrote `*entry` [-Wread-after-reset]"
             , "   --> test.c:10:18"
             , "    |"
             , "  10|   send_ack(chat, entry->id);"
             , "    |                  ^^^^^^^^^"
             ]]

    it "warns through a function that calls the one that clears" $ do
        shouldWarn' (testC $ clearEntry ++
            [ "static void reset(Entry *entry) {"
            , "  clear_entry(entry);"
            , "}"
            , "uint32_t handle(Entry *entry) {"
            , "  reset(entry);"
            , "  return entry->id;"
            , "}"
            ])
            [[ "error: `entry->id` is read after `reset` overwrote `*entry` [-Wread-after-reset]"
             , "   --> test.c:10:10"
             , "    |"
             , "  10|   return entry->id;"
             , "    |          ^^^^^^^^^"
             ]]

    it "accepts a field read before the call" $ do
        shouldAccept' (testC $ clearEntry ++
            [ "void handle(Chat *chat, Entry *entry) {"
            , "  const uint32_t id = entry->id;"
            , "  clear_entry(entry);"
            , "  send_ack(chat, id);"
            , "}"
            ])

    it "accepts a field set again before it is read" $ do
        shouldAccept' (testC $ clearEntry ++
            [ "uint32_t handle(Entry *entry, uint32_t id) {"
            , "  clear_entry(entry);"
            , "  entry->id = id;"
            , "  return entry->id;"
            , "}"
            ])

    it "accepts a field the clearing function sets again" $ do
        shouldAccept' (testC
            [ "static void reset_entry(Entry *entry, uint32_t id) {"
            , "  *entry = (Entry){nullptr};"
            , "  entry->id = id;"
            , "}"
            , "uint32_t handle(Entry *entry) {"
            , "  reset_entry(entry, 1);"
            , "  return entry->id;"
            , "}"
            ])

    it "accepts reads after the object is passed to another function" $ do
        shouldAccept' (testC $ clearEntry ++
            [ "uint32_t handle(Entry *entry) {"
            , "  clear_entry(entry);"
            , "  fill_entry(entry);"
            , "  return entry->id;"
            , "}"
            ])

    it "accepts reads of another object in the same variable" $ do
        shouldAccept' (testC $ clearEntry ++
            [ "uint32_t handle(Entry *entry, Entry *next) {"
            , "  clear_entry(entry);"
            , "  entry = next;"
            , "  return entry->id;"
            , "}"
            ])

    it "accepts reads in a path that doesn't run after the call" $ do
        shouldAccept' (testC $ clearEntry ++
            [ "uint32_t handle(Entry *entry) {"
            , "  clear_entry(entry);"
            , "  return 0;"
            , "  return entry->id;"
            , "}"
            ])

    it "accepts an array field passed to be written" $ do
        shouldAccept' (testC $
            [ "typedef struct Entry { uint8_t key[32]; } Entry;"
            ] ++ clearEntry ++
            [ "void handle(Entry *entry, const uint8_t *key) {"
            , "  clear_entry(entry);"
            , "  memcpy(entry->key, key, 32);"
            , "}"
            ])

    it "accepts a field a loop grows from its cleared value" $ do
        shouldAccept' (testC $ clearEntry ++
            [ "bool load(Entry *entry, uint32_t count) {"
            , "  clear_entry(entry);"
            , "  for (uint32_t i = 0; i < count; ++i) {"
            , "    uint8_t *data = (uint8_t *)realloc(entry->data, i + 1);"
            , "    if (data == nullptr) {"
            , "      return false;"
            , "    }"
            , "    entry->data = data;"
            , "  }"
            , "  return true;"
            , "}"
            ])

    it "accepts reads after a function copied another object in" $ do
        shouldAccept' (testC
            [ "static void copy_entry(Entry *dst, const Entry *src) {"
            , "  *dst = *src;"
            , "}"
            , "uint32_t handle(Entry *entry, const Entry *other) {"
            , "  copy_entry(entry, other);"
            , "  return entry->id;"
            , "}"
            ])

    it "warns after a function set its argument to an empty object" $ do
        shouldWarn' (testC
            [ "static const Entry empty_entry = {0};"
            , "static void setup(Entry *entry) {"
            , "  *entry = empty_entry;"
            , "}"
            , "uint32_t handle(Entry *entry) {"
            , "  setup(entry);"
            , "  return entry->id;"
            , "}"
            ])
            [[ "error: `entry->id` is read after `setup` overwrote `*entry` [-Wread-after-reset]"
             , "   --> test.c:7:10"
             , "    |"
             , "   7|   return entry->id;"
             , "    |          ^^^^^^^^^"
             ]]

    it "accepts reads after a function filled its argument from its parameters" $ do
        shouldAccept' (testC
            [ "static void init_entry(Entry *entry, uint32_t id) {"
            , "  *entry = (Entry){id};"
            , "}"
            , "uint32_t handle(Entry *entry) {"
            , "  init_entry(entry, 1);"
            , "  return entry->id;"
            , "}"
            ])

    it "warns in a case after one that breaks" $ do
        shouldWarn' (testC $ clearEntry ++
            [ "uint32_t handle(Entry *entry, int type) {"
            , "  clear_entry(entry);"
            , "  switch (type) {"
            , "    case 0:"
            , "      break;"
            , "    case 1:"
            , "      return entry->id;"
            , "  }"
            , "  return 0;"
            , "}"
            ])
            [[ "error: `entry->id` is read after `clear_entry` overwrote `*entry` [-Wread-after-reset]"
             , "   --> test.c:11:14"
             , "    |"
             , "  11|       return entry->id;"
             , "    |              ^^^^^^^^^"
             ]]
