{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.WraparoundSpec (spec) where
import           Data.Text             (Text)
import           Test.Hspec            (Spec, it)

import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["wraparound"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["wraparound"]


prelude :: [Text]
prelude =
    [ "typedef unsigned char uint8_t;"
    , "typedef unsigned short uint16_t;"
    , "typedef unsigned int uint32_t;"
    , "typedef struct Conn { uint16_t start; uint32_t id; uint8_t slots[2048]; } Conn;"
    ]


spec :: Spec
spec = do
    it "warns about a ring index decremented modulo the wrong size" $ do
        shouldWarn' (prelude ++
            [ "int full(const Conn *c) {"
            , "  return (c->id % 2048) == (uint16_t)(c->start - 1);"
            , "}"
            ])
            [[ "error: `(uint16_t) (c->start - 1)` can wrap around to 65535, but `c->id % 2048` is always between 0 and 2047, so the comparison is always false when it wraps [-Wwraparound]"
             , "   --> test.c:6:28"
             , "    |"
             , "   6|   return (c->id % 2048) == (uint16_t)(c->start - 1);"
             , "    |                            ^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "warns about unsigned subtraction without a cast" $ do
        shouldWarn' (prelude ++
            [ "int full(uint32_t id, uint32_t start) {"
            , "  return start - 1 != id % 2048;"
            , "}"
            ])
            [[ "error: `start - 1` can wrap around to 4294967295, but `id % 2048` is always between 0 and 2047, so the comparison is always true when it wraps [-Wwraparound]"
             , "   --> test.c:6:10"
             , "    |"
             , "   6|   return start - 1 != id % 2048;"
             , "    |          ^^^^^^^^^"
             ]]

    it "follows wrapped values through locals" $ do
        shouldWarn' (prelude ++
            [ "int full(const Conn *c) {"
            , "  const uint16_t prev = c->start - 1;"
            , "  const uint32_t idx = c->id % 2048;"
            , "  return idx == prev;"
            , "}"
            ])
            [[ "error: `prev` can wrap around to 65535, but `idx` is always between 0 and 2047, so the comparison is always false when it wraps [-Wwraparound]"
             , "   --> test.c:8:17"
             , "    |"
             , "   6|   const uint16_t prev = c->start - 1;"
             , "    |                         ^^^^^^^^^^^^"
             , "    |                         |"
             , "    |                         wraps around here"
             , "   8|   return idx == prev;"
             , "    |                 ^^^^"
             ]]

    it "warns about a wrapped value reduced modulo a size that doesn't divide its range" $ do
        shouldWarn' (prelude ++
            [ "int used(const uint8_t *relays, uint16_t counter) {"
            , "  int n = 0;"
            , "  for (unsigned i = 0; i < 24; ++i) {"
            , "    n += relays[(counter - (i + 1)) % (6 * 4)];"
            , "  }"
            , "  return n;"
            , "}"
            ])
            [[ "error: `counter - (i + 1)` can wrap around to values from 4294967272 to 4294967295, and 4294967296 is not a multiple of 24, so `% 24` then gives a different index than it would without the wrap [-Wwraparound]"
             , "   --> test.c:8:18"
             , "    |"
             , "   8|     n += relays[(counter - (i + 1)) % (6 * 4)];"
             , "    |                  ^^^^^^^^^^^^^^^^^"
             ]]

    it "warns about a wrapped index past the end of an array" $ do
        shouldWarn' (prelude ++
            [ "uint8_t last(const Conn *c) {"
            , "  return c->slots[(uint16_t)(c->start - 1)];"
            , "}"
            ])
            [[ "error: `(uint16_t) (c->start - 1)` can wrap around to 65535, past the end of `c->slots`, which has 2048 elements [-Wwraparound]"
             , "   --> test.c:6:19"
             , "    |"
             , "   6|   return c->slots[(uint16_t)(c->start - 1)];"
             , "    |                   ^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "forgets facts about fields when a call can change them" $ do
        shouldWarn' (prelude ++
            [ "void reset(Conn *c);"
            , "int full(Conn *c) {"
            , "  if (c->start == 0) {"
            , "    return 0;"
            , "  }"
            , "  reset(c);"
            , "  return (c->id % 2048) == (uint16_t)(c->start - 1);"
            , "}"
            ])
            [[ "error: `(uint16_t) (c->start - 1)` can wrap around to 65535, but `c->id % 2048` is always between 0 and 2047, so the comparison is always false when it wraps [-Wwraparound]"
             , "   --> test.c:11:28"
             , "    |"
             , "  11|   return (c->id % 2048) == (uint16_t)(c->start - 1);"
             , "    |                            ^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "doesn't assume a static local still has its initial value" $ do
        shouldWarn' (prelude ++
            [ "int full(const Conn *c) {"
            , "  static uint16_t start = 1;"
            , "  const int r = (c->id % 2048) == (uint16_t)(start - 1);"
            , "  start = c->start;"
            , "  return r;"
            , "}"
            ])
            [[ "error: `(uint16_t) (start - 1)` can wrap around to 65535, but `c->id % 2048` is always between 0 and 2047, so the comparison is always false when it wraps [-Wwraparound]"
             , "   --> test.c:7:35"
             , "    |"
             , "   7|   const int r = (c->id % 2048) == (uint16_t)(start - 1);"
             , "    |                                   ^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "warns about the difference of 16-bit values wrapping around at 32 bits" $ do
        shouldWarn' (prelude ++
            [ "int late(uint16_t seq, uint16_t bottom, uint32_t size) {"
            , "  return (uint32_t)(seq - bottom) > size;"
            , "}"
            ])
            [[ "error: `seq - bottom` can wrap around to values from 4294901761 to 4294967295, because it is the difference of 16-bit values but wraps around at 32 bits here [-Wwraparound]"
             , "   --> test.c:6:21"
             , "    |"
             , "   6|   return (uint32_t)(seq - bottom) > size;"
             , "    |                     ^^^^^^^^^^^^"
             ]]

    it "accepts the difference of 16-bit values wrapping around at 16 bits" $ do
        shouldAccept' (prelude ++
            [ "int late(uint16_t seq, uint16_t bottom, uint32_t size) {"
            , "  return (uint16_t)(seq - bottom) > size;"
            , "}"
            ])

    it "knows a field holds an index another function reduces" $ do
        shouldWarn' (prelude ++
            [ "void advance(Conn *c) {"
            , "  c->start = (c->start + 1) % 2048;"
            , "}"
            , "int full(const Conn *c, uint16_t id) {"
            , "  return c->start == (uint16_t)(id - 1);"
            , "}"
            ])
            [[ "error: `(uint16_t) (id - 1)` can wrap around to 65535, but `c->start` is always between 0 and 2047, so the comparison is always false when it wraps [-Wwraparound]"
             , "   --> test.c:9:22"
             , "    |"
             , "   9|   return c->start == (uint16_t)(id - 1);"
             , "    |                      ^^^^^^^^^^^^^^^^^^"
             ]]

    it "forgets what a field holds once its address escapes" $ do
        shouldAccept' (prelude ++
            [ "void load(uint16_t *value);"
            , "void advance(Conn *c) {"
            , "  c->start = (c->start + 1) % 2048;"
            , "  load(&c->start);"
            , "}"
            , "int full(const Conn *c, uint16_t id) {"
            , "  return c->start == (uint16_t)(id - 1);"
            , "}"
            ])

    it "forgets what a struct's fields hold once a function it doesn't see can write them" $ do
        shouldAccept' (prelude ++
            [ "void load(Conn *c);"
            , "void advance(Conn *c) {"
            , "  c->start = (c->start + 1) % 2048;"
            , "  load(c);"
            , "}"
            , "int full(const Conn *c, uint16_t id) {"
            , "  return c->start == (uint16_t)(id - 1);"
            , "}"
            ])

    it "knows what a function returns" $ do
        shouldWarn' (prelude ++
            [ "static uint16_t index_of(uint32_t id) {"
            , "  return id % 2048;"
            , "}"
            , "int full(uint32_t id, uint16_t start) {"
            , "  return index_of(id) == (uint16_t)(start - 1);"
            , "}"
            ])
            [[ "error: `(uint16_t) (start - 1)` can wrap around to 65535, but `index_of(id)` is always between 0 and 2047, so the comparison is always false when it wraps [-Wwraparound]"
             , "   --> test.c:9:26"
             , "    |"
             , "   9|   return index_of(id) == (uint16_t)(start - 1);"
             , "    |                          ^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "knows what a static function's callers pass" $ do
        shouldAccept' (prelude ++
            [ "static uint8_t before(const Conn *c, uint16_t i) {"
            , "  return c->slots[(uint16_t)(i - 1)];"
            , "}"
            , "uint8_t first(const Conn *c) {"
            , "  return before(c, 1);"
            , "}"
            ])

    it "doesn't assume what callers pass to a function whose address escapes" $ do
        shouldWarn' (prelude ++
            [ "static uint8_t before(const Conn *c, uint16_t i) {"
            , "  return c->slots[(uint16_t)(i - 1)];"
            , "}"
            , "uint8_t (*const get)(const Conn *c, uint16_t i) = before;"
            , "uint8_t first(const Conn *c) {"
            , "  return before(c, 1);"
            , "}"
            ])
            [[ "error: `(uint16_t) (i - 1)` can wrap around to 65535, past the end of `c->slots`, which has 2048 elements [-Wwraparound]"
             , "   --> test.c:6:19"
             , "    |"
             , "   6|   return c->slots[(uint16_t)(i - 1)];"
             , "    |                   ^^^^^^^^^^^^^^^^^"
             ]]

    it "knows a difference can't wrap after a guard relating its operands" $ do
        shouldAccept' (prelude ++
            [ "uint8_t rest(const Conn *c, uint16_t size, uint16_t offset) {"
            , "  if (size < offset) {"
            , "    return 0;"
            , "  }"
            , "  return c->slots[(uint16_t)(size - offset)];"
            , "}"
            ])

    it "knows a loop bound is past the index" $ do
        shouldAccept' (prelude ++
            [ "uint8_t last(const Conn *c, uint16_t count) {"
            , "  uint8_t x = 0;"
            , "  for (uint16_t i = 0; i < count; ++i) {"
            , "    x = c->slots[(uint16_t)(count - i - 1)];"
            , "  }"
            , "  return x;"
            , "}"
            ])

    it "forgets how paths relate after writing one of them" $ do
        shouldWarn' (prelude ++
            [ "uint8_t rest(const Conn *c, uint16_t size, uint16_t offset, uint16_t other) {"
            , "  if (size < offset) {"
            , "    return 0;"
            , "  }"
            , "  offset = other;"
            , "  return c->slots[(uint16_t)(size - offset)];"
            , "}"
            ])
            [[ "error: `(uint16_t) (size - offset)` can wrap around to values from 1 to 65535, past the end of `c->slots`, which has 2048 elements [-Wwraparound]"
             , "   --> test.c:10:19"
             , "    |"
             , "  10|   return c->slots[(uint16_t)(size - offset)];"
             , "    |                   ^^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "accepts a decrement modulo the ring size" $ do
        shouldAccept' (prelude ++
            [ "int full(const Conn *c) {"
            , "  return (c->id % 2048) == (c->start + 2048 - 1) % 2048;"
            , "}"
            ])

    it "accepts a wrapped value reduced modulo a divisor of its range" $ do
        shouldAccept' (prelude ++
            [ "int full(const Conn *c) {"
            , "  return (c->id % 2048) == (uint16_t)(c->start - 1) % 2048;"
            , "}"
            ])

    it "accepts a decrement guarded against wrapping" $ do
        shouldAccept' (prelude ++
            [ "int full(const Conn *c) {"
            , "  if (c->start == 0) {"
            , "    return 0;"
            , "  }"
            , "  return (c->id % 2048) == (uint16_t)(c->start - 1);"
            , "}"
            ])

    it "accepts a decrement in a switch case that excludes wrapping" $ do
        shouldAccept' (prelude ++
            [ "int full(const Conn *c) {"
            , "  switch (c->start) {"
            , "    case 0:"
            , "      return 0;"
            , "    default:"
            , "      return (c->id % 2048) == (uint16_t)(c->start - 1);"
            , "  }"
            , "}"
            ])

    it "accepts a decrement in a loop that stops before wrapping" $ do
        shouldAccept' (prelude ++
            [ "int find(const Conn *c, uint32_t n) {"
            , "  while (n > 0) {"
            , "    if ((c->id % 2048) == n - 1) {"
            , "      return 1;"
            , "    }"
            , "    --n;"
            , "  }"
            , "  return 0;"
            , "}"
            ])

    it "checks the first iteration of a do-while loop before its condition" $ do
        shouldWarn' (prelude ++
            [ "int find(const Conn *c, uint32_t n) {"
            , "  do {"
            , "    if ((c->id % 2048) == n - 1) {"
            , "      return 1;"
            , "    }"
            , "  } while (--n > 0);"
            , "  return 0;"
            , "}"
            ])
            [[ "error: `n - 1` can wrap around to 4294967295, but `c->id % 2048` is always between 0 and 2047, so the comparison is always false when it wraps [-Wwraparound]"
             , "   --> test.c:7:27"
             , "    |"
             , "   7|     if ((c->id % 2048) == n - 1) {"
             , "    |                           ^^^^^"
             ]]

    it "accepts the unsigned range check idiom" $ do
        shouldAccept' (prelude ++
            [ "extern const uint16_t UINT16_MAX;"
            , "int fits(uint32_t length) {"
            , "  return length - 1 < UINT16_MAX;"
            , "}"
            ])

    it "accepts comparisons where the other side can have any value" $ do
        shouldAccept' (prelude ++
            [ "int next(uint16_t a, uint16_t b) {"
            , "  return a == (uint16_t)(b - 1);"
            , "}"
            ])
