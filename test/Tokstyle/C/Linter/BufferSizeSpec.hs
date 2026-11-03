{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.BufferSizeSpec (spec) where
import           Data.Text             (Text)
import           Test.Hspec            (Spec, it)

import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["buffer-size"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["buffer-size"]


prelude :: [Text]
prelude =
    [ "typedef unsigned char uint8_t;"
    , "typedef unsigned short uint16_t;"
    , "typedef unsigned int uint32_t;"
    , "void *memcpy(void *dst, const void *src, unsigned long n);"
    , "void *memset(void *dst, int c, unsigned long n);"
    , "void *alloca(unsigned long size);"
    , "void send(const uint8_t *data, uint16_t length);"
    ]


spec :: Spec
spec = do
    it "warns about a copy into an array whose size wraps around" $ do
        shouldWarn' (prelude ++
            [ "void f(const uint8_t *data, uint32_t length) {"
            , "  const uint16_t size = length + 81;"
            , "  uint8_t buf[size];"
            , "  memcpy(buf + 1, data, length);"
            , "  send(buf, size);"
            , "}"
            ])
            [[ "error: `size` can wrap around to values from 0 to 65535, so `buf` can be smaller than the 4294967296 bytes `memcpy` reaches [-Wbuffer-size]"
             , "   --> test.c:11:3"
             , "    |"
             , "   9|   const uint16_t size = length + 81;"
             , "    |                         ^^^^^^^^^^^"
             , "    |                         |"
             , "    |                         wraps around here"
             , "  11|   memcpy(buf + 1, data, length);"
             , "    |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "warns about an index into allocated memory whose size wraps around" $ do
        shouldWarn' (prelude ++
            [ "void f(uint32_t length) {"
            , "  const uint16_t size = length + 81;"
            , "  uint8_t *const buf = (uint8_t *)alloca(size * sizeof(uint8_t));"
            , "  buf[0] = 1;"
            , "  send(buf, size);"
            , "}"
            ])
            [[ "error: `size` can wrap around to values from 0 to 65535, so `buf` can be smaller than the 1 byte `buf[0]` reaches [-Wbuffer-size]"
             , "   --> test.c:11:3"
             , "    |"
             , "   9|   const uint16_t size = length + 81;"
             , "    |                         ^^^^^^^^^^^"
             , "    |                         |"
             , "    |                         wraps around here"
             , "  11|   buf[0] = 1;"
             , "    |   ^^^^^^"
             ]]

    it "accepts filling exactly the buffer's size" $ do
        shouldAccept' (prelude ++
            [ "void f(uint32_t length) {"
            , "  const uint16_t size = length + 81;"
            , "  uint8_t buf[size];"
            , "  memset(buf, 0, size);"
            , "  send(buf, size);"
            , "}"
            ])

    it "accepts a size limited before it is narrowed" $ do
        shouldAccept' (prelude ++
            [ "void f(const uint8_t *data, uint32_t length) {"
            , "  if (length > 1000) {"
            , "    return;"
            , "  }"
            , "  const uint16_t size = length + 81;"
            , "  uint8_t buf[size];"
            , "  memcpy(buf + 1, data, length);"
            , "  send(buf, size);"
            , "}"
            ])

    it "accepts a size whose parts a guard limits" $ do
        shouldAccept' (prelude ++
            [ "void f(const uint8_t *data, uint16_t length, uint16_t max) {"
            , "  if (1 + length + 81 > max) {"
            , "    return;"
            , "  }"
            , "  const uint16_t size = length + 81;"
            , "  uint8_t buf[size];"
            , "  memcpy(buf + 1, data, length);"
            , "  send(buf, size);"
            , "}"
            ])

    it "knows the limits of integer types declared as objects" $ do
        shouldAccept' (prelude ++
            [ "extern const uint32_t UINT32_MAX;"
            , "void *mem_balloc(const void *mem, uint32_t size);"
            , "void f(const void *mem, const uint8_t *data, uint32_t length) {"
            , "  if (length == UINT32_MAX) {"
            , "    return;"
            , "  }"
            , "  uint8_t *const buf = (uint8_t *)mem_balloc(mem, length + 1);"
            , "  memcpy(buf, data, length);"
            , "}"
            ])

    it "follows a local whose address only a call gets" $ do
        shouldAccept' (prelude ++
            [ "extern const uint32_t UINT32_MAX;"
            , "void *mem_balloc(const void *mem, uint32_t size);"
            , "int read_size(uint32_t *size);"
            , "void f(const void *mem, const uint8_t *data) {"
            , "  uint32_t length;"
            , "  if (!read_size(&length) || length == UINT32_MAX) {"
            , "    return;"
            , "  }"
            , "  uint8_t *const buf = (uint8_t *)mem_balloc(mem, length + 1);"
            , "  memcpy(buf, data, length);"
            , "}"
            ])

    it "knows a call to a function that never returns ends the path" $ do
        shouldAccept' (prelude ++
            [ "void abort(void);"
            , "void f(const uint8_t *data, uint32_t length) {"
            , "  if (length > 1000) {"
            , "    abort();"
            , "  }"
            , "  const uint16_t size = length + 81;"
            , "  uint8_t buf[size];"
            , "  memcpy(buf + 1, data, length);"
            , "  send(buf, size);"
            , "}"
            ])

    it "warns about a call that reaches past memory whose size wraps around" $ do
        shouldWarn' (prelude ++
            [ "void seal(const uint8_t *plain, uint32_t length, uint8_t *sealed) {"
            , "  memcpy(sealed, plain, length + 16);"
            , "}"
            , "void f(const uint8_t *data, uint16_t length) {"
            , "  const uint16_t size = length + 16;"
            , "  uint8_t buf[size];"
            , "  seal(data, length, buf);"
            , "  send(buf, size);"
            , "}"
            ])
            [[ "error: `size` can wrap around to values from 0 to 15, so `buf` can be smaller than the 65551 bytes `seal` reaches [-Wbuffer-size]"
             , "   --> test.c:14:3"
             , "    |"
             , "  12|   const uint16_t size = length + 16;"
             , "    |                         ^^^^^^^^^^^"
             , "    |                         |"
             , "    |                         wraps around here"
             , "  14|   seal(data, length, buf);"
             , "    |   ^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "accepts a call that reaches no further than the memory it gets" $ do
        shouldAccept' (prelude ++
            [ "void seal(const uint8_t *plain, uint32_t length, uint8_t *sealed) {"
            , "  memcpy(sealed, plain, length + 16);"
            , "}"
            , "void f(const uint8_t *data, uint16_t length) {"
            , "  if (length > 1000) {"
            , "    return;"
            , "  }"
            , "  const uint16_t size = length + 16;"
            , "  uint8_t buf[size];"
            , "  seal(data, length, buf);"
            , "  send(buf, size);"
            , "}"
            ])

    it "knows a function reaches a multiple of an argument" $ do
        shouldWarn' (prelude ++
            [ "void fill(uint8_t *out, uint16_t n) {"
            , "  memset(out, 0, n * 4);"
            , "}"
            , "void f(uint16_t length) {"
            , "  const uint16_t size = length + 16;"
            , "  uint8_t buf[size];"
            , "  fill(buf, length);"
            , "  send(buf, size);"
            , "}"
            ])
            [[ "error: `size` can wrap around to values from 0 to 15, so `buf` can be smaller than the 262140 bytes `fill` reaches [-Wbuffer-size]"
             , "   --> test.c:14:3"
             , "    |"
             , "  12|   const uint16_t size = length + 16;"
             , "    |                         ^^^^^^^^^^^"
             , "    |                         |"
             , "    |                         wraps around here"
             , "  14|   fill(buf, length);"
             , "    |   ^^^^^^^^^^^^^^^^^"
             ]]

    it "knows a function reaches no further than the size argument it checks" $ do
        shouldAccept' (prelude ++
            [ "int make(uint8_t *data, uint32_t maxlen, uint16_t size) {"
            , "  if (maxlen < sizeof(uint16_t) + size) {"
            , "    return -1;"
            , "  }"
            , "  memset(data, 0, sizeof(uint16_t) + size);"
            , "  return 0;"
            , "}"
            , "void f(uint16_t size) {"
            , "  const uint16_t length = sizeof(uint16_t) + size;"
            , "  uint8_t buf[length];"
            , "  make(buf, length, size);"
            , "  send(buf, length);"
            , "}"
            ])

    it "knows a value equals a narrower size it was compared with" $ do
        shouldAccept' (prelude ++
            [ "int fill(uint8_t *out, uint16_t n);"
            , "void f(uint8_t *dst, uint16_t n) {"
            , "  const uint16_t size = n + 16;"
            , "  uint8_t buf[size];"
            , "  const int len = fill(buf, n);"
            , "  if (len != size) {"
            , "    return;"
            , "  }"
            , "  memcpy(dst, buf, len);"
            , "}"
            ])

    it "knows a widening cast keeps a size's value" $ do
        shouldAccept' (prelude ++
            [ "int fill(uint8_t *out, uint16_t n);"
            , "void f(uint8_t *dst, uint16_t n) {"
            , "  const uint16_t size = n + 16;"
            , "  uint8_t buf[size];"
            , "  const int len = fill(buf, n);"
            , "  if (len != (int)size) {"
            , "    return;"
            , "  }"
            , "  memcpy(dst, buf, len);"
            , "}"
            ])

    it "warns about a comparison past memory whose element count wraps around" $ do
        shouldWarn' (prelude ++
            [ "int memcmp(const void *a, const void *b, unsigned long n);"
            , "void *mem_valloc(const void *mem, uint32_t nmemb, uint32_t size);"
            , "int f(const void *mem, const uint8_t *data, uint16_t length) {"
            , "  const uint16_t size = length + 16;"
            , "  uint8_t *const buf = (uint8_t *)mem_valloc(mem, size, sizeof(uint8_t));"
            , "  return memcmp(buf, data, length);"
            , "}"
            ])
            [[ "error: `size` can wrap around to values from 0 to 15, so `buf` can be smaller than the 65535 bytes `memcmp` reaches [-Wbuffer-size]"
             , "   --> test.c:13:10"
             , "    |"
             , "  11|   const uint16_t size = length + 16;"
             , "    |                         ^^^^^^^^^^^"
             , "    |                         |"
             , "    |                         wraps around here"
             , "  13|   return memcmp(buf, data, length);"
             , "    |          ^^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "counts the bytes of elements larger than a byte" $ do
        shouldWarn' (prelude ++
            [ "void *mem_valloc(const void *mem, uint32_t nmemb, uint32_t size);"
            , "void f(const void *mem, const uint8_t *data, uint16_t length) {"
            , "  const uint16_t size = length + 16;"
            , "  uint8_t *const buf = (uint8_t *)mem_valloc(mem, size, 4);"
            , "  memcpy(buf, data, length);"
            , "}"
            ])
            [[ "error: `size` can wrap around to values from 0 to 15, so `buf` can be smaller than the 65535 bytes `memcpy` reaches [-Wbuffer-size]"
             , "   --> test.c:12:3"
             , "    |"
             , "  10|   const uint16_t size = length + 16;"
             , "    |                         ^^^^^^^^^^^"
             , "    |                         |"
             , "    |                         wraps around here"
             , "  12|   memcpy(buf, data, length);"
             , "    |   ^^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "accepts filling exactly the elements of a buffer" $ do
        shouldAccept' (prelude ++
            [ "void *mem_valloc(const void *mem, uint32_t nmemb, uint32_t size);"
            , "void f(const void *mem, uint16_t length) {"
            , "  const uint16_t size = length + 16;"
            , "  uint8_t *const buf = (uint8_t *)mem_valloc(mem, size, sizeof(uint32_t));"
            , "  memset(buf, 0, size * sizeof(uint32_t));"
            , "}"
            ])

    it "accepts filling exactly an array of a size times a constant" $ do
        shouldAccept' (prelude ++
            [ "void f(uint32_t length) {"
            , "  const uint16_t size = length + 81;"
            , "  uint8_t buf[size * 4];"
            , "  memset(buf, 0, size * 4);"
            , "  send(buf, size);"
            , "}"
            ])

    it "doesn't trust a size times a constant that wraps around" $ do
        shouldWarn' (prelude ++
            [ "void f(const uint8_t *data, uint32_t n) {"
            , "  uint8_t *const buf = (uint8_t *)alloca(n * 4);"
            , "  memcpy(buf, data, n);"
            , "}"
            ])
            [[ "error: `n * 4` can wrap around to values from 0 to 4294967295, so `buf` can be smaller than the 4294967295 bytes `memcpy` reaches [-Wbuffer-size]"
             , "   --> test.c:10:3"
             , "    |"
             , "   9|   uint8_t *const buf = (uint8_t *)alloca(n * 4);"
             , "    |                                          ^^^^^"
             , "    |                                          |"
             , "    |                                          wraps around here"
             , "  10|   memcpy(buf, data, n);"
             , "    |   ^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "knows how far a library function reaches" $ do
        shouldWarn' (prelude ++
            [ "int crypto_hash_sha256(uint8_t *out, const uint8_t *in, unsigned long long inlen);"
            , "void f(uint8_t *hash, uint16_t length) {"
            , "  const uint16_t size = length + 16;"
            , "  uint8_t buf[size];"
            , "  crypto_hash_sha256(hash, buf, length);"
            , "}"
            ])
            [[ "error: `size` can wrap around to values from 0 to 15, so `buf` can be smaller than the 65535 bytes `crypto_hash_sha256` reaches [-Wbuffer-size]"
             , "   --> test.c:12:3"
             , "    |"
             , "  10|   const uint16_t size = length + 16;"
             , "    |                         ^^^^^^^^^^^"
             , "    |                         |"
             , "    |                         wraps around here"
             , "  12|   crypto_hash_sha256(hash, buf, length);"
             , "    |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]
