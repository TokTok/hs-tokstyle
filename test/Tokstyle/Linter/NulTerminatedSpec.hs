{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.NulTerminatedSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldWarn)


shouldAccept' :: [(FilePath, [Text])] -> IO ()
shouldAccept' = shouldAccept ["nul-terminated"]


spec :: Spec
spec = do
    it "warns on bytes copied to the end of an allocation, through fields and parameters" $
        shouldWarn ["nul-terminated"]
            [ ("entry.h",
                [ "typedef struct Entry {"
                , "    uint8_t *data;"
                , "    uint16_t length;"
                , "} Entry;"
                ])
            , ("store.c",
                [ "static bool store(const Memory *mem, Entry *entry, const uint8_t *data, uint16_t length)"
                , "{"
                , "    uint8_t *copy = (uint8_t *)mem_balloc(mem, length);"
                , "    if (copy == nullptr) {"
                , "        return false;"
                , "    }"
                , "    memcpy(copy, data, length);"
                , "    entry->data = copy;"
                , "    entry->length = length;"
                , "    return true;"
                , "}"
                , "void process(Logger *log, const Entry *entry)"
                , "{"
                , "    handle(log, entry->data, entry->length);"
                , "}"
                ])
            , ("handle.c",
                [ "static void handle_nick(Logger *log, const uint8_t *nick, uint16_t length)"
                , "{"
                , "    LOGGER_WARNING(log, \"bad nick: %s (%u)\", nick, length);"
                , "}"
                , "void handle(Logger *log, const uint8_t *data, uint16_t length)"
                , "{"
                , "    const uint8_t *message = data + 1;"
                , "    handle_nick(log, message, length - 1);"
                , "}"
                ])
            ]
            [[ "warning: `%s` reads `nick` up to a NUL byte, and there can be none before the end of the object it points into [-Wnul-terminated]"
             , "   --> handle.c:3:46"
             , "    |"
             , "   3|     LOGGER_WARNING(log, \"bad nick: %s (%u)\", nick, length);"
             , "    |                                              ^^^^"
             , "    |                                              |"
             , "    |                                              read up to a NUL here"
             , "   7|     const uint8_t *message = data + 1;"
             , "    |                              ^^^^^^^^"
             , "    |                              |"
             , "    |                              `message` is `data + 1` here"
             , "   8|     handle_nick(log, message, length - 1);"
             , "    |                      ^^^^^^^"
             , "    |                      |"
             , "    |                      passed to `handle_nick` as `nick` here"
             , "   ::: store.c:7:5"
             , "    |"
             , "   7|     memcpy(copy, data, length);"
             , "    |     ^^^^^^^^^^^^^^^^^^^^^^^^^"
             , "    |     |"
             , "    |     filled with `length` bytes here, as many as `copy` holds"
             , "   8|     entry->data = copy;"
             , "    |     ^^^^^^^^^^^^^^^^^^"
             , "    |     |"
             , "    |     stored in `Entry.data` here"
             , "  14|     handle(log, entry->data, entry->length);"
             , "    |                 ^^^^^^^^^^^"
             , "    |                 |"
             , "    |                 passed to `handle` as `data` here"
             , "    = note: its bytes are a copy that can fill the object to its end, and nothing writes a NUL into it"
             , "    = help: print it with `%.*s` and its length, or don't print its bytes"
             ]]

    it "warns on an array field a copy can fill" $
        shouldWarn ["nul-terminated"]
            [ ("friend.c",
                [ "#define MAX_NAME_LENGTH 128"
                , "typedef struct Friend {"
                , "    uint8_t name[MAX_NAME_LENGTH];"
                , "    uint16_t name_length;"
                , "} Friend;"
                , "int set_name(Friend *f, const uint8_t *name, uint16_t length)"
                , "{"
                , "    if (length > MAX_NAME_LENGTH || length == 0) {"
                , "        return -1;"
                , "    }"
                , "    memcpy(f->name, name, length);"
                , "    f->name_length = length;"
                , "    return 0;"
                , "}"
                , "void dump(Logger *log, const Friend *f)"
                , "{"
                , "    LOGGER_TRACE(log, \"<%s>\", f->name);"
                , "}"
                ])
            ]
            [[ "warning: `%s` reads `f->name` up to a NUL byte, and there can be none before the end of the object it points into [-Wnul-terminated]"
             , "   --> friend.c:17:31"
             , "    |"
             , "   8|     if (length > MAX_NAME_LENGTH || length == 0) {"
             , "    |         ^^^^^^^^^^^^^^^^^^^^^^^^"
             , "    |         |"
             , "    |         `length` can be `MAX_NAME_LENGTH` after this"
             , "  11|     memcpy(f->name, name, length);"
             , "    |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
             , "    |     |"
             , "    |     filled with `length` bytes here, as many as `Friend.name` holds"
             , "  17|     LOGGER_TRACE(log, \"<%s>\", f->name);"
             , "    |                               ^^^^^^^"
             , "    |                               |"
             , "    |                               read up to a NUL here"
             , "    = note: its bytes are a copy that can fill the object to its end, and nothing writes a NUL into it"
             , "    = help: print it with `%.*s` and its length, or don't print its bytes"
             ]]

    it "accepts bytes with a NUL written after them" $
        shouldAccept'
            [ ("test.c",
                [ "void handle(Logger *log, const uint8_t *data, uint16_t length)"
                , "{"
                , "    VLA(uint8_t, terminated, length + 1);"
                , "    memcpy(terminated, data, length);"
                , "    terminated[length] = 0;"
                , "    LOGGER_INFO(log, \"%s\", terminated);"
                , "}"
                ])
            ]

    it "accepts an array field a copy can't fill to its end" $
        shouldAccept'
            [ ("test.c",
                [ "#define MAX_NAME_LENGTH 128"
                , "typedef struct Friend {"
                , "    uint8_t name[MAX_NAME_LENGTH + 1];"
                , "} Friend;"
                , "int set_name(Friend *f, const uint8_t *name, uint16_t length)"
                , "{"
                , "    if (length > MAX_NAME_LENGTH) {"
                , "        return -1;"
                , "    }"
                , "    memcpy(f->name, name, length);"
                , "    return 0;"
                , "}"
                , "void dump(Logger *log, const Friend *f)"
                , "{"
                , "    LOGGER_TRACE(log, \"<%s>\", f->name);"
                , "}"
                ])
            ]

    it "accepts an allocation larger than what's copied into it" $
        shouldAccept'
            [ ("test.c",
                [ "void handle(Logger *log, const Memory *mem, const uint8_t *data, uint16_t length)"
                , "{"
                , "    uint8_t *copy = (uint8_t *)mem_balloc(mem, length + 1);"
                , "    memcpy(copy, data, length);"
                , "    LOGGER_INFO(log, \"%s\", copy);"
                , "}"
                ])
            ]

    it "accepts a precision, and strings" $
        shouldAccept'
            [ ("test.c",
                [ "static void handle_nick(Logger *log, const uint8_t *nick, uint16_t length)"
                , "{"
                , "    LOGGER_WARNING(log, \"bad nick: %.*s\", (int)length, nick);"
                , "}"
                , "static void handle_name(Logger *log, const char *name)"
                , "{"
                , "    LOGGER_WARNING(log, \"bad name: %s\", name);"
                , "}"
                , "void handle(Logger *log, const Memory *mem, const uint8_t *data, uint16_t length)"
                , "{"
                , "    uint8_t *copy = (uint8_t *)mem_balloc(mem, length);"
                , "    memcpy(copy, data, length);"
                , "    handle_nick(log, copy, length);"
                , "    handle_name(log, \"name\");"
                , "}"
                ])
            ]

    it "accepts what a function it doesn't know fills" $
        shouldAccept'
            [ ("test.c",
                [ "void handle(Logger *log, const Memory *mem, const uint8_t *data, uint16_t length)"
                , "{"
                , "    uint8_t *copy = (uint8_t *)mem_balloc(mem, length);"
                , "    memcpy(copy, data, length);"
                , "    terminate(copy, length);"
                , "    LOGGER_INFO(log, \"%s\", copy);"
                , "}"
                ])
            ]

    it "accepts a field a NUL is written into anywhere" $
        shouldAccept'
            [ ("test.c",
                [ "#define MAX_NAME_LENGTH 128"
                , "typedef struct Friend {"
                , "    uint8_t name[MAX_NAME_LENGTH];"
                , "} Friend;"
                , "int set_name(Friend *f, const uint8_t *name, uint16_t length)"
                , "{"
                , "    if (length > MAX_NAME_LENGTH) {"
                , "        return -1;"
                , "    }"
                , "    memcpy(f->name, name, length);"
                , "    return 0;"
                , "}"
                , "void terminate(Friend *f)"
                , "{"
                , "    f->name[MAX_NAME_LENGTH - 1] = '\\0';"
                , "}"
                , "void dump(Logger *log, const Friend *f)"
                , "{"
                , "    LOGGER_TRACE(log, \"<%s>\", f->name);"
                , "}"
                ])
            ]

    it "accepts a copy whose guard isn't on its way" $
        shouldAccept'
            [ ("test.c",
                [ "#define MAX_NAME_LENGTH 128"
                , "typedef struct Friend {"
                , "    uint8_t name[MAX_NAME_LENGTH];"
                , "} Friend;"
                , "int set_name(Friend *f, const uint8_t *name, uint16_t length, bool check)"
                , "{"
                , "    if (check) {"
                , "        if (length > MAX_NAME_LENGTH) {"
                , "            return -1;"
                , "        }"
                , "    }"
                , "    memcpy(f->name, name, length);"
                , "    return 0;"
                , "}"
                , "void dump(Logger *log, const Friend *f)"
                , "{"
                , "    LOGGER_TRACE(log, \"<%s>\", f->name);"
                , "}"
                ])
            ]

    it "accepts a copy of text, which brings its own NUL" $
        shouldAccept'
            [ ("test.c",
                [ "void hello(Logger *log)"
                , "{"
                , "    char text[6];"
                , "    memcpy(text, \"hello\", sizeof(text));"
                , "    LOGGER_INFO(log, \"%s\", text);"
                , "}"
                ])
            ]

    it "warns on a string a function can return without writing" $
        shouldWarn ["nul-terminated"]
            [ ("net.h", netH)
            , ("os_network.c", strerror
                [ "static const char *net_strerror_r(int error, char *tmp, size_t tmp_size)"
                , "{"
                , "    const char *retstr = strerror_r(error, tmp, tmp_size);"
                , "    if (errno != 0) {"
                , "        snprintf(tmp, tmp_size, \"error %d\", error);"
                , "    }"
                , "    return retstr;"
                , "}"
                ])
            , ("caller.c", caller)
            ]
            [[ "warning: `%s` reads `net_strerror(error, &error_str)` up to a NUL byte, and those bytes can be ones nothing wrote [-Wnul-terminated]"
             , "   --> caller.c:5:29"
             , "    |"
             , "   5|     LOGGER_ERROR(log, \"%s\", net_strerror(error, &error_str));"
             , "    |                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
             , "    |                             |"
             , "    |                             read up to a NUL here"
             , "    |                                                  ^^^^^^^^^"
             , "    |                                                  |"
             , "    |                                                  `error_str` is passed here before anything writes it"
             , "   ::: os_network.c:5:26"
             , "    |"
             , "   5|     const char *retstr = strerror_r(error, tmp, tmp_size);"
             , "    |                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
             , "    |                          |"
             , "    |                          with `_GNU_SOURCE`, this can return its own string without writing `tmp`"
             , "   6|     if (errno != 0) {"
             , "    |         ^^^^^^^^^^"
             , "    |         |"
             , "    |         `errno != 0` can be false here"
             , "  15|     const char *retstr = net_strerror_r(error, buf->data, NET_STRERROR_SIZE);"
             , "    |                                                ^^^^^^^^^"
             , "    |                                                |"
             , "    |                                                passed to `net_strerror_r` here"
             , "  17|     return buf->data;"
             , "    |            ^^^^^^^^^"
             , "    |            |"
             , "    |            returning `buf->data`"
             , "    = note: they're whatever was in that memory, which may have no NUL before its end"
             , "    = note: this is where `defined(_GNU_SOURCE) && defined(__GLIBC__)` holds"
             , "    = help: write the bytes on every path, or return the string that was written"
             ]]

    it "accepts what `strerror_r` returning an int writes" $
        shouldAccept'
            [ ("net.h", netH)
            , ("os_network.c", strerror
                [ "static const char *net_strerror_r(int error, char *tmp, size_t tmp_size)"
                , "{"
                , "    const int fmt_error = strerror_r(error, tmp, tmp_size);"
                , "    if (fmt_error != 0) {"
                , "        snprintf(tmp, tmp_size, \"error %d\", error);"
                , "    }"
                , "    return tmp;"
                , "}"
                ])
            , ("caller.c", caller)
            ]

    it "accepts copying the string `strerror_r` returns" $
        shouldAccept'
            [ ("net.h", netH)
            , ("os_network.c", strerror
                [ "static const char *net_strerror_r(int error, char *tmp, size_t tmp_size)"
                , "{"
                , "    const char *retstr = strerror_r(error, tmp, tmp_size);"
                , "    if (errno != 0) {"
                , "        snprintf(tmp, tmp_size, \"error %d\", error);"
                , "    } else if (retstr != tmp) {"
                , "        snprintf(tmp, tmp_size, \"%s\", retstr);"
                , "    }"
                , "    return tmp;"
                , "}"
                ])
            , ("caller.c", caller)
            ]

    it "accepts a function returning the string that was written" $
        shouldAccept'
            [ ("net.h", netH)
            , ("os_network.c",
                [ "#include \"net.h\""
                , "char *net_strerror(int error, Net_Strerror *buf)"
                , "{"
                , "    const char *retstr = strerror_r(error, buf->data, NET_STRERROR_SIZE);"
                , "    return retstr;"
                , "}"
                ])
            , ("caller.c", caller)
            ]

    it "accepts a buffer written before the call, or given a value" $
        shouldAccept'
            [ ("net.h", netH)
            , ("os_network.c", strerror gnuStrerror)
            , ("caller.c",
                [ "#include \"net.h\""
                , "static void recv_error(const Logger *log, int error)"
                , "{"
                , "    Net_Strerror error_str;"
                , "    memset(&error_str, 0, sizeof(error_str));"
                , "    LOGGER_ERROR(log, \"%s\", net_strerror(error, &error_str));"
                , "    Net_Strerror other = {0};"
                , "    LOGGER_ERROR(log, \"%s\", net_strerror(error, &other));"
                , "}"
                ])
            ]

    it "accepts not writing on a branch a caller decides" $
        shouldAccept'
            [ ("net.h", netH)
            , ("os_network.c",
                [ "#include \"net.h\""
                , "char *net_strerror(int error, Net_Strerror *buf)"
                , "{"
                , "    if (abs(error) != 0) {"
                , "        snprintf(buf->data, NET_STRERROR_SIZE, \"error %d\", error);"
                , "    }"
                , "    return buf->data;"
                , "}"
                ])
            , ("caller.c", caller)
            ]

    it "accepts a read only builds the function can't be in have" $
        shouldAccept'
            [ ("net.h", netH)
            , ("os_network.c",
                [ "#if !defined(OS_WIN32) && defined(_WIN32)"
                , "#define OS_WIN32"
                , "#endif /* _WIN32 */"
                , "#ifndef OS_WIN32"
                ] ++ strerror gnuStrerror ++
                [ "#endif /* OS_WIN32 */"
                ])
            , ("caller.c",
                [ "#include \"net.h\""
                , "#ifdef _WIN32"
                , "static void recv_error(const Logger *log, int error)"
                , "{"
                , "    Net_Strerror error_str;"
                , "    LOGGER_ERROR(log, \"%s\", net_strerror(error, &error_str));"
                , "}"
                , "#endif /* _WIN32 */"
                ])
            ]
  where
    netH =
        [ "#define NET_STRERROR_SIZE 256"
        , "typedef struct Net_Strerror {"
        , "    char data[NET_STRERROR_SIZE];"
        , "    uint16_t size;"
        , "} Net_Strerror;"
        , "char *net_strerror(int error, Net_Strerror *buf);"
        ]

    gnuStrerror =
        [ "static const char *net_strerror_r(int error, char *tmp, size_t tmp_size)"
        , "{"
        , "    const char *retstr = strerror_r(error, tmp, tmp_size);"
        , "    return retstr;"
        , "}"
        ]

    strerror body =
        [ "#include \"net.h\""
        , "#if defined(_GNU_SOURCE) && defined(__GLIBC__)"
        ] ++ body ++
        [ "#endif /* GNU */"
        , "char *net_strerror(int error, Net_Strerror *buf)"
        , "{"
        , "    errno = 0;"
        , "    const char *retstr = net_strerror_r(error, buf->data, NET_STRERROR_SIZE);"
        , "    buf->size = (uint16_t)strlen(retstr);"
        , "    return buf->data;"
        , "}"
        ]

    caller =
        [ "#include \"net.h\""
        , "static void recv_error(const Logger *log, int error)"
        , "{"
        , "    Net_Strerror error_str;"
        , "    LOGGER_ERROR(log, \"%s\", net_strerror(error, &error_str));"
        , "}"
        ]
