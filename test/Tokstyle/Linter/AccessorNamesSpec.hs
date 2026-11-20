{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.AccessorNamesSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [(FilePath, [Text])] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["accessor-names"]


shouldAccept' :: [(FilePath, [Text])] -> IO ()
shouldAccept' = shouldAccept ["accessor-names"]


header :: [Text]
header =
    [ "typedef struct Tox_Options Tox_Options;"
    , "const char *tox_options_get_proxy_host(const Tox_Options *options);"
    , "void tox_options_set_proxy_host(Tox_Options *options, const char *proxy_host);"
    ]


spec :: Spec
spec = do
    it "accepts getters and setters named after their field" $
        shouldAccept'
            [ ("tox_options.h", header)
            , ("tox_options.c",
                [ "const char *tox_options_get_proxy_host(const Tox_Options *options)"
                , "{"
                , "    return options->proxy_host;"
                , "}"
                , "void tox_options_set_proxy_host(Tox_Options *options, const char *proxy_host)"
                , "{"
                , "    options->proxy_host = proxy_host;"
                , "}"
                ])
            ]

    it "warns about a getter not named after the field it returns" $
        shouldWarn'
            [ ("tox_options.h", header)
            , ("tox_options.c",
                [ "const char *tox_options_get_proxy_host(const Tox_Options *options)"
                , "{"
                , "    return options->internal_proxy_host;"
                , "}"
                ])
            ]
            [[ "warning: getter `tox_options_get_proxy_host` returns `options->internal_proxy_host`, so its name should end in `_get_internal_proxy_host` [-Waccessor-names]"
             , "   --> tox_options.c:1:13"
             , "    |"
             , "   1| const char *tox_options_get_proxy_host(const Tox_Options *options)"
             , "    |             ^^^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "warns about a setter not named after the field it sets" $
        shouldWarn'
            [ ("tox_options.h", header)
            , ("tox_options.c",
                [ "void tox_options_set_proxy_host(Tox_Options *options, const char *proxy_host)"
                , "{"
                , "    options->host = proxy_host;"
                , "}"
                ])
            ]
            [[ "warning: setter `tox_options_set_proxy_host` sets `options->host`, so its name should end in `_set_host` [-Waccessor-names]"
             , "   --> tox_options.c:1:6"
             , "    |"
             , "   1| void tox_options_set_proxy_host(Tox_Options *options, const char *proxy_host)"
             , "    |      ^^^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "accepts callback setters named `_callback_x` for a field `x_callback`" $
        shouldAccept'
            [ ("tox.h",
                [ "typedef struct Tox Tox;"
                , "typedef void tox_friend_name_cb(Tox *tox);"
                , "void tox_callback_friend_name(Tox *tox, tox_friend_name_cb *callback);"
                ])
            , ("tox.c",
                [ "void tox_callback_friend_name(Tox *tox, tox_friend_name_cb *callback)"
                , "{"
                , "    tox->friend_name_callback = callback;"
                , "}"
                ])
            ]

    it "ignores functions not declared in a public header" $
        shouldAccept'
            [ ("dht.h", [ "typedef struct DHT DHT;", "int dht_get_port(const DHT *dht);" ])
            , ("dht.c",
                [ "int dht_get_port(const DHT *dht)"
                , "{"
                , "    return dht->net_port;"
                , "}"
                ])
            ]

    it "ignores functions that do more than access one field" $
        shouldAccept'
            [ ("tox_options.h", header)
            , ("tox_options.c",
                [ "const char *tox_options_get_proxy_host(const Tox_Options *options)"
                , "{"
                , "    if (options->proxy_type == 0) {"
                , "        return nullptr;"
                , "    }"
                , "    return options->host;"
                , "}"
                , "void tox_options_set_proxy_host(Tox_Options *options, const char *proxy_host)"
                , "{"
                , "    options->host = proxy_host;"
                , "    options->proxy_type = 1;"
                , "}"
                ])
            ]
