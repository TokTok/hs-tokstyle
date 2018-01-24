module Main (main) where

import qualified Tokstyle.C
import qualified Tokstyle.Cimple


sources :: [String]
sources = map ("../c-toxcore/" ++)
    [ "toxav/audio.c"
    {-, "toxav/audio.h"-}
    {-, "toxav/bwcontroller.c"-}
    {-, "toxav/bwcontroller.h"-}
    {-, "toxav/groupav.c"-}
    {-, "toxav/msi.c"-}
    {-, "toxav/rtp.c"-}
    {-, "toxav/toxav.c"-}
    {-, "toxav/toxav_old.c"-}
    {-, "toxav/video.c"-}
    , "toxcore/DHT.c"
    , "toxcore/DHT.h"
    , "toxcore/LAN_discovery.c"
    , "toxcore/LAN_discovery.h"
    , "toxcore/Messenger.c"
    , "toxcore/Messenger.h"
    , "toxcore/TCP_client.c"
    , "toxcore/TCP_client.h"
    , "toxcore/TCP_connection.c"
    , "toxcore/TCP_connection.h"
    , "toxcore/TCP_server.c"
    , "toxcore/TCP_server.h"
    , "toxcore/crypto_core.c"
    , "toxcore/crypto_core.h"
    , "toxcore/friend_connection.c"
    , "toxcore/friend_connection.h"
    , "toxcore/friend_requests.c"
    , "toxcore/friend_requests.h"
    , "toxcore/group.c"
    , "toxcore/group.h"
    , "toxcore/list.c"
    , "toxcore/list.h"
    , "toxcore/logger.c"
    , "toxcore/logger.h"
    , "toxcore/mono_time.c"
    , "toxcore/mono_time.h"
    , "toxcore/net_crypto.c"
    , "toxcore/network.c"
    , "toxcore/network.h"
    , "toxcore/onion.c"
    , "toxcore/onion.h"
    , "toxcore/onion_announce.c"
    , "toxcore/onion_announce.h"
    , "toxcore/onion_client.c"
    , "toxcore/onion_client.h"
    , "toxcore/ping.c"
    , "toxcore/ping.h"
    , "toxcore/ping_array.c"
    {-, "toxcore/ping_array.h"-}
    , "toxcore/tox.c"
    {-, "toxcore/tox.h"-}
    , "toxcore/util.c"
    , "toxcore/util.h"
    ]


main :: IO ()
main = do
    Tokstyle.C.main sources
    Tokstyle.Cimple.main sources
