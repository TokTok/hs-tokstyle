{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wwarn #-}
module Tokstyle.Linter.Callgraph where

import           Control.Applicative         ((<|>))
import           Control.Monad               (forM_, unless)
import qualified Control.Monad.State.Strict  as State
import qualified Data.Array                  as Array
import           Data.Fix                    (foldFix)
import           Data.Foldable               (fold)
import           Data.Graph                  (SCC (..))
import qualified Data.Graph                  as Graph
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import qualified Data.Maybe                  as Maybe
import           Data.Set                    (Set, (\\))
import qualified Data.Set                    as Set
import           Data.String                 (IsString (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (AlexPosn (..), Lexeme (..),
                                              LexemeClass (..),
                                              LiteralType (..), Node,
                                              NodeF (..), lexemeText)
import           Language.Cimple.Diagnostics (Diagnostics, warn)


data Name a = Name
    { nameFile   :: FilePath
    , nameLexeme :: Lexeme a
    }
    deriving (Show, Functor)

instance Ord a => Ord (Name a) where
    (Name _ a) <= (Name _ b) = lexemeText a <= lexemeText b

instance Eq a => Eq (Name a) where
    (Name _ a) == (Name _ b) = lexemeText a == lexemeText b

instance IsString a => IsString (Name a) where
    fromString x = Name "<builtins>" (L (AlexPn 0 0 0) IdVar (fromString x))

globalName :: Name Text
globalName = "<global>"

nameText :: Name a -> a
nameText = lexemeText . nameLexeme


type Callgraph = Map (Name Text) (Set (Name Text))

getSrcName :: (Name Text, Name Text, [Name Text]) -> Name Text
getSrcName (node, _, _) = node

cgToEdges :: Callgraph -> [(Name Text, Name Text, [Name Text])]
cgToEdges = map toEdges . Map.assocs
  where toEdges (src, dsts) = (src, src, Set.toList dsts)


data Env = Env
    { outgoing :: Set (Name Text)
    , locals   :: Set (Name Text)
    , envFunc  :: Maybe (Name Text)
    , funcs    :: Callgraph
    }
    deriving (Show)

instance Semigroup Env where
    a <> b = Env
        { outgoing = Set.union (outgoing a) (outgoing b)
        , locals = Set.union (locals a) (locals b)
        , envFunc = envFunc a <|> envFunc b
        , funcs = Map.unionWith (<>) (funcs a) (funcs b)
        }

instance Monoid Env where
    mempty = empty

empty :: Env
empty = Env
    { outgoing = Set.empty
    , locals   = Set.empty
    , envFunc  = Nothing
    , funcs    = Map.empty
    }


callgraph :: [(FilePath, [Node (Lexeme Text)])] -> Callgraph
callgraph = funcs . mconcat . concatMap (uncurry $ map . foldFix . go)
  where
    go :: FilePath -> NodeF (Lexeme Text) Env -> Env
    go file (LiteralExpr ConstId name) = empty{outgoing = Set.singleton (Name file name)}
    go file (VarExpr name) = empty{outgoing = Set.singleton (Name file name)}

    go file (MacroParam name) = empty{locals = Set.singleton (Name file name)}
    go file (VarDecl _ name arrs) = foldr (<>) empty{locals = Set.singleton (Name file name)} arrs
    go file (VLA _ name size) = empty{locals = Set.singleton (Name file name)} <> size

    go file (FunctionPrototype _ name params) = foldr (<>) empty{envFunc = Just (Name file name)} params

    go file (PreprocDefineConst name env) = empty{funcs = Map.singleton (Name file name) (outgoing env)}
    go file (ConstDefn _ _      name env) = empty{funcs = Map.singleton (Name file name) (outgoing env)}
    go file (Enumerator         name env) = empty{funcs = Map.singleton (Name file name) (maybe Set.empty outgoing env)}
    go _    (StaticAssert          env _) = empty{funcs = Map.singleton globalName (outgoing env)}

    go file (PreprocDefineMacro func params body) =
        let Env{outgoing, locals} = fold params <> body in
        empty{funcs = Map.singleton (Name file func) (Set.difference outgoing locals)}
    go _ (FunctionDefn _ proto body) =
        let
           Env{outgoing, locals, envFunc, funcs} = proto <> body
           func = Maybe.fromMaybe globalName envFunc
        in
        empty{funcs = Map.insert func (outgoing \\ locals \\ Map.keysSet funcs) $ Map.mapKeys (fmap (\k -> nameText func <> "::" <> k)) funcs}

    go _ AggregateDecl   {} = empty
    go _ FunctionDecl    {} = empty
    go _ Typedef         {} = empty
    go _ TypedefFunction {} = empty

    go _ (PreprocIf     _ t e) = fold $ e:t
    go _ (PreprocIfdef  _ t e) = fold $ e:t
    go _ (PreprocIfndef _ t e) = fold $ e:t
    go _ (PreprocElif   _ t e) = fold $ e:t

    go _ n = fold n


checkReferences :: Callgraph -> Diagnostics ()
checkReferences cg =
    forM_ (Map.assocs cg) $ \(src, dsts) ->
        mapM_ (checkForward src) dsts
  where
    checkForward src dst =
        unless (dst `Map.member` cg) $
            warn (nameFile dst) (nameLexeme dst) $ "function `" <> nameText src
                <> "` references undefined global name `" <> nameText dst <> "`"


checkCycles :: Callgraph -> Diagnostics ()
checkCycles cg =
    forM_ (Graph.stronglyConnCompR edgeList) $ \case
        AcyclicSCC{} -> return ()
        CyclicSCC [] -> return ()
        CyclicSCC vs@((src, _, _):_) ->
            let funcs = map (nameText . getSrcName) vs in
            unless (cycleIsOK funcs) $ warnCycle src funcs

  where
    edgeList = cgToEdges cg

    cycleIsOK ["add_to_closest"] = True
    cycleIsOK ["add_to_list"] = True
    cycleIsOK ["dht_pk_callback","change_dht_pk","dht_ip_callback","friend_new_connection"] = True
    cycleIsOK ["add_conn_to_groupchat","g_handle_packet","handle_message_packet_group","freeze_peer"
              ,"try_send_rejoin","handle_packet_rejoin","g_handle_status","set_conns_status_groups"
              ,"set_conns_type_connections","check_disconnected"] = True
    cycleIsOK _ = False

    warnCycle src [_] = warn (nameFile src) (nameLexeme src) $
        "function `" <> nameText src <> "` is recursive; prefer loops instead"
    warnCycle src funcs = warn (nameFile src) (nameLexeme src) $
        "function `" <> nameText src <> "` is part of a cycle: `" <> Text.pack (show funcs) <> "`"


checkUnused :: Callgraph -> Diagnostics ()
checkUnused cg =
    forM_ roots $ \src ->
        warn (nameFile src) (nameLexeme src) $ "unused symbol `" <> nameText src <> "`"
  where
    (graph, nodeFromVertex, _) = Graph.graphFromEdges . cgToEdges $ cg

    roots =
        filter (not . isExempt . nameText)
        . map (getSrcName . nodeFromVertex . fst)
        . filter ((== 0) . snd)
        . Array.assocs
        . Graph.indegree
        $ graph

    isExempt name = or
        [ "crypto_sign_" `Text.isPrefixOf` name
        , "msgpack_" `Text.isPrefixOf` name
        , "TOX_" `Text.isPrefixOf` name
        , "TOXAV_" `Text.isPrefixOf` name
        , "tox_" `Text.isPrefixOf` name
        , "toxav_" `Text.isPrefixOf` name
        , "max_" `Text.isPrefixOf` name
        , "min_" `Text.isPrefixOf` name
        , "::" `Text.isInfixOf` name
        , name `elem`
            [ "<global>"
            -- Feature test macros.
            , "__EXTENSIONS__"
            , "_XOPEN_SOURCE"
            , "_WIN32_WINNT"
            , "WINVER"

            , "NET_PACKET_MAX"  -- TODO(iphydf): Maybe some more general rule about this.
            , "at_shutdown"  -- Actually #if-0'd.

            -- TODO(iphydf): Maybe toxcore should have a bootstrap node API so DHT_bootstrap is
            -- less special.
            , "BOOTSTRAP_INFO_PACKET_ID"
            -- TODO(iphydf): Clean these up.
            , "dht_bootstrap_from_address"
            , "dht_set_self_public_key"
            , "dht_set_self_secret_key"
            , "friend_conn_get_dht_ip_port"
            , "friend_conn_get_onion_friendnum"
            , "get_ip6_loopback"
            , "ipport_self_copy"
            , "mono_time_set_current_time_callback"
            , "net_family_is_tcp_onion"
            , "net_family_is_tox_tcp_ipv6"
            , "onion_announce_entry_public_key"
            , "onion_announce_entry_set_time"
            , "onion_getfriendip"
            , "rb_data"
            , "rb_full"
            , "send_announce_request"
            , "send_data_request"
            , "tcp_connections_public_key"
            , "tcp_send_oob_packet_using_relay"
            , "tcp_server_listen_count"

            -- TODO(iphydf): Clean these up.
            , "ARRAY_ENTRY_SIZE"
            , "AUDIO_MAX_BUFFER_SIZE_BYTES"
            , "FILEKIND_AVATAR"
            , "FILEKIND_DATA"
            , "FILE_PAUSE_BOTH"
            , "MAX_TCP_CONNECTIONS"
            , "MAX_TCP_RELAYS_PEER"
            , "MESSAGE_NORMAL"
            , "MSI_E_INVALID_PARAM"
            , "ONION_DATA_FRIEND_REQ"
            , "PACKET_ID_RANGE_LOSSLESS_NORMAL_END"
            , "PACKET_ID_RANGE_LOSSLESS_NORMAL_START"
            , "PACKET_ID_RANGE_LOSSY_CUSTOM_END"
            , "PACKET_ID_RANGE_RESERVED_END"
            , "PACKET_ID_RANGE_RESERVED_START"
            , "TCP_CLIENT_NO_STATUS"
            , "USERSTATUS_AWAY"
            , "USERSTATUS_BUSY"
            ]
        ]


linter :: Callgraph -> Diagnostics ()
linter cg = do
    checkReferences cg
    checkCycles cg
    checkUnused cg


analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse = reverse . flip State.execState [] . linter . (builtins <>) . callgraph
  where
    builtins = Map.fromList . map (,Set.empty) $
        [ "AF_INET"
        , "AF_INET6"
        , "AF_UNSPEC"

        , "EINPROGRESS"
        , "EWOULDBLOCK"

        , "crypto_box_afternm"
        , "crypto_box_beforenm"
        , "crypto_box_BEFORENMBYTES"
        , "crypto_box_BOXZEROBYTES"
        , "crypto_box_keypair"
        , "crypto_box_NONCEBYTES"
        , "crypto_box_open_afternm"
        , "crypto_box_PUBLICKEYBYTES"
        , "crypto_box_SECRETKEYBYTES"
        , "crypto_box_ZEROBYTES"
        , "crypto_hash_sha256"
        , "crypto_hash_sha256_BYTES"
        , "crypto_hash_sha512"
        , "crypto_hash_sha512_BYTES"
        , "crypto_pwhash_scryptsalsa208sha256"
        , "crypto_pwhash_scryptsalsa208sha256_MEMLIMIT_INTERACTIVE"
        , "crypto_pwhash_scryptsalsa208sha256_OPSLIMIT_INTERACTIVE"
        , "crypto_pwhash_scryptsalsa208sha256_SALTBYTES"
        , "crypto_scalarmult_curve25519_base"
        , "crypto_sign_BYTES"
        , "crypto_sign_PUBLICKEYBYTES"
        , "crypto_sign_SECRETKEYBYTES"
        , "crypto_sign_detached"
        , "crypto_sign_ed25519_pk_to_curve25519"
        , "crypto_sign_ed25519_sk_to_curve25519"
        , "crypto_sign_keypair"
        , "crypto_sign_verify_detached"
        , "crypto_verify_32"
        , "crypto_verify_64"
        , "randombytes"
        , "randombytes_stir"
        , "randombytes_uniform"
        , "sodium_init"
        , "sodium_memzero"
        , "sodium_mlock"
        , "sodium_munlock"

        , "SIZEOF_VLA"
        , "abort"
        , "assert"
        , "calloc"
        , "errno"
        , "fprintf"
        , "fputc"
        , "free"
        , "malloc"
        , "memcmp"
        , "memcpy"
        , "memmove"
        , "memset"
        , "nullptr"
        , "qsort"
        , "realloc"
        , "snprintf"
        , "stderr"
        , "stdout"
        , "strerror_r"
        , "strlen"
        , "strrchr"
        , "strstr"
        , "time"
        , "va_end"
        , "va_start"
        , "vsnprintf"

        , "WSAAddressToString"
        , "WSACleanup"
        , "WSAECONNRESET"
        , "WSAEINPROGRESS"
        , "WSAEWOULDBLOCK"
        , "WSAGetLastError"
        , "WSAStartup"
        , "WSAStringToAddress"

        , "CLOCK_MONOTONIC"
        , "clock_get_time"
        , "clock_gettime"

        , "F_SETFL"
        , "FIONBIO"
        , "FIONREAD"
        , "INADDR_BROADCAST"
        , "INET_ADDRSTRLEN"
        , "INET6_ADDRSTRLEN"
        , "IPPROTO_IPV6"
        , "IPPROTO_TCP"
        , "IPPROTO_UDP"
        , "IPV6_JOIN_GROUP"
        , "O_NONBLOCK"
        , "SIOCGIFBRDADDR"
        , "SIOCGIFCONF"
        , "SOCK_DGRAM"
        , "SOCK_STREAM"
        , "SOL_SOCKET"
        , "SO_BROADCAST"
        , "SO_NOSIGPIPE"
        , "SO_RCVBUF"
        , "SO_REUSEADDR"
        , "SO_SNDBUF"
        , "accept"
        , "bind"
        , "close"
        , "closesocket"
        , "connect"
        , "fcntl"
        , "freeaddrinfo"
        , "getaddrinfo"
        , "getsockopt"
        , "htonl"
        , "htons"
        , "in6addr_loopback"
        , "inet_ntop"
        , "inet_pton"
        , "ioctl"
        , "ioctlsocket"
        , "listen"
        , "ntohl"
        , "ntohs"
        , "recv"
        , "recvfrom"
        , "send"
        , "sendto"
        , "setsockopt"
        , "socket"

        , "EPOLL_CTL_ADD"
        , "EPOLL_CTL_MOD"
        , "EPOLLERR"
        , "EPOLLET"
        , "EPOLLHUP"
        , "EPOLLIN"
        , "epoll_create"
        , "epoll_ctl"
        , "epoll_wait"

        , "__FILE__"
        , "__LINE__"
        , "__VA_ARGS__"
        , "__func__"

        , "ERROR_BUFFER_OVERFLOW"
        , "MSGPACK_OBJECT_ARRAY"
        , "MSGPACK_OBJECT_BIN"
        , "MSGPACK_OBJECT_BOOLEAN"
        , "MSGPACK_OBJECT_POSITIVE_INTEGER"
        , "MSGPACK_UNPACK_SUCCESS"
        , "NO_ERROR"
        , "msgpack_object_equal"
        , "msgpack_object_print"
        , "msgpack_pack_array"
        , "msgpack_pack_bin"
        , "msgpack_pack_bin_body"
        , "msgpack_pack_false"
        , "msgpack_pack_true"
        , "msgpack_pack_uint8"
        , "msgpack_pack_uint16"
        , "msgpack_pack_uint32"
        , "msgpack_pack_uint64"
        , "msgpack_packer_init"
        , "msgpack_sbuffer_destroy"
        , "msgpack_sbuffer_init"
        , "msgpack_sbuffer_write"
        , "msgpack_unpacked_destroy"
        , "msgpack_unpacked_init"
        , "msgpack_unpack_next"

        , "FORMAT_MESSAGE_ALLOCATE_BUFFER"
        , "FORMAT_MESSAGE_FROM_SYSTEM"
        , "FORMAT_MESSAGE_IGNORE_INSERTS"
        , "MAKEWORD"
        , "FormatMessageA"
        , "GetAdaptersInfo"
        , "GetTickCount"
        , "LocalFree"

        , "fuzz_get_count"
        , "fuzz_random_bytes"
        , "fuzz_recv"
        , "fuzz_recvfrom"
        , "fuzz_send"
        , "fuzz_sendto"

        , "SYSTEM_CLOCK"
        , "host_get_clock_service"
        , "mach_host_self"
        , "mach_port_deallocate"
        , "mach_task_self"

        , "at_startup_ran"

        , "TOX_VERSION_IS_API_COMPATIBLE"
        , "tox_options_get_end_port"
        , "tox_options_get_experimental_thread_safety"
        , "tox_options_get_hole_punching_enabled"
        , "tox_options_get_ipv6_enabled"
        , "tox_options_get_local_discovery_enabled"
        , "tox_options_get_log_callback"
        , "tox_options_get_log_user_data"
        , "tox_options_get_proxy_host"
        , "tox_options_get_proxy_port"
        , "tox_options_get_proxy_type"
        , "tox_options_get_savedata_length"
        , "tox_options_get_savedata_type"
        , "tox_options_get_start_port"
        , "tox_options_get_tcp_port"
        , "tox_options_get_udp_enabled"
        , "tox_options_set_experimental_thread_safety"
        , "tox_options_set_hole_punching_enabled"
        , "tox_options_set_ipv6_enabled"
        , "tox_options_set_local_discovery_enabled"
        , "tox_options_set_proxy_type"
        , "tox_options_set_udp_enabled"

        , "PTHREAD_MUTEX_RECURSIVE"
        , "pthread_mutexattr_destroy"
        , "pthread_mutexattr_init"
        , "pthread_mutexattr_settype"
        , "pthread_mutex_destroy"
        , "pthread_mutex_init"
        , "pthread_mutex_lock"
        , "pthread_mutex_trylock"
        , "pthread_mutex_unlock"
        , "pthread_rwlock_destroy"
        , "pthread_rwlock_init"
        , "pthread_rwlock_rdlock"
        , "pthread_rwlock_unlock"
        , "pthread_rwlock_wrlock"

        , "INT_MAX"
        , "INT32_MAX"
        , "UINT8_MAX"
        , "UINT16_MAX"
        , "UINT32_MAX"
        , "UINT64_MAX"
        , "SIZE_MAX"
        , "UINT64_C"

        , "OPUS_APPLICATION_AUDIO"
        , "OPUS_APPLICATION_VOIP"
        , "OPUS_INVALID_PACKET"
        , "OPUS_OK"
        , "OPUS_SET_BITRATE"
        , "OPUS_SET_COMPLEXITY"
        , "OPUS_SET_INBAND_FEC"
        , "OPUS_SET_PACKET_LOSS_PERC"
        , "opus_packet_get_nb_channels"
        , "opus_strerror"
        , "opus_decode"
        , "opus_decoder_create"
        , "opus_decoder_destroy"
        , "opus_decoder_get_nb_samples"
        , "opus_encode"
        , "opus_encoder_create"
        , "opus_encoder_ctl"
        , "opus_encoder_destroy"

        , "VP8_DEBLOCK"
        , "VP8_SET_POSTPROC"
        , "VP8E_SET_CPUUSED"
        , "VP8E_SET_NOISE_SENSITIVITY"
        , "VPX_CODEC_CX_FRAME_PKT"
        , "VPX_IMG_FMT_I420"
        , "VPX_CODEC_INCAPABLE"
        , "VPX_CODEC_OK"
        , "VPX_CODEC_USE_FRAME_THREADING"
        , "VPX_CODEC_USE_POSTPROC"
        , "VPX_EFLAG_FORCE_KF"
        , "VPX_ERROR_RESILIENT_DEFAULT"
        , "VPX_ERROR_RESILIENT_PARTITIONS"
        , "VPX_FRAME_IS_KEY"
        , "VPX_KF_AUTO"
        , "VPX_PLANE_U"
        , "VPX_PLANE_V"
        , "VPX_PLANE_Y"
        , "VPX_RC_ONE_PASS"
        , "VPX_VBR"
        , "vpx_codec_control"
        , "vpx_codec_dec_init"
        , "vpx_codec_decode"
        , "vpx_codec_destroy"
        , "vpx_codec_enc_config_default"
        , "vpx_codec_enc_config_set"
        , "vpx_codec_enc_init"
        , "vpx_codec_encode"
        , "vpx_codec_err_to_string"
        , "vpx_codec_get_cx_data"
        , "vpx_codec_get_frame"
        , "vpx_codec_vp8_cx"
        , "vpx_codec_vp8_dx"
        , "vpx_img_alloc"
        , "vpx_img_free"
        ]
