{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -Wwarn #-}
module Tokstyle.Linter.Callgraph where

import           Control.Applicative         ((<|>))
import           Control.Monad               (unless)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (foldFix)
import           Data.Foldable               (fold)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import qualified Data.Maybe                  as Maybe
import           Data.Set                    (Set, (\\))
import qualified Data.Set                    as Set
import           Data.String                 (IsString (..))
import           Data.Text                   (Text)
import           Language.Cimple             (AlexPosn (..), Lexeme (..),
                                              LexemeClass (..),
                                              LiteralType (..), Node,
                                              NodeF (..), lexemeText)
import           Language.Cimple.Diagnostics (Diagnostics, warn)


newtype Name a = Name { unName :: Lexeme a }
    deriving (Show, Functor)

instance Ord a => Ord (Name a) where
    (Name a) <= (Name b) = lexemeText a <= lexemeText b

instance Eq a => Eq (Name a) where
    (Name a) == (Name b) = lexemeText a == lexemeText b

instance IsString a => IsString (Name a) where
    fromString x = Name (L (AlexPn 0 0 0) IdVar (fromString x))

nameText :: Name a -> a
nameText = lexemeText . unName


type CallGraph = Map (Name Text) (FilePath, Set (Name Text))

data Env = Env
    { outgoing :: Set (Name Text)
    , locals   :: Set (Name Text)
    , envFunc  :: Maybe (Name Text)
    , funcs    :: CallGraph
    }
    deriving (Show)

instance Semigroup Env where
    a <> b = Env
        { outgoing = Set.union (outgoing a) (outgoing b)
        , locals = Set.union (locals a) (locals b)
        , envFunc = envFunc a <|> envFunc b
        , funcs = Map.unionWith merge (funcs a) (funcs b)
        }
      where
        merge (file, dstsA) (_, dstsB) = (file, dstsA <> dstsB)

instance Monoid Env where
    mempty = empty

empty :: Env
empty = Env
    { outgoing = Set.empty
    , locals   = Set.empty
    , envFunc  = Nothing
    , funcs    = Map.empty
    }


callgraph :: [(FilePath, [Node (Lexeme Text)])] -> Map (Name Text) (FilePath, Set (Name Text))
callgraph = funcs . mconcat . concatMap (uncurry $ map . foldFix . go)
  where
    go :: FilePath -> NodeF (Lexeme Text) Env -> Env
    go _ (LiteralExpr ConstId name) = empty{outgoing = Set.singleton (Name name)}
    go _ (VarExpr name) = empty{outgoing = Set.singleton (Name name)}

    go _ (MacroParam name) = empty{locals = Set.singleton (Name name)}
    go _ (VarDecl _ name arrs) = foldr (<>) empty{locals = Set.singleton (Name name)} arrs
    go _ (VLA _ name size) = empty{locals = Set.singleton (Name name)} <> size

    go _ (FunctionPrototype _ name params) = foldr (<>) empty{envFunc = Just (Name name)} params

    go file (PreprocDefineConst name env) = empty{funcs = Map.singleton (Name name) (file, outgoing env)}
    go file (ConstDefn _ _      name env) = empty{funcs = Map.singleton (Name name) (file, outgoing env)}
    go file (Enumerator         name env) = empty{funcs = Map.singleton (Name name) (file, maybe Set.empty outgoing env)}

    go file (PreprocDefineMacro func params body) =
        let Env{outgoing, locals} = fold params <> body in
        empty{funcs = Map.singleton (Name func) (file, Set.difference outgoing locals)}
    go file (FunctionDefn _ proto body) =
        let
           Env{outgoing, locals, envFunc, funcs} = proto <> body
           func = Maybe.fromMaybe "<global>" envFunc
        in
        empty{funcs = Map.insert func (file, outgoing \\ locals \\ Map.keysSet funcs) $ Map.mapKeys (fmap (\k -> nameText func <> "::" <> k)) funcs}

    go _ AggregateDecl   {} = empty
    go _ FunctionDecl    {} = empty
    go _ StaticAssert    {} = empty
    go _ Typedef         {} = empty
    go _ TypedefFunction {} = empty

    go _ (PreprocIf     _ t e) = fold $ e:t
    go _ (PreprocIfdef  _ t e) = fold $ e:t
    go _ (PreprocIfndef _ t e) = fold $ e:t
    go _ (PreprocElif   _ t e) = fold $ e:t

    go _ n = fold n


linter :: Map (Name Text) (FilePath, Set (Name Text)) -> Diagnostics ()
linter cg =
    mapM_ (\(src, (file, dsts)) -> mapM_ (checkForward file src) dsts) (Map.assocs cg)

  where
    checkForward file src dst =
        unless (dst `Map.member` cg) $
            warn file (unName dst) $ "function `" <> nameText src
                <> "` references undefined global name `" <> nameText dst <> "`"


analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Text]
analyse = reverse . flip State.execState [] . linter . (builtins <>) . callgraph
  where
    builtins = Map.fromList . map (,("<builtin>", Set.empty)) $
        [ "AF_INET"
        , "AF_INET6"
        , "AF_UNSPEC"

        , "EAGAIN"
        , "EINPROGRESS"
        , "EWOULDBLOCK"

        , "crypto_box_afternm"
        , "crypto_box_beforenm"
        , "crypto_box_BEFORENMBYTES"
        , "crypto_box_BOXZEROBYTES"
        , "crypto_box_keypair"
        , "crypto_box_NONCEBYTES"
        , "crypto_box_open_afternm"
        , "crypto_box_ZEROBYTES"
        , "crypto_hash_sha256"
        , "crypto_hash_sha256_BYTES"
        , "crypto_hash_sha512"
        , "crypto_pwhash_scryptsalsa208sha256"
        , "crypto_pwhash_scryptsalsa208sha256_MEMLIMIT_INTERACTIVE"
        , "crypto_pwhash_scryptsalsa208sha256_OPSLIMIT_INTERACTIVE"
        , "crypto_pwhash_scryptsalsa208sha256_SALTBYTES"
        , "crypto_scalarmult_curve25519_base"
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
        , "msgpack_packer_init"
        , "msgpack_pack_false"
        , "msgpack_pack_true"
        , "msgpack_pack_uint16"
        , "msgpack_pack_uint32"
        , "msgpack_pack_uint64"
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
        , "opus_packet_get_bandwidth"
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
