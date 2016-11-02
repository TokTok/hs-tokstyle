{-# LANGUAGE DeriveFunctor #-}
module Main (main) where

import qualified Data.List                       as List
import           Language.C
import           Language.C.Analysis.AstAnalysis
import           Language.C.Analysis.SemRep
import           Language.C.Analysis.TravMonad
import           Language.C.System.GCC
import           Language.C.System.Preprocess
import           System.Environment              (getArgs)

import qualified Checks.Naming
import           Result


sources :: [String]
sources =
    [ "../c-toxcore/toxav/audio.c"
    , "../c-toxcore/toxav/bwcontroller.c"
    , "../c-toxcore/toxav/groupav.c"
    , "../c-toxcore/toxav/msi.c"
    , "../c-toxcore/toxav/rtp.c"
    , "../c-toxcore/toxav/toxav.c"
    , "../c-toxcore/toxav/toxav_old.c"
    , "../c-toxcore/toxav/video.c"
    , "../c-toxcore/toxcore/DHT.c"
    , "../c-toxcore/toxcore/LAN_discovery.c"
    , "../c-toxcore/toxcore/Messenger.c"
    , "../c-toxcore/toxcore/TCP_client.c"
    , "../c-toxcore/toxcore/TCP_connection.c"
    , "../c-toxcore/toxcore/TCP_server.c"
    , "../c-toxcore/toxcore/assoc.c"
    , "../c-toxcore/toxcore/crypto_core.c"
    , "../c-toxcore/toxcore/friend_connection.c"
    , "../c-toxcore/toxcore/friend_requests.c"
    , "../c-toxcore/toxcore/group.c"
    , "../c-toxcore/toxcore/list.c"
    , "../c-toxcore/toxcore/logger.c"
    , "../c-toxcore/toxcore/net_crypto.c"
    , "../c-toxcore/toxcore/network.c"
    , "../c-toxcore/toxcore/onion.c"
    , "../c-toxcore/toxcore/onion_announce.c"
    , "../c-toxcore/toxcore/onion_client.c"
    , "../c-toxcore/toxcore/ping.c"
    , "../c-toxcore/toxcore/ping_array.c"
    , "../c-toxcore/toxcore/tox.c"
    , "../c-toxcore/toxcore/util.c"
    ]


phaseCpp :: FilePath -> IO (FilePath, InputStream)
phaseCpp file = do
    cppArgs <- (["-std=c99", "-U__BLOCKS__", "-D_VA_LIST", "-D_Nonnull=", "-D_Nullable=", "-D__attribute__(x)="] ++) <$> getArgs
    result <- runPreprocessor (newGCC "gcc") $ rawCppArgs cppArgs file
    case result of
        Left err -> fail $ show err
        Right ok -> return (file, ok)


phaseParse :: FilePath -> InputStream -> Result CTranslUnit
phaseParse file preprocessed = do
    case parseC preprocessed (initPos file) of
        Left err -> fail $ show err
        Right tu -> return tu


phaseAnalyse :: CTranslUnit -> Result (CTranslUnit, GlobalDecls, [CError])
phaseAnalyse tu = do
    case runTrav_ (analyseAST tu) of
        Left errs           -> fail $ concatMap show errs
        Right (decls, cerr) -> return (tu, decls, cerr)


phaseCheck :: (CTranslUnit, GlobalDecls, [CError]) -> [CError]
phaseCheck (tu, decls, cerr) =
    --cerr ++
    Checks.Naming.check tu decls


printError :: ErrorInfo -> IO ()
printError (ErrorInfo _ pos msgs) =
    putStrLn $ file ++ ":" ++ show line ++ ": " ++ List.intercalate "\n\t" msgs
  where
    file = posFile pos
    line = posRow  pos


showResult :: Result [CError] -> IO ()
showResult (Success cerr) = mapM_ (printError . errorInfo) cerr
showResult (Failure  err) = putStr err


main :: IO ()
main = do
    putStrLn "[=] preprocessing..."
    preprocessed <- mapM phaseCpp sources
    putStrLn "[=] parsing..."
    let trees = map (uncurry phaseParse) preprocessed
    putStrLn "[=] analysing..."
    let analyses = map (>>= phaseAnalyse) trees
    putStrLn "[=] checking..."
    let checks = map (fmap phaseCheck) analyses
    mapM_ showResult checks
