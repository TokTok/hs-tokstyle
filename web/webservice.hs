{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Network.Wai.Handler.Warp    (Port, run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           System.Environment          (getArgs)
import           System.IO                   (BufferMode (..), hSetBuffering,
                                              stdout)

import qualified Tokstyle.App                as App

-- Run the server.
runTestServer :: Port -> IO ()
runTestServer port = run port $ simpleCors App.app

-- Put this all to work!
main :: IO ()
main = do
  -- So real time logging works correctly.
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    [port] -> runTestServer $ read port
    _      -> runTestServer 8001
