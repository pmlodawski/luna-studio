{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Server where

import Network.Transport      hiding (Connection, address)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import qualified Network.Transport as Transport
import Control.Concurrent
import Data.Map
import Control.Exception
import System.Environment
import Data.Binary (Binary, decode)
import Data.ByteString.Lazy (fromStrict)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Generics (Generic)

import Control.Distributed.Process
import Control.Distributed.Process.Node


main :: IO ()
main = do
  Right t <- createTransport "127.0.0.17" "8080" defaultTCPParameters
  node <- newLocalNode t initRemoteTable

  _ <- forkProcess node $ do
    self <- getSelfPid
    register "serverPID" self
    msg <- expect :: Process String
    liftIO $ print msg


  threadDelay (10 * 1000 * 1000)
  