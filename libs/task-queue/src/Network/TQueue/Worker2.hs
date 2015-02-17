{-# LANGUAGE DeriveGeneric #-}

module Worker where

import Network.Transport     (newEndPoint, EndPointAddress(EndPointAddress), Reliability(ReliableOrdered), connect, receive, closeTransport, defaultConnectHints)
import qualified Network.Transport as Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import System.Environment    (getArgs)
import Data.ByteString.Char8 (pack)
import Data.Binary (Binary, encode)
import GHC.Generics (Generic)
import Data.ByteString.Lazy (toStrict)
import Worker.Class
import qualified Message as Message
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  [host, port, serverAddr] <- getArgs
  Right transport <- createTransport host port defaultTCPParameters
  Right endpoint  <- newEndPoint transport

  node <- newLocalNode transport initRemoteTable

  let addr = EndPointAddress (pack "127.0.0.17:8080:0")
      srvID = NodeId addr

  _ <- forkProcess node $ do
      whereisRemoteAsync srvID "serverPID"
      liftIO $ print ">>>"
      WhereIsReply _ (Just serverPid) <- expect
      liftIO $ print "<<<"
      liftIO $ print serverPid
      send serverPid "hello"

  threadDelay (10 * 1000 * 1000)