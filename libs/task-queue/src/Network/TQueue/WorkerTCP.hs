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

main :: IO ()
main = do
  [host, port, serverAddr] <- getArgs
  Right transport <- createTransport host port defaultTCPParameters
  Right endpoint  <- newEndPoint transport

  node <- newLocalNode transport initRemoteTable

  let addr = EndPointAddress (pack serverAddr)
      srvID = NodeId addr

  _ <- forkProcess node $ do
      whereisRemoteAsync srvID "serverPID"
      liftIO $ print "!!!"
      serverPid <- expect :: Process WhereIsReply
      liftIO $ print "!!!"

  --  -- get our own process id
  --  --self <- getSelfPid
  --  send self "hello"
  --  hello <- expect :: Process String
  --  liftIO $ putStrLn hello

--  Right conn <- connect endpoint addr ReliableOrdered defaultConnectHints
  x <- connect endpoint addr ReliableOrdered defaultConnectHints
  let conn = case x of
              Right conn -> conn
              Left err -> error$ "Error connecting: "++show err
  Transport.send conn [toStrict . encode $ Message.RegisterWorker $ Worker Nothing]


  print ">> 1"
  receive endpoint >>= print
  print ">> 2"
  receive endpoint >>= print
  print ">> 3"
  receive endpoint >>= print

  closeTransport transport
