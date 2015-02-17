{-# LANGUAGE DeriveGeneric #-}

module Worker where

import Network.Transport     (newEndPoint, EndPointAddress(EndPointAddress), Reliability(ReliableOrdered), connect, receive, send, closeTransport, defaultConnectHints)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import System.Environment    (getArgs)
import Data.ByteString.Char8 (pack)
import Data.Binary (Binary, encode)
import GHC.Generics (Generic)
import Data.ByteString.Lazy (toStrict)
import Worker.Class
import qualified Message as Message

main :: IO ()
main = do
  [host, port, serverAddr] <- getArgs
  Right transport <- createTransport host port defaultTCPParameters
  Right endpoint  <- newEndPoint transport

  let addr = EndPointAddress (pack serverAddr)
--  Right conn <- connect endpoint addr ReliableOrdered defaultConnectHints
  x <- connect endpoint addr ReliableOrdered defaultConnectHints
  let conn = case x of
              Right conn -> conn
              Left err -> error$ "Error connecting: "++show err
  send conn [toStrict . encode $ Message.RegisterWorker $ Worker Nothing]
  --close conn

  --replicateM_ 3 $ receive endpoint >>= print

  print ">> 1"
  receive endpoint >>= print
  print ">> 2"
  receive endpoint >>= print
  print ">> 3"
  receive endpoint >>= print

  closeTransport transport
