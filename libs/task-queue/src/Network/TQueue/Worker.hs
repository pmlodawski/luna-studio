{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE TemplateHaskell           #-}

module Main where

import Network.Transport     (EndPointAddress(EndPointAddress))
import Control.Distributed.Process hiding (call)
import Control.Distributed.Process.Platform hiding (__remoteTable)
import Control.Distributed.Process.Platform.Async
import Control.Distributed.Process.Platform.ManagedProcess
import Control.Distributed.Process.Platform.Time
import Control.Distributed.Process.Platform.Timer (sleep)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process.Node hiding (call)
import Control.Concurrent (threadDelay)
import GHC.Generics (Generic)
import Data.Binary (Binary) 
import Data.Typeable (Typeable)
import Data.ByteString.Char8 (pack)
import System.Environment    (getArgs)

import qualified Server as Server

data Add = Add Double Double
  deriving (Typeable, Generic)
instance Binary Add


data Operation = RegisterWorker
               | Quit
               deriving (Show, Generic)

instance Binary Operation


launchWorker :: Process ProcessId
launchWorker = spawnLocal $ serve () (statelessInit Infinity) server >> return () where
  server = statelessProcess { apiHandlers            = [ handleCall_ (\(Add x y) -> return (x + y)) ]
                                                       -- , handleCast_ (\(WhereIsReply{}) a -> (liftIO $ print "!!!!") >> continue_ a) ]
                            , unhandledMessagePolicy = Drop
                            }


main = do
  [host, port, serverAddr] <- getArgs

  Right transport <- createTransport host port defaultTCPParameters
  node <- newLocalNode transport initRemoteTable

  let addr = EndPointAddress (pack serverAddr)
      srvID = NodeId addr

  _ <- forkProcess node $ do
    sid <- discoverServer srvID
    liftIO $ putStrLn "x"
    liftIO $ print sid
    r <- callTimeout sid (Server.Add 5 6) 100 :: Process (Maybe Double)
    liftIO $ putStrLn "x"
    liftIO $ threadDelay (10 * 1000 * 1000)


  threadDelay (10 * 1000 * 1000)
  return ()


discoverServer srvID = do
  whereisRemoteAsync srvID "serverPID"
  reply <- expectTimeout 100 :: Process (Maybe WhereIsReply)
  case reply of
    Just (WhereIsReply _ msid) -> case msid of
      Just sid -> return sid
      Nothing  -> discoverServer srvID
    Nothing                    -> discoverServer srvID