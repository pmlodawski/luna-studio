{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE TemplateHaskell           #-}

module Main where

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


data Add = Add Double Double
  deriving (Typeable, Generic)
instance Binary Add


data Operation = RegisterWorker
               | Quit
               deriving (Show, Generic)

instance Binary Operation


launchServer :: Process ProcessId
launchServer = spawnLocal $ serve () (statelessInit Infinity) server >> return () where
  server = statelessProcess { apiHandlers            = [ handleCall_ (\(Add x y) -> return (x + y)) ]
                            , unhandledMessagePolicy = Drop
                            }


main = do
  Right transport <- createTransport "127.0.0.1" "8080" defaultTCPParameters
  node <- newLocalNode transport initRemoteTable
  _ <- forkProcess node $ do
    self <- getSelfPid
    register "serverPID" self

    liftIO $ putStrLn "x"
    mid <- launchServer
    liftIO $ putStrLn "y"
    r <- call mid (Add 5 6) :: Process Double
    liftIO $ print r
    liftIO $ putStrLn "z"


  threadDelay (10 * 1000 * 1000)
  return ()
