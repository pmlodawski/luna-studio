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

import Control.Distributed.Process.Platform.Task.Queue.BlockingQueue as Pool

startPool :: SizeLimit -> Process ProcessId
startPool sz = spawnLocal $ do
  Pool.start (pool sz :: Process (InitResult (BlockingQueue String)))

sampleTask :: (TimeInterval, String) -> Process String
sampleTask (t, s) = sleep t >> return s

$(remotable ['sampleTask])

--testSimplePoolJobBlocksCaller :: Process (AsyncResult String)
testSimplePoolJobBlocksCaller :: Process (Maybe String)
testSimplePoolJobBlocksCaller = do
  pid <- startPool 1
  job <- return $ ($(mkClosure 'sampleTask) (seconds 2, "foobar"))
  --callAsync pid job >>= wait
  callTimeout pid job (seconds 1)

myRemoteTable :: RemoteTable
myRemoteTable = __remoteTable initRemoteTable

main = do
  Right t <- createTransport "127.0.0.1" "8080" defaultTCPParameters
  node <- newLocalNode t myRemoteTable
  _ <- forkProcess node $ do
    liftIO $ putStrLn "x"
    x <- testSimplePoolJobBlocksCaller
    liftIO $ print x
    liftIO $ putStrLn "y"

  threadDelay (10 * 1000 * 1000)
  return ()


-- OUTPUT:
--   AsyncFailed (DiedException "exit-from=pid://127.0.0.1:8080:0:13")
--   x
