{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE TemplateHaskell           #-}

module Server where

import Control.Distributed.Process hiding (call)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process.Node hiding (call)


main = do
  Right transport <- createTransport "127.0.0.1" "8080" defaultTCPParameters
  node <- newLocalNode transport initRemoteTable
  return ()
