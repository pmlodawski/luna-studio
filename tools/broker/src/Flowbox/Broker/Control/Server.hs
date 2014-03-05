---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Broker.Control.Server where

import           Control.Monad       (forM_)
import           System.ZMQ4.Monadic (ZMQ)
import qualified System.ZMQ4.Monadic as ZMQ

import           Flowbox.Broker.Control.BrokerData              (BrokerData)
import qualified Flowbox.Broker.Control.Processor               as Processor
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Broker.Control.Server"


run :: String -> BrokerData -> IO ()
run ctrlAddr bd = ZMQ.runZMQ $ serve ctrlAddr bd
--run  = ZMQ.runZMQ . serve


serve :: String -> BrokerData -> ZMQ z ()
serve ctrlAddr bd = do
    rep <- ZMQ.socket ZMQ.Rep
    ZMQ.bind rep ctrlAddr
    acceptAndHandle rep bd


acceptAndHandle :: (ZMQ.Receiver t, ZMQ.Sender t)
                => ZMQ.Socket z t -> BrokerData -> ZMQ z ()
acceptAndHandle socket bd = forM_ [0..] $ handleCall socket bd


handleCall :: (ZMQ.Receiver t, ZMQ.Sender t)
           => ZMQ.Socket z t -> BrokerData -> Int -> ZMQ z ()
handleCall socket bd requestID = do
    encoded_request  <- ZMQ.receive socket
    encoded_response <- Processor.process bd encoded_request $ encodeP requestID
    ZMQ.send socket [] encoded_response
