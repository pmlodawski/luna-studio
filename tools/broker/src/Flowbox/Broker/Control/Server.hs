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

import           Flowbox.Broker.Control.Handler.Handler         (Handler)
import qualified Flowbox.Broker.Control.Processor               as Processor
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Broker.Control.Server"


run :: Handler h => String -> h -> IO ()
run ctrlAddr handler = ZMQ.runZMQ $ serve ctrlAddr handler
--run  = ZMQ.runZMQ . serve


serve :: Handler h => String -> h -> ZMQ z ()
serve ctrlAddr handler = do
    rep <- ZMQ.socket ZMQ.Rep
    ZMQ.bind rep ctrlAddr
    acceptAndHandle rep handler


acceptAndHandle :: (ZMQ.Receiver t, ZMQ.Sender t, Handler h)
                => ZMQ.Socket z t -> h -> ZMQ z ()
acceptAndHandle socket handler = forM_ [0..] $ handleCall socket handler


handleCall :: (ZMQ.Receiver t, ZMQ.Sender t, Handler h)
           => ZMQ.Socket z t -> h -> Int -> ZMQ z ()
handleCall socket handler requestID = do
    encoded_request  <- ZMQ.receive socket
    encoded_response <- Processor.process handler encoded_request $ encodeP requestID
    ZMQ.send socket [] encoded_response
