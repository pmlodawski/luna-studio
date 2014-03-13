---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.ZMQ.RPC.Server where

import           Control.Monad       (forM_)
import           System.ZMQ4.Monadic (ZMQ)
import qualified System.ZMQ4.Monadic as ZMQ

import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                   as Proto
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Flowbox.ZMQ.RPC.Processor                      as Processor
import           Flowbox.ZMQ.RPC.RPCHandler                     (RPCHandler)



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.ZMQ.RPC.Server"


run :: Proto.Serializable request
    => String -> RPCHandler request -> IO ()
run ctrlAddr handler = ZMQ.runZMQ $ serve ctrlAddr handler


serve :: Proto.Serializable request
      => String -> RPCHandler request -> ZMQ z ()
serve ctrlAddr handler = do
    rep <- ZMQ.socket ZMQ.Rep
    ZMQ.bind rep ctrlAddr
    acceptAndHandle rep handler


acceptAndHandle :: (ZMQ.Receiver t, ZMQ.Sender t, Proto.Serializable request)
                => ZMQ.Socket z t -> RPCHandler request -> ZMQ z ()
acceptAndHandle socket handler = forM_ [0..] $ handleCall socket handler


handleCall :: (ZMQ.Receiver t, ZMQ.Sender t, Proto.Serializable request)
           => ZMQ.Socket z t -> RPCHandler request -> Int -> ZMQ z ()
handleCall socket handler requestID = do
    encoded_request  <- ZMQ.receive socket
    encoded_response <- Processor.process handler encoded_request $ encodeP requestID
    ZMQ.send socket [] encoded_response
