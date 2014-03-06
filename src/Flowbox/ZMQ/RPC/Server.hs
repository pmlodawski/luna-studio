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
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Flowbox.ZMQ.RPC.Processor                      as Processor
import           Flowbox.ZMQ.RPC.RPCHandler                     (ProtoSerializable, RPCHandler)



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.ZMQ.RPC.Server"


run :: ProtoSerializable request
    => String -> RPCHandler ctx request -> ctx -> IO ()
run ctrlAddr handler ctx = ZMQ.runZMQ $ serve ctrlAddr handler ctx


serve :: ProtoSerializable request
      => String -> RPCHandler ctx request -> ctx -> ZMQ z ()
serve ctrlAddr handler ctx = do
    rep <- ZMQ.socket ZMQ.Rep
    ZMQ.bind rep ctrlAddr
    acceptAndHandle rep handler ctx


acceptAndHandle :: (ZMQ.Receiver t, ZMQ.Sender t, ProtoSerializable request)
                => ZMQ.Socket z t -> RPCHandler ctx request -> ctx -> ZMQ z ()
acceptAndHandle socket handler ctx = forM_ [0..] $ handleCall socket handler ctx


handleCall :: (ZMQ.Receiver t, ZMQ.Sender t, ProtoSerializable request)
           => ZMQ.Socket z t -> RPCHandler ctx request -> ctx -> Int -> ZMQ z ()
handleCall socket handler ctx requestID = do
    encoded_request  <- ZMQ.receive socket
    encoded_response <- Processor.process handler ctx encoded_request $ encodeP requestID
    ZMQ.send socket [] encoded_response
