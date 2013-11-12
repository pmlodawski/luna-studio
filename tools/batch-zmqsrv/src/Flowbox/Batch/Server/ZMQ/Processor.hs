
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Batch.Server.ZMQ.Processor where


import           Control.Applicative                
import qualified Data.ByteString.Char8            as Char8
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy             as ByteStringL
import qualified System.ZMQ3.Monadic              as ZMQ3
import qualified Text.ProtocolBuffers             as Proto

import qualified Text.ProtocolBuffers.Reflections as Reflections
import qualified Text.ProtocolBuffers.WireMessage as WireMessage

import           Flowbox.Prelude                  hiding (error)
import qualified Flowbox.Batch.Server.ZMQ.Handler as Handler
import           Flowbox.Batch.Server.ZMQ.Handler   (Handler)
import           Flowbox.System.Log.Logger          



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ.Processor"


call :: (Reflections.ReflectDescriptor a,   WireMessage.Wire a,
         Reflections.ReflectDescriptor msg, WireMessage.Wire msg)
     => h -> ByteString -> (h -> msg -> ZMQ3.ZMQ z a) -> ZMQ3.ZMQ z ByteString
call handler encoded_args method = case Proto.messageGet $ ByteStringL.fromStrict encoded_args of
    Right (args, _) -> (ByteStringL.toStrict . Proto.messagePut) <$> method handler args
    Left   err      -> fail $ "Error while decoding args: " ++ err


process :: (Handler h, ZMQ3.Receiver t, ZMQ3.Sender t) => ZMQ3.Socket z t -> h -> ZMQ3.ZMQ z ()
process socket handler = do
    method       <- ZMQ3.receive socket
    encoded_args <- ZMQ3.receive socket

    --let call_ = call handler encoded_args 

    result <- case Char8.unpack method of 
                   "ping"  -> call handler encoded_args Handler.ping 
                   "ping2" -> call handler encoded_args Handler.ping2
                   _       -> fail $ "Unsupported method: " ++ show method
    
    ZMQ3.send socket [] result
    