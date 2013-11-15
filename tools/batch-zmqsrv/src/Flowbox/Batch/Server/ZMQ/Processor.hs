
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Batch.Server.ZMQ.Processor where

import           Control.Applicative                         
import qualified Data.ByteString.Lazy                      as ByteString
import           Data.ByteString.Lazy                        (ByteString)
import qualified System.ZMQ3.Monadic                       as ZMQ3
import qualified Text.ProtocolBuffers                      as Proto

import qualified Text.ProtocolBuffers.Reflections          as Reflections
import qualified Text.ProtocolBuffers.WireMessage          as WireMessage

import           Flowbox.Prelude                           hiding (error)
import qualified Flowbox.Batch.Server.ZMQ.Handlers.Handler as Handler
import           Flowbox.Batch.Server.ZMQ.Handlers.Handler   (Handler)
import           Flowbox.System.Log.Logger                   
import qualified Generated.ServerApi.Server.Method         as Method
import           Generated.ServerApi.Server.Method           (Method(Method))
import qualified Generated.ServerApi.Server.Method.Name    as MethodName



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ.Processor"


call :: (Reflections.ReflectDescriptor a,   WireMessage.Wire a,
         Reflections.ReflectDescriptor msg, WireMessage.Wire msg) 
     => h -> ByteString -> (h -> msg -> ZMQ3.ZMQ z a) -> ZMQ3.ZMQ z ByteString
call handler encoded_args method = case Proto.messageGet encoded_args of
    Left   err      -> fail $ "Error while decoding arguments from request: " ++ err
    Right (args, _) -> Proto.messagePut <$> method handler args


selectCall :: Handler h => h -> ByteString -> ZMQ3.ZMQ z ByteString
selectCall handler encoded_request = let 
    (encoded_method, encoded_args) = ByteString.splitAt methodSize encoded_request 
    in case Proto.messageGet encoded_method of
        Left   err         -> fail $ "Error while decoding method from request: " ++ err
        Right (method, _) -> case Method.name method of 
            MethodName.PING  -> call handler encoded_args Handler.ping 
            MethodName.PING2 -> call handler encoded_args Handler.ping2

methodSize :: Proto.Int64
methodSize = Proto.messageSize $ Method MethodName.PING

process :: (Handler h, ZMQ3.Receiver t, ZMQ3.Sender t) => ZMQ3.Socket z t -> h -> ZMQ3.ZMQ z ()
process socket handler = do
    ZMQ3.liftIO $ loggerIO debug "method processing started"
    encoded_request  <- ByteString.fromStrict <$> ZMQ3.receive socket
    encoded_response <- ByteString.toStrict   <$> selectCall handler encoded_request
    ZMQ3.send socket [] encoded_response
