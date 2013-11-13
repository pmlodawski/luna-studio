
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Batch.Server.ZMQ.Processor where


import           Control.Applicative                  
import qualified Data.ByteString.Char8              as Char8
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Lazy               as ByteStringL
import qualified System.ZMQ3.Monadic                as ZMQ3
import qualified Text.ProtocolBuffers               as Proto

import qualified Text.ProtocolBuffers.Extensions as Extensions
import qualified Text.ProtocolBuffers.Reflections   as Reflections
import qualified Text.ProtocolBuffers.WireMessage   as WireMessage

import           Flowbox.Prelude                    hiding (error)
import qualified Flowbox.Batch.Server.ZMQ.Handler   as Handler
import           Flowbox.Batch.Server.ZMQ.Handler     (Handler)
import           Flowbox.System.Log.Logger            
import qualified Generated.ServerApi.Request        as Request
import qualified Generated.ServerApi.Request.Method as Method
import qualified Generated.ServerApi.Server.Ping.Call   as PingCall



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ.Processor"


--call :: Handler h => h -> ByteString -> ZMQ3.ZMQ z ByteString
--call handler encoded_request  = case Proto.messageGet $ ByteStringL.fromStrict encoded_request of
--    Left   err         -> fail $ "Error while decoding request: " ++ err
--    Right (request, _) -> do let x :: PingCall.Call
--                                 x = Extensions.getVal encoded_request (Request.ext'field request)
--                             return undefined 


call :: Handler h => h -> ByteString -> ZMQ3.ZMQ z ByteString
call handler encoded_request  = case Proto.messageGet $ ByteStringL.fromStrict encoded_request of
    Left   err         -> fail $ "Error while decoding request: " ++ err
    Right (request, _) -> case Extensions.getExt PingCall.request request of 
        Left   e          -> fail $ "Missing extension field: "++ e
        Right (Just args) -> do result <- case Request.method request of 
                                            Method.Ping -> Handler.ping handler args
                                return $ ByteStringL.toStrict $ Proto.messagePut result


process :: (Handler h, ZMQ3.Receiver t, ZMQ3.Sender t) => ZMQ3.Socket z t -> h -> ZMQ3.ZMQ z ()
process socket handler = do
    ZMQ3.liftIO $ loggerIO debug "method processing started"
    encoded_request  <- ZMQ3.receive socket
    encoded_response <- call handler encoded_request
    ZMQ3.send socket [] encoded_response
