
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Broker.Control.Processor where

import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Lazy            as ByteString
import qualified Data.Map                        as Map
import           System.ZMQ4.Monadic             (ZMQ)
import           Text.ProtocolBuffers            (Int32)
import qualified Text.ProtocolBuffers            as Proto
import qualified Text.ProtocolBuffers.Extensions as Extensions

import           Flowbox.Broker.Control.BrokerData              (BrokerData)
import qualified Flowbox.Broker.Control.Handler.ID              as HandlerID
import           Flowbox.Control.Error
import           Flowbox.Prelude                                hiding (error)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import           Generated.Proto.Broker.Exception               (Exception (Exception))
import qualified Generated.Proto.Broker.Exception               as Exception
import qualified Generated.Proto.Broker.Request                 as Request
import qualified Generated.Proto.Broker.Request.Method          as Method
import           Generated.Proto.Broker.Response                (Response (Response))
import qualified Generated.Proto.Broker.Response.Type           as ResponseType

import qualified Generated.Proto.Broker.ID.New.Args   as ID_New
import qualified Generated.Proto.Broker.ID.New.Result as ID_New



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Broker.Control.Processor"


responseExt :: ResponseType.Type -> Maybe Int32 -> r -> Extensions.Key Maybe Response r -> ByteString
responseExt t i r rspkey = ByteString.toStrict $ Proto.messagePut
                         $ Extensions.putExt rspkey (Just r)
                         $ Response t i $ Extensions.ExtField Map.empty


process :: BrokerData -> ByteString -> Int32 -> ZMQ z ByteString
process brokerData encodedRequest requestID = case Proto.messageGet $ ByteString.fromStrict encodedRequest of
    Left   e           -> fail $ "Error while decoding request: " ++ e
    Right (request, _) -> case Request.method request of
        Method.ID_New     -> call HandlerID.new ID_New.req ID_New.rsp
        where
            call method reqkey rspkey = do
                e <- runEitherT $ scriptIO $ unsafeCall method reqkey rspkey
                case e of
                    Left  m -> do loggerIO error m
                                  let exc = Exception $ encodePJ m
                                  return $ responseExt ResponseType.Exception (Just requestID) exc Exception.rsp
                    Right a ->    return a

            unsafeCall method reqkey rspkey = do
                r <- case Extensions.getExt reqkey request of
                    Right (Just args) -> do loggerIO debug $ show args
                                            method brokerData args
                    Right Nothing     -> fail $ "Error while getting extension"
                    Left   e'         -> fail $ "Error while getting extension: " ++ e'
                loggerIO trace $ show r
                return $ responseExt ResponseType.Result (Just requestID) r rspkey
