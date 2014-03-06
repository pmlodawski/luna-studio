
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Flowbox.ZMQ.RPC.Processor where

import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Lazy            as ByteString
import qualified Data.Map                        as Map
import           System.ZMQ4.Monadic             (ZMQ)
import           Text.ProtocolBuffers            (Int32)
import qualified Text.ProtocolBuffers            as Proto
import qualified Text.ProtocolBuffers.Extensions as Extensions

import           Flowbox.Control.Error
import           Flowbox.Prelude                                hiding (error)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import           Flowbox.ZMQ.RPC.RPCHandler                     (ProtoSerializable, RPCHandler)
import           Generated.Proto.Rpc.Exception                  (Exception (Exception))
import qualified Generated.Proto.Rpc.Exception                  as Exception
import           Generated.Proto.Rpc.Response                   (Response (Response))
import qualified Generated.Proto.Rpc.Response.Type              as ResponseType



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.ZMQ.RPC.Processor"


responseExt :: ResponseType.Type -> Maybe Int32 -> rsp -> Extensions.Key Maybe Response rsp -> ByteString
responseExt rspType rspId rsp rspKey = ByteString.toStrict $ Proto.messagePut
                                     $ Extensions.putExt rspKey (Just rsp)
                                     $ Response rspType rspId $ Extensions.ExtField Map.empty


process :: ProtoSerializable request
        => RPCHandler ctx request -> ctx -> ByteString -> Int32 -> ZMQ z ByteString
process handler ctx encodedRequest requestID = case Proto.messageGet $ ByteString.fromStrict encodedRequest of
    Left   er          -> fail $ "Error while decoding request: " ++ er
    Right (request, _) -> handler call request
        where
            call method reqKey rspKey = do
                e <- runEitherT $ scriptIO $ unsafeCall method reqKey rspKey
                case e of
                    Left msg  -> do loggerIO error msg
                                    let exc = Exception $ encodePJ msg
                                    return $ responseExt ResponseType.Exception (Just requestID) exc Exception.rsp
                    Right rsp -> return rsp

            unsafeCall method reqKey rspKey = do
                rsp <- case Extensions.getExt reqKey request of
                       Right (Just args) -> do loggerIO debug $ show args
                                               method ctx args
                       Right Nothing     -> fail $ "Error while getting extension"
                       Left   e          -> fail $ "Error while getting extension: " ++ e
                loggerIO trace $ show rsp
                return $ responseExt ResponseType.Result (Just requestID) rsp rspKey
