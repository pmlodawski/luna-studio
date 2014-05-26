---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Flowbox.ZMQ.RPC.Server.Processor where

import           Control.Exception               (SomeException, try)
import           Control.Monad                   (join)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Either
import           Data.ByteString                 (ByteString)
import           Data.EitherR                    (fmapL)
import           System.ZMQ4.Monadic             (ZMQ)
import qualified Text.ProtocolBuffers.Extensions as Extensions

import           Flowbox.Prelude                                hiding (error)
import           Flowbox.System.Log.Logger
import           Flowbox.Text.ProtocolBuffers                   (Int32, Serializable)
import qualified Flowbox.Text.ProtocolBuffers                   as Proto
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import           Flowbox.ZMQ.RPC.Handler                        (RPCHandler)
import           Flowbox.ZMQ.RPC.RPC                            (RPC)
import qualified Flowbox.ZMQ.RPC.RPC                            as RPC
import           Generated.Proto.Rpc.Exception                  (Exception (Exception))
import qualified Generated.Proto.Rpc.Exception                  as Exception
import           Generated.Proto.Rpc.Response                   (Response (Response))
import qualified Generated.Proto.Rpc.Response.Type              as ResponseType



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.ZMQ.RPC.Processor"


responseExt :: ResponseType.Type -> Maybe Int32 -> rsp -> Extensions.Key Maybe Response rsp -> ByteString
responseExt rspType rspId rsp rspKey = Proto.messagePut'
                                     $ Extensions.putExt rspKey (Just rsp)
                                     $ Response rspType rspId Proto.mkExtField


process :: Serializable request
        => RPCHandler request -> ByteString -> Int32 -> ZMQ z ByteString
process handler encodedRequest requestID = case Proto.messageGet' encodedRequest of
    Left  err     -> responseError requestID err
    Right request -> handler call request where
        call method reqKey rspKey = do
            result <- handleError $ do args <- hoistEither $ Proto.getExt' reqKey request
                                       loggerIO debug $ show args
                                       method args
            either (responseError requestID) (responseResult requestID rspKey) result


responseResult :: (Show result, MonadIO m) => Int32 -> Proto.Key Maybe Response result -> result -> m ByteString
responseResult requestID rspKey result = do
    loggerIO trace $ show result
    return $ responseExt ResponseType.Result (Just requestID) result rspKey


responseError :: Int32 -> RPC.Error -> ZMQ z ByteString
responseError requestID err = do
    loggerIO error err
    let exc = Exception $ encodePJ err
    return $ responseExt ResponseType.Exception (Just requestID) exc Exception.rsp


handleError :: MonadIO m => RPC r -> m (Either RPC.Error r)
handleError fun = do
    result <- liftIO $ (try :: IO a -> IO (Either SomeException a)) $ runEitherT fun
    return $ join $ fmapL (\exception -> "Unhandled exception: " ++ show exception) result
