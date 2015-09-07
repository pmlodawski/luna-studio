--------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Bus.Control.Handler.Handler where

import Control.Monad.Trans.Either (eitherT, hoistEither)
import Data.Binary                (Binary)
import Data.ByteString            (ByteString)
import System.ZMQ4.Monadic        (ZMQ)

import           Flowbox.Bus.Control.BusCtx     (BusCtx)
import           Flowbox.Bus.Control.Handler.ID as HandlerID
import           Flowbox.Bus.RPC.RPC            as RPC (messageGet', messagePut')
import           Flowbox.Bus.RPC.Types
import           Flowbox.Prelude                hiding (error)
import           Flowbox.System.Log.Logger
import           Flowbox.ZMQ.RPC.Handler        (RPCHandler)
import qualified Flowbox.ZMQ.RPC.RPC            as RPC



loggerIO :: LoggerIO
loggerIO = getLoggerIO $moduleName


handler :: String -> BusCtx -> RPCHandler
handler methodName ctx encodedRequest = eitherT handleError handleResult eResult
    where handleError  = responseError methodName
          handleResult request = case request of
            IDCreate -> call request $ HandlerID.create ctx
          eResult = hoistEither (RPC.messageGet' encodedRequest) >>= val
          val (Request _ value) = unpackValue value
          call request foo = do
            result <- RPC.run $ do loggerIO debug $ show request
                                   foo request
            either (responseError methodName) (responseResult methodName) result


responseResult :: (Show result, Binary result, Typeable result) => String -> result -> ZMQ z ByteString
responseResult methodName result = do
    loggerIO trace $ show result
    let res = Status $ packValue result
    return $ RPC.messagePut' $ Response methodName res []


responseError :: String -> RPC.Error -> ZMQ z ByteString
responseError methodName err = do
    loggerIO error err
    let result = ErrorResult err
    return $ RPC.messagePut' $ Response methodName result []
