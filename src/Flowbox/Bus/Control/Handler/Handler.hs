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
--import           Flowbox.Bus.Control.Handler.ID as HandlerID
import           Flowbox.Bus.RPC.RPC            as RPC (messageGet', messagePut')
--import           Flowbox.Bus.RPC.Types
import           Flowbox.Prelude                hiding (error)
import           Flowbox.System.Log.Logger
import           Flowbox.ZMQ.RPC.Handler        (RPCHandler)
import qualified Flowbox.ZMQ.RPC.RPC            as RPC
import           Generated.Proto.Bus.Request          (Request)
import qualified Generated.Proto.Bus.Request          as Request
import qualified Generated.Proto.Bus.Request.Method   as Method
import qualified Generated.Proto.Bus.ID.Create.Args   as ID_Create
import qualified Generated.Proto.Bus.ID.Create.Result as ID_Create
import Flowbox.Bus.Control.Handler.ID as HandlerID


handler :: BusCtx -> RPCHandler Request
handler ctx callback request = case Request.method request of
    Method.ID_Create -> callback (HandlerID.create ctx) ID_Create.req ID_Create.rsp

--loggerIO :: LoggerIO
--loggerIO = getLoggerIO $moduleName


--handler :: String -> BusCtx -> RPCHandler Request
--handler methodName ctx encodedRequest = eitherT handleError handleResult eResult
--    where handleError  = responseError methodName
--          handleResult request = undefined{-case request of
--            ID_Create -> call request $ HandlerID.create ctx-}
--          eResult = hoistEither (RPC.messageGet' encodedRequest) >>= val
--          val (Request _ value) = unpackValue value
--          call request foo = do
--            result <- RPC.run $ do loggerIO debug $ show request
--                                   foo request
--            either (responseError methodName) (responseResult methodName) result


--responseResult :: (Show result, Binary result, Typeable result) => String -> result -> ZMQ z ByteString
--responseResult methodName result = do
--    loggerIO trace $ show result
--    let res = Status $ packValue result
--    return $ RPC.messagePut' $ Response methodName res []


--responseError :: String -> RPC.Error -> ZMQ z ByteString
--responseError methodName err = do
--    loggerIO error err
--    let result = ErrorResult err
--    return $ RPC.messagePut' $ undefined -- Response methodName result []
