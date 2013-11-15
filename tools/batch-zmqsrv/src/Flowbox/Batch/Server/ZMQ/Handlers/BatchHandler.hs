
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.ZMQ.Handlers.BatchHandler where

import qualified System.ZMQ3.Monadic                       as ZMQ3
import qualified Text.ProtocolBuffers.Basic                as Proto

import           Flowbox.Prelude                             
import qualified Flowbox.Batch.Server.ZMQ.Handlers.Handler as Handler
import           Flowbox.Batch.Server.ZMQ.Handlers.Handler   (Handler)
import           Flowbox.System.Log.Logger                   
import qualified Generated.ServerApi.Server.Ping.Result    as PingResult
import qualified Generated.ServerApi.Server.Ping2.Result   as Ping2Result



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ.Handlers.BatchHandler"


data BatchHandler = BatchHandler


empty :: BatchHandler
empty = BatchHandler


instance Handler BatchHandler where
    ping  _ i = do ZMQ3.liftIO $ loggerIO info $ "Called ping: " ++ show i
                   return $ PingResult.Result $ Just $ Proto.uFromString "ping2"
    ping2 _ i = do ZMQ3.liftIO $ loggerIO info $ "Called ping2: " ++ show i
                   return $ Ping2Result.Result $ Just $ Proto.uFromString "ping2"