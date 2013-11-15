
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Flowbox.Batch.Server.ZMQ.Handlers.BatchHandler where

import           Control.Applicative                         
import qualified Data.IORef                                as IORef
import           Data.IORef                                  (IORef)
import qualified System.ZMQ3.Monadic                       as ZMQ3
import qualified Text.ProtocolBuffers.Basic                as Proto

import           Flowbox.Prelude                             
import qualified Flowbox.Batch.Batch                       as Batch
import           Flowbox.Batch.Batch                         (Batch)
import qualified Flowbox.Batch.Project.ProjectManager      as ProjectManager
import qualified Flowbox.Batch.Samples.Std                 as Sample
import qualified Flowbox.Batch.Server.ZMQ.Handlers.Handler as Handler
import           Flowbox.Batch.Server.ZMQ.Handlers.Handler   (Handler)
import qualified Flowbox.Config.Config                     as Config
import           Flowbox.System.Log.Logger                   
import qualified Generated.ServerApi.Server.Ping.Result    as PingResult
import qualified Generated.ServerApi.Server.Ping2.Result   as Ping2Result



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.ZMQ.Handlers.BatchHandler"


type BatchHandler = IORef Batch


empty :: IO BatchHandler
empty = do emptyBatch <- Batch.make <$> Config.load
           IORef.newIORef $ emptyBatch { Batch.projectManager = ProjectManager.mkGraph [(0, Sample.project)] [] }



instance Handler BatchHandler where
    ping  _ i = do ZMQ3.liftIO $ loggerIO info $ "Called ping: " ++ show i
                   return $ PingResult.Result $ Just $ Proto.uFromString "ping2"
    ping2 _ i = do ZMQ3.liftIO $ loggerIO info $ "Called ping2: " ++ show i
                   return $ Ping2Result.Result $ Just $ Proto.uFromString "ping2"