---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.ProjectManager.Context where

import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Data.IORef                 (IORef)
import qualified Data.IORef                 as IORef

import           Flowbox.Batch.Batch   (Batch, BatchEnv)
import qualified Flowbox.Batch.Batch   as Batch
import           Flowbox.Config.Config (Config)
import           Flowbox.Control.Error
import           Flowbox.Prelude

type ContextRef = IORef BatchEnv


mk :: Config -> IO ContextRef
mk cfg = IORef.newIORef $ Batch.make cfg


run :: ContextRef -> Batch a -> IO a
run ctxRef batch = do
    ctx <- liftIO $ IORef.readIORef ctxRef
    (result, newCtx) <- runStateT (runEitherT batch) ctx
    liftIO $ IORef.writeIORef ctxRef newCtx
    eitherStringToM result
