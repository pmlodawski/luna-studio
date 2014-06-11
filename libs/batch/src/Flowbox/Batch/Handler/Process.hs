---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Batch.Handler.Process  where

import           Flowbox.Batch.Batch           (Batch)
import           Flowbox.Batch.Handler.Common  (processMapOp)
import qualified Flowbox.Batch.Handler.Common  as Batch
import qualified Flowbox.Batch.Process.Handle  as Handle
import qualified Flowbox.Batch.Process.Map     as ProcessMap
import qualified Flowbox.Batch.Process.Process as Process
import qualified Flowbox.Batch.Process.State   as Process
import qualified Flowbox.Batch.Project.Project as Project
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Flowbox.System.Process        as Process



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handler.Process"


processes :: Project.ID -> Batch [Process.ID]
processes projectID = ProcessMap.keys <$> Batch.getProcessMap projectID


terminate :: Process.ID -> Project.ID -> Batch ()
terminate processID projectID = processMapOp projectID (\processMap -> do
    handle <- ProcessMap.lookup processID processMap <?> ("No process with ID=" ++ show processID)
    liftIO $ Process.terminateProcess $ Handle.processHandle handle
    return (ProcessMap.delete processID processMap, ()))


status :: Process.ID -> Project.ID -> Batch Process.State
status = undefined
