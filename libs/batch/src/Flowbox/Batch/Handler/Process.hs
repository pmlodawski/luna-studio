---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Process  where

import           Flowbox.Batch.Batch           (Batch)
import           Flowbox.Batch.Handler.Common  (noresult, processMapOp, readonly)
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


processes :: (Applicative m, Monad m) => Project.ID -> Batch -> m [Process.ID]
processes projectID = readonly . processMapOp projectID (\_ processMap ->
    return (processMap, ProcessMap.keys processMap))


terminate :: Process.ID -> Project.ID -> Batch -> IO Batch
terminate processID projectID = noresult . processMapOp projectID (\_ processMap -> do
    handle <- ProcessMap.lookup processID processMap <?> ("No process with ID=" ++ show processID)
    Process.terminateProcess $ Handle.processHandle handle
    return (ProcessMap.delete processID processMap, ()))


status :: Process.ID -> Project.ID -> Batch -> m Process.State
status = undefined
