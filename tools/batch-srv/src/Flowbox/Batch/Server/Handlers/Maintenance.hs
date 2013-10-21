---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Maintenance (
    ping,
    dump,
    shutdown,
    initialize,
) where

import           Data.IORef                             (IORef)

import           Flowbox.Prelude                        
import           Flowbox.Batch.Batch                    (Batch)
import qualified Flowbox.Batch.Handlers.Maintenance   as BatchM
import           Flowbox.Batch.Server.Handlers.Common   (tRunScript)
import           Flowbox.Control.Error                  
import           Flowbox.System.Log.Logger              



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.Maintenance"

------ public api -------------------------------------------------

ping :: IORef Batch -> IO ()
ping _ = tRunScript $ do
    loggerIO info "ping"


dump :: IORef Batch -> IO ()
dump batchHandler = tRunScript $ do
    batch <- tryReadIORef batchHandler
    scriptIO $ print batch


shutdown :: IORef Batch -> IO ()
shutdown _ = tRunScript $ do
    loggerIO info "called shutdown"


initialize :: IORef Batch -> IO ()
initialize batchHandler = tRunScript $ do 
    scriptIO $ loggerIO info "called initialize"
    batch <- tryReadIORef batchHandler
    scriptIO $ BatchM.initialize batch
