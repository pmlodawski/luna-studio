
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.Handler.BatchHandler where

import           Control.Applicative                        
import qualified Control.Concurrent.MVar                  as MVar
import           Control.Concurrent.MVar                    (MVar)
import qualified Data.IORef                               as IORef
import           Data.IORef                                 (IORef)

import           Flowbox.Prelude                            
import qualified Flowbox.Batch.Batch                      as Batch
import           Flowbox.Batch.Batch                        (Batch)
import qualified Flowbox.Batch.Project.ProjectManager     as ProjectManager
import qualified Flowbox.Batch.Samples.Std                as Sample
import qualified Flowbox.Batch.Server.Handler.Handler     as Handler
import           Flowbox.Batch.Server.Handler.Handler       (Handler)
import qualified Flowbox.Batch.Server.Handler.FileSystem  as HFileSystem
import qualified Flowbox.Batch.Server.Handler.Project     as HProject
import qualified Flowbox.Batch.Server.Handler.Maintenance as HMaintenance
import qualified Flowbox.Config.Config                    as Config
import           Flowbox.System.Log.Logger                  



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handler.BatchHandler"


data BatchHandler = BatchHandler { quitMutex :: MVar Bool
                                 , batchRef  :: IORef Batch
                                 }


empty :: IO BatchHandler
empty = do emptyBatch <- Batch.make <$> Config.load
           BatchHandler <$> MVar.newEmptyMVar
                        <*> IORef.newIORef emptyBatch { Batch.projectManager = ProjectManager.mkGraph [(0, Sample.project)] [] }


instance Handler BatchHandler where
    ls         h = HFileSystem.ls    (batchRef h)
    stat       h = HFileSystem.stat  (batchRef h)
    mkdir      h = HFileSystem.mkdir (batchRef h)
    touch      h = HFileSystem.touch (batchRef h)
    rm         h = HFileSystem.rm    (batchRef h)
    cp         h = HFileSystem.cp    (batchRef h)
    mv         h = HFileSystem.mv    (batchRef h)
    
    projects      h = HProject.projects      (batchRef h)
    projectByID   h = HProject.projectByID   (batchRef h)
    createProject h = HProject.createProject (batchRef h)
    openProject   h = HProject.openProject   (batchRef h)
    updateProject h = HProject.updateProject (batchRef h)
    closeProject  h = HProject.closeProject  (batchRef h)
    storeProject  h = HProject.storeProject  (batchRef h)

    initialize h = HMaintenance.initialize (batchRef h)
    ping       h = HMaintenance.ping       (batchRef h) 
    dump       h = HMaintenance.dump       (batchRef h)
    shutdown   h = HMaintenance.shutdown   (quitMutex h)
