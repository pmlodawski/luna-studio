
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Server.Handler.BatchHandler where

import           Control.Applicative
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Data.IORef              (IORef)
import qualified Data.IORef              as IORef

import           Flowbox.Batch.Batch                      (Batch)
import qualified Flowbox.Batch.Batch                      as Batch
import qualified Flowbox.Batch.Server.Handler.AST         as HAST
import qualified Flowbox.Batch.Server.Handler.FileSystem  as HFileSystem
import qualified Flowbox.Batch.Server.Handler.Graph       as HGraph
import           Flowbox.Batch.Server.Handler.Handler     (Handler)
import qualified Flowbox.Batch.Server.Handler.Handler     as Handler
import qualified Flowbox.Batch.Server.Handler.Library     as HLibrary
import qualified Flowbox.Batch.Server.Handler.Maintenance as HMaintenance
import qualified Flowbox.Batch.Server.Handler.NodeDefault as HNodeDefault
import qualified Flowbox.Batch.Server.Handler.Project     as HProject
import qualified Flowbox.Config.Config                    as Config
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
--import qualified Flowbox.Batch.Project.ProjectManager     as ProjectManager
--import qualified Flowbox.Batch.Samples.Std                as Sample



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handler.BatchHandler"


data BatchHandler = BatchHandler { quitMutex :: MVar Bool
                                 , batchRef  :: IORef Batch
                                 }


empty :: IO BatchHandler
empty = do emptyBatch <- Batch.make <$> Config.load
           BatchHandler <$> MVar.newEmptyMVar
                        <*> IORef.newIORef emptyBatch
                        -- <*> IORef.newIORef emptyBatch { Batch.projectManager = ProjectManager.mkGraph [(0, Sample.project)] [] }


instance Handler BatchHandler where
    ls         h = HFileSystem.ls    (batchRef h)
    stat       h = HFileSystem.stat  (batchRef h)
    mkdir      h = HFileSystem.mkdir (batchRef h)
    touch      h = HFileSystem.touch (batchRef h)
    rm         h = HFileSystem.rm    (batchRef h)
    cp         h = HFileSystem.cp    (batchRef h)
    mv         h = HFileSystem.mv    (batchRef h)

    libraries     h = HLibrary.libraries     (batchRef h)
    libraryByID   h = HLibrary.libraryByID   (batchRef h)
    createLibrary h = HLibrary.createLibrary (batchRef h)
    loadLibrary   h = HLibrary.loadLibrary   (batchRef h)
    unloadLibrary h = HLibrary.unloadLibrary (batchRef h)
    storeLibrary  h = HLibrary.storeLibrary  (batchRef h)
    buildLibrary  h = HLibrary.buildLibrary  (batchRef h)
    runLibrary    h = HLibrary.runLibrary    (batchRef h)

    initialize h = HMaintenance.initialize (batchRef h)
    ping       h = HMaintenance.ping       (batchRef h)
    dump       h = HMaintenance.dump       (batchRef h)
    shutdown   h = HMaintenance.shutdown   (quitMutex h)

    nodeDefaults      h = HNodeDefault.nodeDefaults      (batchRef h)
    setNodeDefault    h = HNodeDefault.setNodeDefault    (batchRef h)
    removeNodeDefault h = HNodeDefault.removeNodeDefault (batchRef h)

    projects      h = HProject.projects      (batchRef h)
    projectByID   h = HProject.projectByID   (batchRef h)
    createProject h = HProject.createProject (batchRef h)
    openProject   h = HProject.openProject   (batchRef h)
    updateProject h = HProject.updateProject (batchRef h)
    closeProject  h = HProject.closeProject  (batchRef h)
    storeProject  h = HProject.storeProject  (batchRef h)

    addModule            h = HAST.addModule            (batchRef h)
    addClass             h = HAST.addClass             (batchRef h)
    addFunction          h = HAST.addFunction          (batchRef h)
    definitions          h = HAST.definitions          (batchRef h)
    updateModuleCls      h = HAST.updateModuleCls      (batchRef h)
    updateModuleImports  h = HAST.updateModuleImports  (batchRef h)
    updateModuleFields   h = HAST.updateModuleFields   (batchRef h)
    updateClassCls       h = HAST.updateClassCls       (batchRef h)
    updateClassFields    h = HAST.updateClassFields    (batchRef h)
    updateFunctionName   h = HAST.updateFunctionName   (batchRef h)
    updateFunctionPath   h = HAST.updateFunctionPath   (batchRef h)
    updateFunctionInputs h = HAST.updateFunctionInputs (batchRef h)
    updateFunctionOutput h = HAST.updateFunctionOutput (batchRef h)
    remove               h = HAST.remove               (batchRef h)

    nodesGraph h = HGraph.nodesGraph (batchRef h)
    nodeByID   h = HGraph.nodeByID   (batchRef h)
    addNode    h = HGraph.addNode    (batchRef h)
    removeNode h = HGraph.removeNode (batchRef h)
    connect    h = HGraph.connect    (batchRef h)
    disconnect h = HGraph.disconnect (batchRef h)
