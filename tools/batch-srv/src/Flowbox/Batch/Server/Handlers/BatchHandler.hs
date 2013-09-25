
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Flowbox.Batch.Server.Handlers.BatchHandler where

import           Prelude                                  hiding (error)
import qualified Data.IORef                               as IORef
import           Data.IORef                                 (IORef)

import           Batch_Iface                                
import           Thrift.Transport.Handle                    ()

import           Flowbox.Control.Error                      
import qualified Flowbox.Batch.Batch                      as Batch
import           Flowbox.Batch.Batch                        (Batch(..))
--import qualified Flowbox.Batch.Project.ProjectManager as ProjectManager
--import qualified Flowbox.Batch.Samples.Modules as Sample
import qualified Flowbox.Batch.Server.Handlers.Defs       as HDefs
import qualified Flowbox.Batch.Server.Handlers.Defaults   as HDefaults
import qualified Flowbox.Batch.Server.Handlers.Graph      as HGraph
import qualified Flowbox.Batch.Server.Handlers.Libs       as HLibs
import qualified Flowbox.Batch.Server.Handlers.Projects   as HProjects
import qualified Flowbox.Batch.Server.Handlers.Types      as HTypes
import qualified Flowbox.Batch.Server.Handlers.FileSystem as HFileSystem
import           Flowbox.System.Log.Logger                  


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.BatchHandler"


type BatchHandler = IORef Batch


empty :: IO BatchHandler
empty = IORef.newIORef Batch.empty
--newBatchHandler = IORef.newIORef $ Batch.empty { Batch.projectManager = ProjectManager.mkGraph [
--                                                                             (0, Sample.project) 
--                                                                             --(0, Project.empty)
--                                                                                                ] []
--                                         }


instance Batch_Iface BatchHandler where
    projects            = HProjects.projects
    projectByID         = HProjects.projectByID
    createProject       = HProjects.createProject
    openProject         = HProjects.openProject
    updateProject       = HProjects.updateProject
    closeProject        = HProjects.closeProject
    storeProject        = HProjects.storeProject

    libraries           = HLibs.libraries
    libraryByID         = HLibs.libraryByID
    createLibrary       = HLibs.createLibrary
    loadLibrary         = HLibs.loadLibrary
    unloadLibrary       = HLibs.unloadLibrary
    storeLibrary        = HLibs.storeLibrary
    buildLibrary        = HLibs.buildLibrary
    libraryRootDef      = HLibs.libraryRootDef

    defsGraph           = HDefs.defsGraph
    defByID             = HDefs.defByID
    addDefinition       = HDefs.addDefinition
    updateDefinition    = HDefs.updateDefinition
    removeDefinition    = HDefs.removeDefinition
    definitionChildren  = HDefs.definitionChildren
    definitionParent    = HDefs.definitionParent
    resolveDefinition   = HDefs.resolveDefinition

    newTypeModule       = HTypes.newTypeModule
    newTypeClass        = HTypes.newTypeClass
    newTypeFunction     = HTypes.newTypeFunction
    newTypeUdefined     = HTypes.newTypeUdefined
    newTypeNamed        = HTypes.newTypeNamed
    newTypeName         = HTypes.newTypeName
    newTypeTuple        = HTypes.newTypeTuple

    nodesGraph          = HGraph.nodesGraph
    nodeByID            = HGraph.nodeByID
    addNode             = HGraph.addNode
    updateNode          = HGraph.updateNode
    removeNode          = HGraph.removeNode
    connect             = HGraph.connect
    disconnect          = HGraph.disconnect

    nodeDefaults        = HDefaults.nodeDefaults
    setNodeDefault      = HDefaults.setNodeDefault
    removeNodeDefault   = HDefaults.removeNodeDefault

    fS_ls               = HFileSystem.ls
    fS_stat             = HFileSystem.stat
    fS_mkdir            = HFileSystem.mkdir
    fS_touch            = HFileSystem.touch
    fS_rm               = HFileSystem.rm
    fS_cp               = HFileSystem.cp
    fS_mv               = HFileSystem.mv

    ping _              = loggerIO info "ping"
    dump batchHandler   = runScript $ do batch <- tryReadIORef batchHandler
                                         scriptIO $ print batch
    shutdown _          = loggerIO info "shutdown"
