
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

--import Data.List
import           Data.IORef                               
import           Network                                  
--import System.Environment(getArgs)


-- Thrift libraries
--import Thrift
--import Thrift.Transport.Handle
--import Thrift.Protocol
--import Thrift.Protocol.Binary
import           Thrift.Server                            (runBasicServer)

-- Generated files
import qualified Batch                                  as TBatch
import           Batch_Iface                              
import qualified Flowbox.Batch.Batch                    as Batch
import           Flowbox.Batch.Batch                      (Batch(..))
import qualified Flowbox.Batch.Server.Handlers.Defs     as HDefs
import qualified Flowbox.Batch.Server.Handlers.Graph    as HGraph
import qualified Flowbox.Batch.Server.Handlers.Libs     as HLibs
import qualified Flowbox.Batch.Server.Handlers.Projects as HProjects
import qualified Flowbox.Batch.Server.Handlers.Types    as HTypes

import qualified Flowbox.Batch.Project.Project          as Project
import qualified Flowbox.Batch.Project.ProjectManager   as ProjectManager
import qualified Flowbox.Batch.Samples.Modules          as Sample

port :: PortNumber
port = 30521


type BatchHandler = IORef Batch


--newBatchHandler = newIORef Batch.empty
newBatchHandler :: IO BatchHandler
newBatchHandler = newIORef $ Batch.empty { Batch.projectManager = ProjectManager.mkGraph [
                                                                             (0, Sample.project) 
                                                                             --(0, Project.empty)
                                                                                                ] []
                                         , Batch.activeProjectID = 0 }

 
instance Batch_Iface BatchHandler where
    projects            = HProjects.projects
    createProject       = HProjects.createProject
    openProject         = HProjects.openProject
    closeProject        = HProjects.closeProject
    storeProject        = HProjects.storeProject
    setActiveProject    = HProjects.setActiveProject

    libraries           = HLibs.libraries
    createLibrary       = HLibs.createLibrary
    loadLibrary         = HLibs.loadLibrary
    unloadLibrary       = HLibs.unloadLibrary
    storeLibrary        = HLibs.storeLibrary
    buildLibrary        = HLibs.buildLibrary
    libraryRootDef      = HLibs.libraryRootDef

    defsGraph           = HDefs.defsGraph
    newDefinition       = HDefs.newDefinition
    addDefinition       = HDefs.addDefinition
    updateDefinition    = HDefs.updateDefinition
    removeDefinition    = HDefs.removeDefinition
    definitionChildren  = HDefs.definitionChildren
    definitionParent    = HDefs.definitionParent

    newTypeModule       = HTypes.newTypeModule
    newTypeClass        = HTypes.newTypeClass
    newTypeFunction     = HTypes.newTypeFunction
    newTypeUdefined     = HTypes.newTypeUdefined
    newTypeNamed        = HTypes.newTypeNamed
    newTypeVariable     = HTypes.newTypeVariable
    newTypeList         = HTypes.newTypeList
    newTypeTuple        = HTypes.newTypeTuple

    nodesGraph          = HGraph.nodesGraph
    addNode             = HGraph.addNode
    updateNode          = HGraph.updateNode
    removeNode          = HGraph.removeNode
    connect             = HGraph.connect
    disconnect          = HGraph.disconnect

    ping _              = putStrLn "ping"


main :: IO ()
main = do
    handler <- newBatchHandler
    putStrLn "Starting the server..."
    _ <- runBasicServer handler TBatch.process port
    putStrLn "done."
