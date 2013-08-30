
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Monad                              (when)
import qualified Control.Concurrent                       as Concurrent
import qualified Control.Concurrent.MVar                  as MVar
import           Control.Concurrent.MVar                    (MVar)
import qualified Data.IORef                               as IORef
import           Data.IORef                                 (IORef)
import           Data.Text.Lazy                             (pack)
import qualified Network                                  as Network
import qualified System.IO                                as IO
import qualified System.Exit                              as Exit


import           Thrift.Transport.Handle                    ()
import qualified Thrift.Protocol.Binary                   as Protocol
import           Thrift.Protocol.Binary                     (Protocol)
import qualified Thrift.Server                            as Server
import           Thrift.Transport                           (Transport)

-- Generated files
import qualified Batch                                    as TBatch
import           Batch_Iface                                
import qualified Flowbox.Batch.Batch                      as Batch
import           Flowbox.Batch.Batch                        (Batch(..))
import           Flowbox.Batch.Server.Handlers.Common       (logger)
import qualified Flowbox.Batch.Server.Handlers.Defs       as HDefs
import qualified Flowbox.Batch.Server.Handlers.Defaults   as HDefaults
import qualified Flowbox.Batch.Server.Handlers.Graph      as HGraph
import qualified Flowbox.Batch.Server.Handlers.Libs       as HLibs
import qualified Flowbox.Batch.Server.Handlers.Projects   as HProjects
import qualified Flowbox.Batch.Server.Handlers.Types      as HTypes
import qualified Flowbox.Batch.Server.Handlers.FileSystem as HFileSystem


import qualified Flowbox.Batch.Project.Project            as Project
import qualified Flowbox.Batch.Project.ProjectManager     as ProjectManager
import qualified Flowbox.Batch.Samples.Modules            as Sample
import           Flowbox.Control.Error                      
import           Flowbox.System.Log.Logger                  



port :: Network.PortNumber
port = 30521


type BatchHandler = IORef Batch


--newBatchHandler = newIORef Batch.empty
newBatchHandler :: IO BatchHandler
newBatchHandler = IORef.newIORef $ Batch.empty { Batch.projectManager = ProjectManager.mkGraph [
                                                                             (0, Sample.project) 
                                                                             --(0, Project.empty)
                                                                                                ] []
                                         }


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

    newTypeModule       = HTypes.newTypeModule
    newTypeClass        = HTypes.newTypeClass
    newTypeFunction     = HTypes.newTypeFunction
    newTypeUdefined     = HTypes.newTypeUdefined
    newTypeNamed        = HTypes.newTypeNamed
    newTypeVariable     = HTypes.newTypeVariable
    newTypeList         = HTypes.newTypeList
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

    ping _              = logger.info $ "ping"
    dump batchHandler   = runScript $ do batch <- tryReadIORef batchHandler
                                         scriptIO $ print batch
    shutdown _          = logger.info $ "shutdown"


processCommand :: (Protocol iprot, Protocol oprot, Transport itransp, Transport otransp, Batch_Iface batch)
         => MVar Bool -> batch -> (iprot itransp, oprot otransp) -> IO Bool
processCommand quitmutex handler (iprot, oprot) = do
    (name, typ, seqid) <- Protocol.readMessageBegin iprot
    TBatch.proc_ handler (iprot,oprot) (name,typ,seqid)
    when (name == pack "shutdown") (MVar.putMVar quitmutex True)
    return True


accepter :: Network.Socket -> IO (Protocol.BinaryProtocol IO.Handle,
                                    Protocol.BinaryProtocol IO.Handle)
accepter s = do
    (h, addr, p) <- Network.accept s
    logger.info $ "Accepted connection from " ++ addr ++ " : " ++ (show p)
    return (Protocol.BinaryProtocol h, Protocol.BinaryProtocol h)


server :: MVar Bool -> IO ()
server quitmutex = do
    handler   <- newBatchHandler
    logger.info $ "Starting the server"
    _ <- Server.runThreadedServer accepter handler (processCommand quitmutex) (Network.PortNumber port)
    return ()


waitForQuit :: MVar t -> IO b
waitForQuit quitmutex = do
    _ <- MVar.takeMVar quitmutex
    logger.warning $ "shutting down in 3 seconds"
    Concurrent.threadDelay 1000000
    logger.warning $ "shutting down in 2 seconds"
    Concurrent.threadDelay 1000000
    logger.warning $ "shutting down in 1 second"
    Concurrent.threadDelay 1000000
    logger.warning $ "shutting down..."
    Exit.exitSuccess


main :: IO ()
main = do
    logger.setLevel $ DEBUG
    quitmutex <- MVar.newEmptyMVar
    _ <- Concurrent.forkIO (server quitmutex)
    waitForQuit quitmutex

