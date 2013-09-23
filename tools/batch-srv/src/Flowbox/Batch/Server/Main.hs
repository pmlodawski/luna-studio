
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Prelude                                  hiding (error)
import           Control.Monad                              (when)
import qualified Control.Concurrent                       as Concurrent
import qualified Control.Concurrent.MVar                  as MVar
import           Control.Concurrent.MVar                    (MVar)
import qualified Control.Exception                        as Exception
import qualified Data.IORef                               as IORef
import           Data.IORef                                 (IORef)
import           Data.Text.Lazy                             (pack)
import qualified System.Exit                              as Exit


import qualified Batch                                    as TBatch
import           Batch_Iface                                
import           Thrift.Transport.Handle                    ()
import qualified Thrift.Protocol.Binary                   as TProtocol
import           Thrift.Protocol.Binary                     (Protocol)
import           Thrift.Transport                           (Transport)

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
import qualified Flowbox.Batch.Server.Server              as Server
import           Flowbox.System.Log.Logger                  


rootLogger :: Logger
rootLogger = getLogger "Flowbox"


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server"


address :: String
address = "127.0.0.1"


port :: Int
port = 30521


type BatchHandler = IORef Batch


newBatchHandler :: IO BatchHandler
newBatchHandler = IORef.newIORef Batch.empty
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

    ping _              = loggerIO info"ping"
    dump batchHandler   = runScript $ do batch <- tryReadIORef batchHandler
                                         scriptIO $ print batch
    shutdown _          = loggerIO info "shutdown"


processCommand :: (Protocol iprot, Protocol oprot, Transport itransp, Transport otransp, Batch_Iface batch)
         => MVar Bool -> batch -> (iprot itransp, oprot otransp) -> IO Bool
processCommand quitmutex handler (iprot, oprot) = do
    (name, typ, seqid) <- TProtocol.readMessageBegin iprot
    TBatch.proc_ handler (iprot,oprot) (name,typ,seqid)
    when (name == pack "shutdown") (MVar.putMVar quitmutex True)
    return True


serve :: MVar Bool -> IO ()
serve quitmutex = do
    handler   <- newBatchHandler
    loggerIO info "Starting the server"
    _ <- Server.runSingleConnectionServer Server.accepter handler (processCommand quitmutex) address port
    return ()


waitForQuit :: MVar t -> IO b
waitForQuit quitmutex = do
    _ <- MVar.takeMVar quitmutex
    loggerIO warning "shutting down in 3 seconds"
    Concurrent.threadDelay 1000000
    loggerIO warning "shutting down in 2 seconds"
    Concurrent.threadDelay 1000000
    loggerIO warning "shutting down in 1 second"
    Concurrent.threadDelay 1000000
    loggerIO warning "shutting down..."
    Exit.exitSuccess


main :: IO ()
main = do
    rootLogger setLevel DEBUG
    quitmutex <- MVar.newEmptyMVar
    _ <- Concurrent.forkIO 
        $ Exception.handle (\(e :: Exception.SomeException) -> do loggerIO error $ "Server run failure: " ++ show e
                                                                  MVar.putMVar quitmutex True) 
        (serve quitmutex)
    waitForQuit quitmutex
