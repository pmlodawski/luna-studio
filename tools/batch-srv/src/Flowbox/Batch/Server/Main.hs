---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

--import Data.List
import Data.IORef
import Network
--import System.Environment(getArgs)


-- Thrift libraries
--import Thrift
--import Thrift.Transport.Handle
--import Thrift.Protocol
--import Thrift.Protocol.Binary
import Thrift.Server (runBasicServer)

-- Generated files
import qualified Batch
import           Batch_Iface

import qualified Flowbox.Batch.Server.Handlers.Defs         as HDefs
import qualified Flowbox.Batch.Server.Handlers.Graph        as HGraph
import qualified Flowbox.Batch.Server.Handlers.Libs         as HLibs
import qualified Flowbox.Batch.Server.Handlers.Types        as HTypes

import qualified Flowbox.Batch.Project.Project              as Project
import           Flowbox.Batch.Project.Project                (Project)
import qualified Flowbox.Luna.Samples.Packages              as Sample


port :: PortNumber
port = 30521


type BatchHandler = IORef Project


newBatchHandler :: IO BatchHandler
newBatchHandler = do
    --ref <- newIORef Project.empty
    ref <- newIORef $  Project.empty { Project.name = "project"
                                     , Project.core = Sample.core } 
    return ref


instance Batch_Iface BatchHandler where
    libraries     = HLibs.libraries
    createLibrary = HLibs.createLibrary
    loadLibrary   = HLibs.loadLibrary
    unloadLibrary = HLibs.unloadLibrary
    storeLibrary  = HLibs.storeLibrary
    libraryRootDef = HLibs.libraryRootDef

    defsGraph          = HDefs.defsGraph
    newDefinition      = HDefs.newDefinition
    addDefinition      = HDefs.addDefinition
    updateDefinition   = HDefs.updateDefinition
    removeDefinition   = HDefs.removeDefinition
    definitionChildren = HDefs.definitionChildren
    definitionParent   = HDefs.definitionParent

    newTypeModule   = HTypes.newTypeModule
    newTypeClass    = HTypes.newTypeClass
    newTypeFunction = HTypes.newTypeFunction
    newTypeUdefined = HTypes.newTypeUdefined
    newTypeNamed    = HTypes.newTypeNamed
    newTypeVariable = HTypes.newTypeVariable
    newTypeList     = HTypes.newTypeList
    newTypeTuple    = HTypes.newTypeTuple

    graph      = HGraph.graph
    addNode    = HGraph.addNode
    updateNode = HGraph.updateNode
    removeNode = HGraph.removeNode
    connect    = HGraph.connect
    disconnect = HGraph.disconnect

    ping _     = putStrLn "ping"


main :: IO ()
main = do
    handler <- newBatchHandler
    putStrLn "Starting the server..."
    _ <- runBasicServer handler Batch.process port
    putStrLn "done."
