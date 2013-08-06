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

import qualified Handlers.Defs  as HDefs
import qualified Handlers.Graph as HGraph
import qualified Handlers.Libs  as HLibs
import qualified Handlers.Types as HTypes

import qualified Luna.Core as Core
import           Luna.Core   (Core)



port :: PortNumber
port = 30521

type BatchHandler = IORef Core

newBatchHandler :: IO BatchHandler
newBatchHandler = do
    ref <- newIORef Core.empty
    return ref

instance Batch_Iface BatchHandler where
    libraries     = HLibs.libraries
    loadLibrary   = HLibs.loadLibrary
    unloadLibrary = HLibs.unloadLibrary
    libraryRootDef = HLibs.libraryRootDef

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
