---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

--import Data.List
--import System.IO
import Data.Int
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


import qualified DefinitionHandler
import qualified GraphHandler
import qualified LibraryHandler
import qualified TypesHandler


import qualified Libs_Types

import qualified Luna.Core

import qualified Luna.Lib.Library   as Library
import           Luna.Lib.Library     (Library(..))
import qualified Luna.System.UniPath as UniPath
import           Luna.Tools.Serialization
import           Luna.Tools.Serialization.LibsSerialization



test :: IO ()
test = do
    let 
        l = Library "std" $ UniPath.fromUnixString "/opt/luna/lib/std"
        l2 = encode (l, 2::Int32)
        l3 = (decode l2) :: Either String (Library, Int32)
    print l
    print l2
    print l3
    return ()


port :: PortNumber
port = 30521

data BatchHandler = BatchHandler

newBatchHandler :: IO BatchHandler
newBatchHandler = do
    return $ BatchHandler

instance Batch_Iface BatchHandler where
    libraries     = LibraryHandler.libraries
    loadLibrary   = LibraryHandler.loadLibrary
    unloadLibrary = LibraryHandler.unloadLibrary

    newDefinition      = DefinitionHandler.newDefinition
    addDefinition      = DefinitionHandler.addDefinition
    updateDefinition   = DefinitionHandler.updateDefinition
    removeDefinition   = DefinitionHandler.removeDefinition
    definitionChildren = DefinitionHandler.definitionChildren
    definitionParent   = DefinitionHandler.definitionParent

    newTypeModule   = TypesHandler.newTypeModule
    newTypeClass    = TypesHandler.newTypeClass
    newTypeFunction = TypesHandler.newTypeFunction
    newTypeUdefined = TypesHandler.newTypeUdefined
    newTypeNamed    = TypesHandler.newTypeNamed
    newTypeVariable = TypesHandler.newTypeVariable
    newTypeList     = TypesHandler.newTypeList
    newTypeTuple    = TypesHandler.newTypeTuple

    graph      = GraphHandler.graph
    addNode    = GraphHandler.addNode
    updateNode = GraphHandler.updateNode
    removeNode = GraphHandler.removeNode
    connect    = GraphHandler.connect
    disconnect = GraphHandler.disconnect

    ping _ = putStrLn "ping"


main :: IO ()
main = do
    test
    handler <- newBatchHandler
    putStrLn "Starting the server..."
    _ <- runBasicServer handler Batch.process port
    putStrLn "done."
