---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import Data.List
import System.IO
import Network
import System.Environment(getArgs)

import qualified Data.Vector as Vector
import           Data.Vector   (Vector)

-- Thrift libraries
import Thrift
import Thrift.Transport.Handle
import Thrift.Protocol
import Thrift.Protocol.Binary
import Thrift.Server

-- Generated files
import qualified Batch
import           Batch_Iface
import qualified Defs_Types as TDefs
import qualified Graph_Types as TGraph
import qualified Types_Types as TTypes


port :: PortNumber
port = 30521

data BatchHandler = BatchHandler

newBatchHandler :: IO BatchHandler
newBatchHandler = do
    return $ BatchHandler

instance Batch_Iface BatchHandler where
    loadLibrary a library = putStrLn "NOT IMPLEMENTED - loadLibrary"
    unloadLibrary a library = putStrLn "NOT IMPLEMENTED - unloadLibrary"

    newDefinition a type' flags attrs = do 
                                            putStrLn "NOT IMPLEMENTED - newDefinition"
                                            return $ TDefs.NodeDefinition type' flags attrs (Just 0) (Just 0)
    addDefinition a (Just definition) parent = do
                                                 putStrLn "NOT IMPLEMENTED - addDefinition"
                                                 return $ definition
    updateDefinition a definition = putStrLn "NOT IMPLEMENTED - updateDefinition"
    removeDefinition a definition = putStrLn "NOT IMPLEMENTED - removeDefinition"

    definitionChildren a definition = do 
                                        putStrLn "NOT IMPLEMENTED - definitionChildren"
                                        return $ Vector.fromList [] :: IO (Vector TDefs.NodeDefinition)
    definitionParent a (Just definition) = do 
                                        putStrLn "NOT IMPLEMENTED - definitionParent"
                                        return definition

    newTypeModule   a name = do 
                                putStrLn "NOT IMPLEMENTED - newTypeModule"
                                return $ TTypes.Type $ Just $ Vector.fromList []
    newTypeClass    a name params = do
                                putStrLn "NOT IMPLEMENTED - newTypeClass"
                                return $ TTypes.Type $ Just $ Vector.fromList []
    newTypeFunction a name inputs outputs = do
                                putStrLn "NOT IMPLEMENTED - newTypeFunction"
                                return $ TTypes.Type $ Just $ Vector.fromList []
    newTypeUdefined a = do
                                putStrLn "NOT IMPLEMENTED - newTypeUdefined"
                                return $ TTypes.Type $ Just $ Vector.fromList []
    newTypeNamed    a name = do
                                putStrLn "NOT IMPLEMENTED - newTypeNamed"
                                return $ TTypes.Type $ Just $ Vector.fromList []
    newTypeVariable a name type' = do
                                putStrLn "NOT IMPLEMENTED - newTypeVariable"
                                return $ TTypes.Type $ Just $ Vector.fromList []
    newTypeList     a type' = do
                                putStrLn "NOT IMPLEMENTED - newTypeList"
                                return $ TTypes.Type $ Just $ Vector.fromList []
    newTypeTuple    a types = do
                                putStrLn "NOT IMPLEMENTED - newTypeTuple"
                                return $ TTypes.Type $ Just $ Vector.fromList []

    graph a definition = do
                            putStrLn "NOT IMPLEMENTED - graph"
                            return $ TGraph.Graph Nothing Nothing

    addNode a node definition = do
                            putStrLn "NOT IMPLEMENTED - addNode"
                            return $ TGraph.Node Nothing Nothing Nothing Nothing Nothing
    updateNode a node definition = do
                            putStrLn "NOT IMPLEMENTED - updateNode"
    removeNode a node definition = do
                            putStrLn "NOT IMPLEMENTED - removeNode"

    connect    a srcNode srcPort dstNode dstPort definition = do putStrLn "NOT IMPLEMENTED - connect"
    disconnect a srcNode srcPort dstNode dstPort definition = do putStrLn "NOT IMPLEMENTED - disconnect"
    ping a = putStrLn "ping"


main :: IO ()
main = do
    handler <- newBatchHandler
    putStrLn "Starting the server..."
    runBasicServer handler Batch.process port
    putStrLn "done."
