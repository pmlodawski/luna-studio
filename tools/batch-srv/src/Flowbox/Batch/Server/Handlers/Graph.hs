---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Graph (
    nodesGraph,
    
    addNode,
    updateNode,
    removeNode,
    connect,
    disconnect
) 
where


import           Data.Int                                                    
import           Data.IORef                                                  
import qualified Data.Vector                                               as Vector

import           Flowbox.Batch.Server.Handlers.Common                        
import qualified Defs_Types                                                as TDefs
import qualified Graph_Types                                               as TGraph
import qualified Graphview_Types                                           as TGraphView
import qualified Libs_Types                                                as TLibs
import qualified Flowbox.Batch.Batch                                       as Batch
import           Flowbox.Batch.Batch                                         (Batch(..))
import           Flowbox.Batch.Tools.Serialize.Thrift.Conversion.GraphView   ()
import qualified Flowbox.Luna.Lib.Library                                  as Library
import qualified Flowbox.Luna.Network.Def.Definition                       as Definition
import qualified Flowbox.Luna.Network.Graph.Node                           as Node
import           Flowbox.Luna.Network.Graph.Node                             (Node(..))
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion   
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Graph        ()


------ public api helpers -----------------------------------------

--nodesConnectOperation :: ((Node.ID, Node) -> [Int]
--                          -> (Node.ID, Node) -> Int
--                          -> Definition.ID -> Library.ID -> result)
--                      -> (Maybe TGraph.Node) -> (Maybe TGraphView.PortDescriptor)
--                      -> (Maybe TGraph.Node) -> (Maybe Int32)
--                      -> Maybe TDefs.Definition -> Maybe TLibs.Library -> result
--nodesConnectOperation operation mtsrcNode mtsrcPort mtdstNode mtdstPort = 
--    defOperation(\ (defID, _) libID -> case mtsrcNode of
--        Nothing       -> throw' "`srcNode` field is missing"
--        Just tsrcNode -> case decode tsrcNode of
--            Left message               -> throw' $ "Failed to decode `srcNode` : " ++ message
--            Right (srcNodeID, srcNode) -> case mtsrcPort of
--                Nothing       -> throw' "`srcPort` field is missing"
--                Just tsrcPort -> case mtdstNode of 
--                    Nothing       -> throw' "`dstNode` field is missing"
--                    Just tdstNode -> case decode tdstNode of
--                        Left message               -> throw' $ "Failed to decode `dstNode` : " ++ message
--                        Right (dstNodeID, dstNode) -> case mtdstPort of 
--                            Nothing       -> throw' "`dstPort` field is missing"
--                            Just tdstPort -> let vectorToList = map i32toi . Vector.toList
--                                                 srcPort = vectorToList tsrcPort
--                                                 dstPort = i32toi tdstPort 
--                                             in operation (srcNodeID, srcNode) srcPort (dstNodeID, dstNode) dstPort defID libID)


--nodeDefOperation :: ((Node.ID, Node) -> Definition.ID -> Library.ID -> result) 
--                 -> Maybe TGraph.Node 
--                 -> Maybe TDefs.Definition -> Maybe TLibs.Library -> result
--nodeDefOperation operation mtnode = 
--    defOperation (\ (defID, _) libID -> case mtnode of
--        Nothing    -> throw' "`node` field is missing"
--        Just tnode -> case decode tnode of 
--            Left message         -> throw' $ "Failed to decode `node` field: " ++ message
--            Right (nodeID, node) -> operation (nodeID, node) defID libID)

            
------ public api -------------------------------------------------

nodesGraph :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> IO TGraphView.GraphView
nodesGraph batchHandler = defOperation (\(defID, _) libID -> do
    putStrLn "called graph"
    batch <- readIORef batchHandler
    case Batch.nodesGraph defID libID batch of
        Left message -> throw' message
        Right agraph -> return $ encode agraph)


addNode :: IORef Batch -> Maybe TGraph.Node -> Maybe Int32 -> Maybe Int32 -> IO TGraph.Node
addNode batchHandler = nodeDefOperation (\(_, node) defID libID -> do
    putStrLn "called addNode"
    batch <- readIORef batchHandler
    case Batch.addNode node defID libID batch of
        Left  message            -> throw' message
        Right (newBatch, nodeID) -> do
            writeIORef batchHandler newBatch
            return $ encode (nodeID, node))


updateNode :: IORef Batch -> Maybe TGraph.Node -> Maybe Int32 -> Maybe Int32 -> IO ()
updateNode batchHandler = nodeDefOperation (\(nodeID, node) defID libID -> do 
    putStrLn "called updateNode"
    batch <- readIORef batchHandler
    case Batch.updateNode (nodeID, node) defID libID batch of
        Left  message  -> throw' message
        Right newBatch -> do
            writeIORef batchHandler newBatch)


removeNode :: IORef Batch -> Maybe TGraph.Node -> Maybe Int32 -> Maybe Int32 -> IO ()
removeNode batchHandler = nodeDefOperation (\(nodeID, _) defID libID -> do
    putStrLn "called removeNode"
    batch <- readIORef batchHandler
    case Batch.removeNode nodeID defID libID batch of
        Left  message  -> throw' message
        Right newBatch -> do
            writeIORef batchHandler newBatch)


connect :: IORef Batch -> Maybe TGraph.Node -> Maybe TGraphView.PortDescriptor
                       -> Maybe TGraph.Node-> Maybe Int32
                       -> Maybe Int32 -> Maybe Int32 -> IO ()
connect batchHandler = nodesConnectOperation (\(srcNodeID, _) srcPort 
                                               (dstNodeID, _) dstPort defID libID -> do 
    putStrLn "called connect"
    batch <- readIORef batchHandler
    case Batch.connect srcNodeID srcPort dstNodeID dstPort defID libID batch of
        Left  message  -> throw' message
        Right newBatch -> do
            writeIORef batchHandler newBatch)


disconnect :: IORef Batch -> Maybe TGraph.Node -> Maybe TGraphView.PortDescriptor
                          -> Maybe TGraph.Node -> Maybe Int32 
                          -> Maybe Int32 -> Maybe Int32 -> IO ()
disconnect batchHandler = nodesConnectOperation (\(srcNodeID, _) srcPort
                                                  (dstNodeID, _) dstPort defID libID -> do 
    putStrLn "called disconnect"
    batch <- readIORef batchHandler
    case Batch.disconnect srcNodeID srcPort dstNodeID dstPort defID libID batch of
        Left  message  -> throw' message
        Right newBatch -> do
            writeIORef batchHandler newBatch)


