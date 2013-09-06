---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Graph (
    nodesGraph,
    nodeByID,
    
    addNode,
    updateNode,
    removeNode,
    connect,
    disconnect,
) 
where

import           Prelude                                                   hiding (error)
import           Data.Int                                                    (Int32)
import           Data.IORef                                                  

import           Flowbox.Batch.Server.Handlers.Common                        (tRunScript, vector2List)
import qualified Graph_Types                                               as TGraph
import qualified Graphview_Types                                           as TGraphView
import           Flowbox.Batch.Batch                                         (Batch(..))
import qualified Flowbox.Batch.Handlers.Graph                              as BatchG
import           Flowbox.Batch.Tools.Serialize.Thrift.Conversion.GraphView   ()
import           Flowbox.Control.Error                                       
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Graph        ()
import           Flowbox.System.Log.Logger                                   
import           Flowbox.Tools.Conversion                                    



logger = getLoggerIO "Flowbox.Batch.Server.Handlers.Graph"

------ public api -------------------------------------------------

nodesGraph :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO TGraphView.GraphView
nodesGraph batchHandler mtdefID mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called nodesGraph"
    defID     <- tryGetID mtdefID "defID"
    libID     <- tryGetID mtlibID "libID"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    scriptIO $ logger.debug $ "defID: " ++ (show defID) ++ " libID: " ++ (show libID) ++ " projectID: " ++ (show projectID)
    agraph    <- tryRight $ BatchG.nodesGraph defID libID projectID batch
    return $ encode agraph


nodeByID :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO TGraph.Node
nodeByID batchHandler mtnodeID mtdefID mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called nodeByID"
    nodeID    <- tryGetID mtnodeID    "nodeID"
    defID     <- tryGetID mtdefID     "defID"
    libID     <- tryGetID mtlibID     "libID"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    scriptIO $ logger.debug $ "nodeID: " ++ (show nodeID) ++ " defID: " ++ (show defID) ++ " libID: " ++ (show libID) ++ " projectID: " ++ (show projectID)
    node <- tryRight $ BatchG.nodeByID nodeID defID libID projectID batch
    return $ encode (nodeID, node)


addNode :: IORef Batch -> Maybe TGraph.Node -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO TGraph.Node
addNode batchHandler mtnode mtdefID mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called addNode"
    tnode     <- mtnode <??> "'node' argument is missing"
    (_, node) <- tryRight $ decode tnode
    defID     <- tryGetID mtdefID "defID"
    libID     <- tryGetID mtlibID "libID"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    scriptIO $ logger.debug $ "node: " ++ (show node) ++ " defID: " ++ (show defID) ++ " libID: " ++ (show libID) ++ " projectID: " ++ (show projectID)
    (newBatch, nodeID) <- tryRight $ BatchG.addNode node defID libID projectID batch
    tryWriteIORef batchHandler newBatch
    return $ encode (nodeID, node)


updateNode :: IORef Batch -> Maybe TGraph.Node -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO ()
updateNode batchHandler mtnode mtdefID mtlibID mtprojectID = tRunScript $ do 
    scriptIO $ logger.info $ "called updateNode"
    tnode     <- mtnode <??> "'node' argument is missing"
    node      <- tryRight $ decode tnode
    defID     <- tryGetID mtdefID "defID"
    libID     <- tryGetID mtlibID "libID"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    scriptIO $ logger.debug $ "node: " ++ (show node) ++ " defID: " ++ (show defID) ++ " libID: " ++ (show libID) ++ " projectID: " ++ (show projectID)
    newBatch  <- tryRight $ BatchG.updateNode node defID libID projectID batch
    tryWriteIORef batchHandler newBatch
    return ()


removeNode :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO ()
removeNode batchHandler mtnodeID mtdefID mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called removeNode"
    nodeID    <- tryGetID mtnodeID "nodeID"
    defID     <- tryGetID mtdefID  "defID"
    libID     <- tryGetID mtlibID  "libID"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    scriptIO $ logger.debug $ "nodeID: " ++ (show nodeID) ++ " defID: " ++ (show defID) ++ " libID: " ++ (show libID) ++ " projectID: " ++ (show projectID)
    newBatch  <- tryRight $ BatchG.removeNode nodeID defID libID projectID batch
    tryWriteIORef batchHandler newBatch
    return ()


connect :: IORef Batch -> Maybe Int32 -> Maybe TGraphView.PortDescriptor
                       -> Maybe Int32 -> Maybe TGraphView.PortDescriptor
                       -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO ()
connect batchHandler mtsrcNodeID mtsrcPort mtdstNodeID mtdstPort mtdefID mtlibID mtprojectID = tRunScript $ do     
    scriptIO $ logger.info $ "called connect"
    srcNodeID <- tryGetID mtsrcNodeID "srcNodeID"
    tsrcPort  <- mtsrcPort <??> "'srcPort' argument is missing"
    let srcPort = vector2List tsrcPort
    dstNodeID <- tryGetID mtdstNodeID "dstNodeID"
    tdstPort  <- mtdstPort <??> "'dstPort' argument is missing"
    let dstPort = vector2List tdstPort
    defID     <- tryGetID mtdefID     "defID"
    libID     <- tryGetID mtlibID     "libID"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    scriptIO $ logger.debug $ "srcNodeID: " ++ (show srcPort) ++ " srcPort: " ++ (show dstNodeID) ++ " dstNodeID: " ++ (show srcNodeID) ++ " dstPort: " ++ (show dstPort) ++ " defID: " ++ (show defID) ++ " libID: " ++ (show libID) ++ " projectID: " ++ (show projectID)
    newBatch  <- tryRight $ BatchG.connect srcNodeID srcPort dstNodeID dstPort defID libID projectID batch
    tryWriteIORef batchHandler newBatch
    return ()


disconnect :: IORef Batch -> Maybe Int32 -> Maybe TGraphView.PortDescriptor
                          -> Maybe Int32 -> Maybe TGraphView.PortDescriptor
                          -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO ()
disconnect batchHandler mtsrcNodeID mtsrcPort mtdstNodeID mtdstPort mtdefID mtlibID mtprojectID = tRunScript $ do     
    scriptIO $ logger.info $ "called disconnect"
    srcNodeID   <- tryGetID mtsrcNodeID "srcNodeID"
    tsrcPort    <- mtsrcPort <??> "'srcPort' argument is missing"
    let srcPort = vector2List tsrcPort
    dstNodeID   <- tryGetID mtdstNodeID "dstNodeID"
    tdstPort    <- mtdstPort <??> "'dstPort' argument is missing"
    let dstPort = vector2List tdstPort
    defID       <- tryGetID mtdefID     "defID"
    libID       <- tryGetID mtlibID     "libID"
    projectID   <- tryGetID mtprojectID "projectID"
    batch       <- tryReadIORef batchHandler
    scriptIO $ logger.debug $ "srcNodeID: " ++ (show srcPort) ++ " srcPort: " ++ (show dstNodeID) ++ " dstNodeID: " ++ (show srcNodeID) ++ " dstPort: " ++ (show dstPort) ++ " defID: " ++ (show defID) ++ " libID: " ++ (show libID) ++ " projectID: " ++ (show projectID)
    newBatch    <- tryRight $ BatchG.disconnect srcNodeID srcPort dstNodeID dstPort defID libID projectID batch
    tryWriteIORef batchHandler newBatch

