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
import qualified Graph_Types                                               as TGraph
import qualified Graphview_Types                                           as TGraphView
import           Flowbox.Batch.Batch                                         (Batch(..))
import qualified Flowbox.Batch.Handlers.Graph                              as BatchG
import           Flowbox.Batch.Tools.Serialize.Thrift.Conversion.GraphView   ()
import           Flowbox.Control.Error                                       
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Graph        ()
import           Flowbox.Tools.Conversion                                    

            
------ public api -------------------------------------------------

nodesGraph :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> IO TGraphView.GraphView
nodesGraph batchHandler mtdefID mtlibID = tRunScript $ do
    scriptIO $ putStrLn "called nodesGraph"
    defID  <- tryGetID mtdefID "defID"
    libID  <- tryGetID mtlibID "libID"
    batch  <- tryReadIORef batchHandler
    agraph <- tryRight $ BatchG.nodesGraph defID libID batch
    return $ encode agraph


addNode :: IORef Batch -> Maybe TGraph.Node -> Maybe Int32 -> Maybe Int32 -> IO TGraph.Node
addNode batchHandler mtnode mtdefID mtlibID = tRunScript $ do
    scriptIO $ putStrLn "called addNode"
    tnode     <- mtnode <??> "'node' argument is missing"
    (_, node) <- tryRight $ decode tnode
    defID     <- tryGetID mtdefID "defID"
    libID     <- tryGetID mtlibID "libID"
    batch     <- tryReadIORef batchHandler
    (newBatch, nodeID) <- tryRight $ BatchG.addNode node defID libID batch
    tryWriteIORef batchHandler newBatch
    return $ encode (nodeID, node)


updateNode :: IORef Batch -> Maybe TGraph.Node -> Maybe Int32 -> Maybe Int32 -> IO ()
updateNode batchHandler mtnode mtdefID mtlibID = tRunScript $ do 
    scriptIO $ putStrLn "called updateNode"
    tnode  <- mtnode <??> "'node' argument is missing"
    node   <- tryRight $ decode tnode
    defID  <- tryGetID mtdefID "defID"
    libID  <- tryGetID mtlibID "libID"
    batch  <- tryReadIORef batchHandler
    newBatch <- tryRight $ BatchG.updateNode node defID libID batch
    tryWriteIORef batchHandler newBatch
    return ()


removeNode :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO ()
removeNode batchHandler mtnodeID mtdefID mtlibID = tRunScript $ do
    scriptIO $ putStrLn "called removeNode"
    nodeID   <- tryGetID mtnodeID "nodeID"
    defID    <- tryGetID mtdefID  "defID"
    libID    <- tryGetID mtlibID  "libID"
    batch    <- tryReadIORef batchHandler
    newBatch <- tryRight $ BatchG.removeNode nodeID defID libID batch
    tryWriteIORef batchHandler newBatch
    return ()


connect :: IORef Batch -> Maybe Int32 -> Maybe TGraphView.PortDescriptor
                       -> Maybe Int32 -> Maybe TGraphView.PortDescriptor
                       -> Maybe Int32 -> Maybe Int32 -> IO ()
connect batchHandler mtsrcNodeID mtsrcPort mtdstNodeID mtdstPort mtdefID mtlibID = tRunScript $ do     
    scriptIO $ putStrLn "called connect"
    srcNodeID   <- tryGetID mtsrcNodeID "srcNodeID"
    tsrcPort    <- mtsrcPort <??> "'srcPort' field is missing"
    let vectorToList = map i32toi . Vector.toList
        srcPort = vectorToList tsrcPort
    dstNodeID   <- tryGetID mtdstNodeID "dstNodeID"
    tdstPort    <- mtdstPort <??> "'dstPort' field is missing"
    let dstPort = vectorToList tdstPort
    defID       <- tryGetID mtdefID     "defID"
    libID       <- tryGetID mtlibID     "libID"
    batch       <- tryReadIORef batchHandler
    newBatch    <- tryRight $ BatchG.connect srcNodeID srcPort dstNodeID dstPort defID libID batch
    tryWriteIORef batchHandler newBatch
    return ()


disconnect :: IORef Batch -> Maybe Int32 -> Maybe TGraphView.PortDescriptor
                          -> Maybe Int32 -> Maybe TGraphView.PortDescriptor
                          -> Maybe Int32 -> Maybe Int32 -> IO ()
disconnect batchHandler mtsrcNodeID mtsrcPort mtdstNodeID mtdstPort mtdefID mtlibID = tRunScript $ do     
    scriptIO $ putStrLn "called disconnect"
    srcNodeID   <- tryGetID mtsrcNodeID "srcNodeID"
    tsrcPort    <- mtsrcPort <??> "'srcPort' field is missing"
    let vectorToList = map i32toi . Vector.toList
        srcPort = vectorToList tsrcPort
    dstNodeID   <- tryGetID mtdstNodeID "dstNodeID"
    tdstPort    <- mtdstPort <??> "'dstPort' field is missing"
    let dstPort = vectorToList tdstPort
    defID       <- tryGetID mtdefID     "defID"
    libID       <- tryGetID mtlibID     "libID"
    batch       <- tryReadIORef batchHandler
    newBatch    <- tryRight $ BatchG.disconnect srcNodeID srcPort dstNodeID dstPort defID libID batch
    tryWriteIORef batchHandler newBatch


