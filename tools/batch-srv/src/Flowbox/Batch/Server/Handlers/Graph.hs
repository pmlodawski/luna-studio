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

    nodeDefaults,
    setNodeDefault,
    removeNodeDefault,
) 
where

import           Prelude                                                   hiding (error)
import           Data.Int                                                    (Int32)
import           Data.IORef                                                  
import qualified Data.Map                                                  as Map

import qualified Data.HashMap.Strict                                       as HashMap
import           Data.HashMap.Strict                                         (HashMap)
import qualified Data.Vector                                               as Vector
import           Data.Vector                                                 (Vector)

import           Flowbox.Batch.Server.Handlers.Common                        (logger, tRunScript, vector2List)
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

------ public api -------------------------------------------------

nodesGraph :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO TGraphView.GraphView
nodesGraph batchHandler mtdefID mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called nodesGraph"
    defID     <- tryGetID mtdefID "defID"
    libID     <- tryGetID mtlibID "libID"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
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
    newBatch    <- tryRight $ BatchG.disconnect srcNodeID srcPort dstNodeID dstPort defID libID projectID batch
    tryWriteIORef batchHandler newBatch


nodeDefaults :: IORef Batch -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32
             -> IO (HashMap (Vector Int32) TGraph.DefaultValue)
nodeDefaults batchHandler mtnodeID mtdefID mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called nodeDefaults"
    scriptIO $ logger.error $ "not implemented"
    nodeID    <- tryGetID mtnodeID    "nodeID"
    defID     <- tryGetID mtdefID     "defID"
    libID     <- tryGetID mtlibID     "libID"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    defaults  <- tryRight $ BatchG.nodeDefaults nodeID defID libID projectID batch
    let encodeMapItem (k, v) = (Vector.fromList $ map itoi32 k, encode v)
        tdefaults = HashMap.fromList $ map encodeMapItem $ Map.toList defaults
    return tdefaults


setNodeDefault :: IORef Batch -> Maybe (Vector Int32) -> Maybe TGraph.DefaultValue
               -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO ()
setNodeDefault batchHandler mtdstPort mtvalue mtnodeID mtdefID mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called setNodeDefault"
    scriptIO $ logger.error $ "not implemented"
    tdstPort  <- mtdstPort <??> "'dstPort' argument is missing"
    let dstPort = vector2List tdstPort
    tvalue    <- mtvalue   <??> "'value' argument is missing"
    value     <- tryRight $ decode tvalue
    nodeID    <- tryGetID mtnodeID    "nodeID"
    defID     <- tryGetID mtdefID     "defID"
    libID     <- tryGetID mtlibID     "libID"
    projectID <- tryGetID mtprojectID "projectID"
    batch     <- tryReadIORef batchHandler
    newBatch  <- tryRight $ BatchG.setNodeDefault dstPort value nodeID defID libID projectID batch
    tryWriteIORef batchHandler newBatch


removeNodeDefault :: IORef Batch -> Maybe (Vector Int32)
                  -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> Maybe Int32 -> IO ()
removeNodeDefault batchHandler mtdstPort mtnodeID mtdefID mtlibID mtprojectID = tRunScript $ do
    scriptIO $ logger.info $ "called removeNodeDefault"
    scriptIO $ logger.error $ "not implemented"
    tdstPort  <- mtdstPort <??> "'dstPort' argument is missing"
    let dstPort = vector2List tdstPort
    nodeID    <- tryGetID mtnodeID    "nodeID"
    defID     <- tryGetID mtdefID     "defID"
    libID     <- tryGetID mtlibID     "libID"
    projectID <- tryGetID mtprojectID "projectID"   
    batch     <- tryReadIORef batchHandler
    newBatch  <- tryRight $ BatchG.removeNodeDefault dstPort nodeID defID libID projectID batch
    tryWriteIORef batchHandler newBatch
    