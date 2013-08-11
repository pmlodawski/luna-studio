---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Graph (
    graph,
    addNode,
    updateNode,
    removeNode,
    connect,
    disconnect
) 
where

import           Control.Monad
import           Data.Int
import           Data.IORef
import qualified Data.Vector    as Vector

import           Flowbox.Batch.Server.Handlers.Common
import           Flowbox.Batch.Server.Handlers.Defs                          (defOperation)
import qualified Defs_Types                                                as TDefs
import qualified Graph_Types                                               as TGraph
import qualified Graphview_Types                                           as TGraphView
import qualified Flowbox.Batch.Batch                                       as Batch
import           Flowbox.Batch.Batch                                         (Batch(..))
import           Flowbox.Batch.Tools.Serialize.Thrift.Conversion.GraphView   ()
import qualified Flowbox.Luna.Network.Def.Definition                       as Definition
import qualified Flowbox.Luna.Network.Graph.Node                           as Node
import           Flowbox.Luna.Network.Graph.Node                             (Node(..))
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Graph        ()


graph :: IORef Batch -> Maybe TDefs.Definition -> IO TGraphView.GraphView
graph = defOperation (\batchHandler defID _ -> do
    putStrLn "called graph"
    batch <- readIORef batchHandler
    case Batch.nodesGraph defID batch of
        Left message -> throw' message
        Right agraph -> return $ encode agraph)


addNode :: IORef Batch -> Maybe TGraph.Node -> Maybe TDefs.Definition -> IO TGraph.Node
addNode = nodeDefOperation (\batchHandler (_, node) defID -> do
    putStrLn "called addNode"
    batch <- readIORef batchHandler
    case Batch.addNode node defID batch of
        Left  message            -> throw' message
        Right (newBatch, nodeID) -> do
            writeIORef batchHandler newBatch
            return $ encode (nodeID, node))


updateNode :: IORef Batch -> Maybe TGraph.Node -> Maybe TDefs.Definition -> IO ()
updateNode = nodeDefOperation (\batchHandler (nodeID, node) defID -> do 
    putStrLn "called updateNode"
    batch <- readIORef batchHandler
    case Batch.updateNode (nodeID, node) defID batch of
        Left  message  -> throw' message
        Right newBatch -> do
            writeIORef batchHandler newBatch)


removeNode :: IORef Batch -> Maybe TGraph.Node -> Maybe TDefs.Definition -> IO ()
removeNode = nodeDefOperation (\batchHandler (nodeID, _) defID-> do
    putStrLn "called removeNode"
    batch <- readIORef batchHandler
    case Batch.removeNode nodeID defID batch of
        Left  message  -> throw' message
        Right newBatch -> do
            writeIORef batchHandler newBatch)

connect :: IORef Batch -> Maybe TGraph.Node -> Maybe TGraphView.PortDescriptor
                      -> Maybe TGraph.Node -> Maybe Int32
        -> Maybe TDefs.Definition -> IO ()
connect = nodesConnectOperation (\batchHandler (srcNodeID, _) srcPort 
                                               (dstNodeID, _) dstPort defID -> do 
    putStrLn "called connect"
    batch <- readIORef batchHandler
    case Batch.connect srcNodeID srcPort dstNodeID dstPort defID batch of
        Left  message  -> throw' message
        Right newBatch -> do
            writeIORef batchHandler newBatch)


disconnect :: IORef Batch -> Maybe TGraph.Node -> Maybe TGraphView.PortDescriptor
                         -> Maybe TGraph.Node -> Maybe Int32
           -> Maybe TDefs.Definition -> IO ()
disconnect = nodesConnectOperation (\batchHandler (srcNodeID, _) srcPort
                                                  (dstNodeID, _) dstPort defID -> do 
    putStrLn "called disconnect"
    batch <- readIORef batchHandler
    case Batch.disconnect srcNodeID srcPort dstNodeID dstPort defID batch of
        Left  message  -> throw' message
        Right newBatch -> do
            writeIORef batchHandler newBatch)


------ public api helpers -----------------------------------------

nodesConnectOperation :: (IORef Batch -> (Node.ID, Node) -> [Int]
                                     -> (Node.ID, Node) -> Int
                                     -> Definition.ID -> IO result)
                      -> IORef Batch -> (Maybe TGraph.Node) -> (Maybe TGraphView.PortDescriptor)
                                    -> (Maybe TGraph.Node) -> (Maybe Int32)
                      -> Maybe TDefs.Definition -> IO result
nodesConnectOperation operation batchHandler mtsrcNode mtsrcPort mtdstNode mtdstPort mtdefinition = do
    case mtsrcNode of
        Nothing       -> throw' "`srcNode` field is missing"
        Just tsrcNode -> case decode tsrcNode of
            Left message               -> throw' $ "Failed to decode `srcNode` : " ++ message
            Right (srcNodeID, srcNode) -> case mtsrcPort of
                Nothing       -> throw' "`srcPort` field is missing"
                Just tsrcPort -> case mtdstNode of 
                    Nothing       -> throw' "`dstNode` field is missing"
                    Just tdstNode -> case decode tdstNode of
                        Left message               -> throw' $ "Failed to decode `dstNode` : " ++ message
                        Right (dstNodeID, dstNode) -> case mtdstPort of 
                            Nothing       -> throw' "`dstPort` field is missing"
                            Just tdstPort -> case mtdefinition of
                                Nothing          -> throw' "`definition` field is missing"
                                Just tdefinition -> do
                                    let mdefID = liftM i32toi $ TDefs.f_Definition_defID tdefinition
                                    case mdefID of 
                                        Nothing    -> throw' "`defID` field is missing"
                                        Just defID -> let vectorToList = map i32toi . Vector.toList
                                                          srcPort = vectorToList tsrcPort
                                                          dstPort = i32toi tdstPort 
                                                      in operation batchHandler (srcNodeID, srcNode) srcPort (dstNodeID, dstNode) dstPort defID


nodeDefOperation :: (IORef Batch -> (Node.ID, Node) -> Definition.ID -> IO result) 
                 -> IORef Batch -> Maybe TGraph.Node -> Maybe TDefs.Definition -> IO result
nodeDefOperation operation batchHandler mtnode mtdefinition = do 
    case mtnode of
        Nothing    -> throw' "`node` field is missing"
        Just tnode -> case decode tnode of 
            Left message         -> throw' $ "Failed to decode `node` field: " ++ message
            Right (nodeID, node) -> case mtdefinition of 
                Nothing          -> throw' "`definition` field is missing"
                Just tdefinition -> do
                    let mdefID = liftM i32toi $ TDefs.f_Definition_defID tdefinition
                    case mdefID of
                        Nothing    -> throw' "`defID` field is missing"
                        Just defID -> operation batchHandler (nodeID, node) defID
            