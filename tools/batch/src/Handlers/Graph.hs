---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Handlers.Graph (
graph,
addNode,
updateNode,
removeNode,
connect,
disconnect
) 
where

import           Control.Monad
import           Data.IORef
import qualified Data.Vector    as Vector

import           Handlers.Common
import           Handlers.Defs                   (defOperation)
import qualified Defs_Types                    as TDefs
import qualified Graph_Types                   as TGraph
import qualified Luna.Core                     as Core
import           Luna.Core                       (Core)
import qualified Luna.Network.Def.DefManager   as DefManager
import qualified Luna.Network.Def.Definition      as Definition
import           Luna.Network.Def.Definition        (Definition)
import qualified Luna.Network.Graph.Graph      as Graph
import qualified Luna.Network.Graph.Node       as Node
import           Luna.Network.Graph.Node         (Node(..))
import           Luna.Tools.Conversion
import           Luna.Tools.Conversion.Defs    ()
import           Luna.Tools.Conversion.Graph   ()


graph :: IORef Core -> Maybe TDefs.Definition -> IO TGraph.Graph
graph = defOperation (\batchHandler defID definition -> do
    putStrLn "called graph"
    core <- readIORef batchHandler
    let defManager = Core.defManager core
        agraph = Definition.graph definition
    return $ encode agraph)


addNode :: IORef Core -> Maybe TGraph.Node -> Maybe TDefs.Definition -> IO TGraph.Node
addNode = nodeDefOperation (\batchHandler (_, node) (defID, definition) -> do
    putStrLn "called addNode"
    core <- readIORef batchHandler
    let defManager    = Core.defManager core
        agraph        = Definition.graph definition
        [nodeID]      = Graph.newNodes 1 agraph
        newGraph      = Graph.insNode (nodeID, node) agraph
        newDefinition = definition {Definition.graph =  newGraph}
        newDefManager = DefManager.updateNode (defID, newDefinition) defManager 
        newCore       = core {Core.defManager = newDefManager}
    writeIORef batchHandler newCore

    return $ encode (nodeID, node))


updateNode :: IORef Core -> Maybe TGraph.Node -> Maybe TDefs.Definition -> IO ()
updateNode = nodeDefOperation (\batchHandler (nodeID, node) (defID, definition) -> do 
    putStrLn "called updateNode"
    core <- readIORef batchHandler
    let defManager    = Core.defManager core
        agraph        = Definition.graph definition
    case Graph.gelem nodeID agraph of 
        False -> throw' $ "Wrong `nodeID` in `node`"
        True  -> do 
            let newGraph      = Graph.updateNode (nodeID, node) agraph
                newDefinition = definition {Definition.graph =  newGraph}
                newDefManager = DefManager.updateNode (defID, newDefinition) defManager 
                newCore       = core {Core.defManager = newDefManager}
            writeIORef batchHandler newCore)


removeNode :: IORef Core -> Maybe TGraph.Node -> Maybe TDefs.Definition -> IO ()
removeNode = nodeDefOperation (\batchHandler (nodeID, _) (defID, definition) -> do
    putStrLn "called removeNode"
    core <- readIORef batchHandler
    let defManager    = Core.defManager core
        agraph        = Definition.graph definition
    case Graph.gelem nodeID agraph of 
        False -> throw' $ "Wrong `nodeID` in `node`"
        True  -> do 
            let newGraph      = Graph.delNode nodeID agraph
                newDefinition = definition {Definition.graph =  newGraph}
                newDefManager = DefManager.updateNode (defID, newDefinition) defManager 
                newCore       = core {Core.defManager = newDefManager}
            writeIORef batchHandler newCore)

connect :: IORef Core -> Maybe TGraph.Node -> Maybe TGraph.PortDescriptor
                      -> Maybe TGraph.Node -> Maybe TGraph.PortDescriptor
        -> Maybe TDefs.Definition -> IO ()
connect = nodesConnectOperation (\batchHandler (srcNodeID, srcNode) srcPort 
                                               (dstNodeID, dstNode) dstPort definition -> do 
    putStrLn "call connect - NOT IMPLEMENTED")


disconnect :: IORef Core -> Maybe TGraph.Node -> Maybe TGraph.PortDescriptor
                         -> Maybe TGraph.Node -> Maybe TGraph.PortDescriptor
           -> Maybe TDefs.Definition -> IO ()
disconnect = nodesConnectOperation (\batchHandler (srcNodeID, srcNode) srcPort
                                                  (dstNodeID, dstNode) dstPort definition -> do 
    putStrLn "call disconnect - NOT IMPLEMENTED")


nodesConnectOperation :: (IORef Core -> (Node.ID, Node) -> [Int]
                                     -> (Node.ID, Node) -> [Int]
                                     -> Definition -> IO result)
                      -> IORef Core -> (Maybe TGraph.Node) -> (Maybe TGraph.PortDescriptor)
                                    -> (Maybe TGraph.Node) -> (Maybe TGraph.PortDescriptor)
                      -> Maybe TDefs.Definition -> IO result
nodesConnectOperation operation batchHandler mtsrcNode mtsrcPort mtdstNode mtdstPort mtdefinition = do
    core <- readIORef batchHandler
    let defManager = Core.defManager core
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
                                        Just defID -> case DefManager.lab defManager defID of 
                                            Nothing         -> throw' $ "Wrong `defID` in `definition`"
                                            Just definition -> let vectorToList = map i32toi . Vector.toList
                                                                   srcPort = vectorToList tsrcPort
                                                                   dstPort = vectorToList tdstPort 
                                                               in operation batchHandler (srcNodeID, srcNode) srcPort (dstNodeID, dstNode) dstPort definition


nodeDefOperation :: (IORef Core -> (Node.ID, Node) -> (Definition.ID, Definition) -> IO result) 
                 -> IORef Core -> Maybe TGraph.Node -> Maybe TDefs.Definition -> IO result
nodeDefOperation operation batchHandler mtnode mtdefinition = do 
    core <- readIORef batchHandler
    let defManager = Core.defManager core
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
                        Just defID -> case DefManager.lab defManager defID of 
                            Nothing         -> throw' $ "Wrong `defID` in `definition`"
                            Just definition -> operation batchHandler (nodeID, node) (defID, definition)
            