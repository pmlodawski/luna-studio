---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module GraphHandler (
graph,
addNode,
updateNode,
removeNode,
connect,
disconnect
) 
where

import           Control.Exception
import           Control.Monad
import           Data.IORef
import           Data.Text.Lazy   (pack, unpack)
import qualified Data.Vector    as Vector
import           Data.Vector      (Vector)

import           Batch_Types (ArgumentException(..))
import           DefinitionHandler               (defOperation)
import qualified Defs_Types                    as TDefs
import qualified Graph_Types                   as TGraph
import qualified Luna.Core                     as Core
import           Luna.Core                       (Core)
import qualified Luna.Network.Def.DefManager   as DefManager
import qualified Luna.Network.Def.NodeDef      as NodeDef
import           Luna.Network.Def.NodeDef        (NodeDef)
import qualified Luna.Network.Graph.Graph      as Graph
import qualified Luna.Network.Graph.Node       as Node
import           Luna.Network.Graph.Node         (Node(..))
import           Luna.Tools.Serialization
import           Luna.Tools.Serialization.Defs    ()
import           Luna.Tools.Serialization.Graph   ()


graph :: IORef Core -> Maybe TDefs.NodeDef -> IO TGraph.Graph
graph = defOperation (\batchHandler defID definition -> do
    putStrLn "called graph"
    core <- readIORef batchHandler
    let defManager = Core.defManager core
        graph = NodeDef.graph definition
    return $ encode graph)



addNode :: IORef Core -> Maybe TGraph.Node -> Maybe TDefs.NodeDef -> IO TGraph.Node
addNode = nodeDefOperation (\batchHandler (_, node) (defID, definition) -> do
    putStrLn "called addNode"
    core <- readIORef batchHandler
    let defManager    = Core.defManager core
        agraph        = NodeDef.graph definition
        [nodeID]      = Graph.newNodes 1 agraph
        newGraph      = Graph.insNode (nodeID, node) agraph
        newDefinition = definition {NodeDef.graph =  newGraph}
        newDefManager = DefManager.updateNode (defID, newDefinition) defManager 
        newCore       = core {Core.defManager = newDefManager}
    print newCore
    writeIORef batchHandler newCore

    return $ encode (nodeID, node))


updateNode :: IORef Core -> Maybe TGraph.Node -> Maybe TDefs.NodeDef -> IO ()
updateNode = nodeDefOperation (\batchHandler (nodeID, node) (defID, definition) -> do 
    --TODO [PM] implement case that nodeID not exist
    putStrLn "called updateNode"
    core <- readIORef batchHandler
    let defManager    = Core.defManager core
        agraph        = NodeDef.graph definition
        newGraph      = Graph.updateNode (nodeID, node) agraph
        newDefinition = definition {NodeDef.graph =  newGraph}
        newDefManager = DefManager.updateNode (defID, newDefinition) defManager 
        newCore       = core {Core.defManager = newDefManager}
    print newCore
    writeIORef batchHandler newCore)


removeNode :: IORef Core -> Maybe TGraph.Node -> Maybe TDefs.NodeDef -> IO ()
removeNode = nodeDefOperation (\batchHandler (nodeID, _) (defID, definition) -> do
    --TODO [PM] implement case that nodeID not exist
    putStrLn "called removeNode - NOT IMPLEMENTED"
    core <- readIORef batchHandler
    let defManager    = Core.defManager core
        agraph        = NodeDef.graph definition
        newGraph      = Graph.delNode nodeID agraph
        newDefinition = definition {NodeDef.graph =  newGraph}
        newDefManager = DefManager.updateNode (defID, newDefinition) defManager 
        newCore       = core {Core.defManager = newDefManager}
    print newCore
    writeIORef batchHandler newCore)


connect    batchHandler mtsrcNode mtsrcPort mtdstNode mtdstPort mtdefinition = do 
    putStrLn "NOT IMPLEMENTED - connect"


disconnect batchHandler mtsrcNode mtsrcPort mtdstNode mtdstPort mtdefinition = do 
    putStrLn "NOT IMPLEMENTED - disconnect"


nodeDefOperation :: (IORef Core -> (Node.ID, Node) -> (NodeDef.ID, NodeDef) -> IO result) 
                 -> IORef Core -> Maybe TGraph.Node -> Maybe TDefs.NodeDef -> IO result
nodeDefOperation operation batchHandler mtnode mtdefinition = do 
    core <- readIORef batchHandler
    let defManager = Core.defManager core
    case mtnode of
        Nothing    -> throw $ ArgumentException $ Just $ pack "`node` field is missing"
        Just tnode -> case decode tnode of 
            Left message         -> throw $ ArgumentException $ Just $ pack $ "Failed to decode `node` field: " ++ message
            Right (nodeID, node) -> case mtdefinition of 
                Nothing          -> throw $ ArgumentException $ Just $ pack "`definition` field is missing"
                Just tdefinition -> do
                    let mdefID = liftM i32toi $ TDefs.f_NodeDef_defID tdefinition
                    case mdefID of
                        Nothing    -> throw $ ArgumentException $ Just $ pack "`defID` field is missing"
                        Just defID -> case DefManager.lab defManager defID of 
                            Nothing         -> throw $ ArgumentException $ Just $ pack $ "Wrong `defID` in `definition`"
                            Just definition -> operation batchHandler (nodeID, node) (defID, definition)
            