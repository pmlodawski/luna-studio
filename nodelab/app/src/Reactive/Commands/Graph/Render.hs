module Reactive.Commands.Graph.Render
    ( renderGraph
    ) where

import           Utils.PreludePlus

import           Control.Monad                     (msum)
import qualified Data.HashMap.Lazy                 as HashMap
import qualified Data.IntMap.Lazy                  as IntMap

import           Empire.API.Data.Node              (Node)
import qualified Empire.API.Data.Node              as Node
import           Empire.API.Data.PortRef           (InPortRef, OutPortRef)

import           Reactive.Commands.Command         (Command)
import qualified Reactive.Commands.Function.Input  as Input
import qualified Reactive.Commands.Function.Output as Output
import           Reactive.Commands.Graph           (updateConnections, updateNodeZOrder)
import           Reactive.Commands.Graph.Connect   (localConnectNodes)
import           Reactive.Commands.Node.Create     (registerNode)
import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph



fastAddNodes :: [Node] -> Command State ()
fastAddNodes nodes = do
    let nodeIds = (view Node.nodeId) <$> nodes
    Global.graph . Graph.nodesMap .= (HashMap.fromList $ nodeIds `zip` nodes)
    mapM_ registerNode nodes

splitNodesAndEdges :: [Node] -> (Maybe Node, Maybe Node, [Node])
splitNodesAndEdges allNodes = (inputEdge, outputEdge, nodes) where
    inputEdge  = msum mayInputEdges
    outputEdge = msum mayOutputEdges
    (mayInputEdges, mayOutputEdges, nodes) = splitNodesAndEdges' allNodes (def, def, def)
    splitNodesAndEdges' [] r = r
    splitNodesAndEdges' (node:t) (i, o, n) = splitNodesAndEdges' t $ case node ^. Node.nodeType of
        Node.InputEdge  {} -> (Just node : i, o, n)
        Node.OutputEdge {} -> (i, Just node : o, n)
        _ -> (i, o, node:n)

renderGraph :: [Node] -> [(OutPortRef, InPortRef)] -> Command State ()
renderGraph allNodes connections = do
    let (inputs, outputs, nodes) = splitNodesAndEdges allNodes
    fastAddNodes nodes
    mapM_ (uncurry localConnectNodes) connections
    withJust inputs  addInputs
    withJust outputs addOutputs
    updateConnections
    updateNodeZOrder

addInputs  :: Node -> Command State ()
addInputs node = do
    let inputs = node ^. Node.nodeType . Node.inputs
        numberedInputs = [0..] `zip` inputs
        nodeId = node ^. Node.nodeId
    Global.graph . Graph.inputsMap .= IntMap.fromList numberedInputs
    Global.graph . Graph.inputsId ?= nodeId
    mapM_ (uncurry $ Input.registerInput nodeId) numberedInputs

addOutputs :: Node -> Command State ()
addOutputs node = do
    let outputs = node ^?! Node.nodeType . Node.output
        nodeId  = node ^. Node.nodeId
    Global.graph . Graph.outputs ?= outputs
    Global.graph . Graph.outputsId ?= nodeId
    Output.registerOutput nodeId outputs
