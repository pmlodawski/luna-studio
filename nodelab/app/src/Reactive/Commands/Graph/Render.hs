module Reactive.Commands.Graph.Render
    ( renderGraph
    ) where

import           Utils.PreludePlus

import           Control.Monad                     (msum)
import qualified Data.HashMap.Lazy                 as HashMap
import qualified Data.IntMap.Lazy                  as IntMap

import           Empire.API.Data.Input             (Input)
import           Empire.API.Data.Node              (Node)
import qualified Empire.API.Data.Node              as Node
import           Empire.API.Data.Output            (Output)
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

splitNodesAndEdges :: [Node] -> ([Input], Maybe Output, [Node])
splitNodesAndEdges allNodes = (inputEdge, outputEdge, nodes) where
    inputEdge  = fromMaybe def $ msum mayInputEdges
    outputEdge = msum mayOutputEdges
    (mayInputEdges, mayOutputEdges, nodes) = splitNodesAndEdges' allNodes (def, def, def)
    splitNodesAndEdges' [] r = r
    splitNodesAndEdges' (node:t) (i, o, n) = splitNodesAndEdges' t $ case node ^. Node.nodeType of
        Node.InputEdge inputs  -> (Just inputs : i, o, n)
        Node.OutputEdge output -> (i, Just output : o, n)
        _ -> (i, o, node:n)

renderGraph :: [Node] -> [(OutPortRef, InPortRef)] -> Command State ()
renderGraph allNodes connections = do
    let (inputs, outputs, nodes) = splitNodesAndEdges allNodes
    fastAddNodes nodes
    mapM_ (uncurry localConnectNodes) connections
    addInputs inputs
    withJust outputs addOutputs
    updateConnections
    updateNodeZOrder

addInputs  :: [Input] -> Command State ()
addInputs inputs = do
    let numberedInputs = [0..] `zip` inputs
    Global.graph . Graph.inputsMap .= IntMap.fromList numberedInputs
    mapM_ (uncurry Input.registerInput) numberedInputs

addOutputs :: Output -> Command State ()
addOutputs outputs = do
    Global.graph . Graph.outputs ?= outputs
    Output.registerOutput outputs
