module Reactive.Plugins.Core.Action.Commands.RenderGraph where

import Utils.PreludePlus

import Reactive.Plugins.Core.Action.Commands.Command (Command, performIO)
import Reactive.Plugins.Core.Action.Commands.AddNode (addNode)

import qualified Reactive.Plugins.Core.Action.Commands.Graph as Graph
import Reactive.Plugins.Core.Action.State.Global (State)
import Object.Node (Node, PortRef)

renderGraph :: [Node] -> [(PortRef, PortRef)] -> Command State ()
renderGraph nodes edges = do
    mapM_ addNode nodes
    mapM_ (uncurry Graph.connectNodes) edges
    Graph.updatePortAngles
    Graph.updateConnections
    Graph.updatePortAnglesUI
    Graph.updateConnectionsUI
