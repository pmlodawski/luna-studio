module Reactive.Plugins.Core.Action.Commands.RenderGraph where

import           Utils.PreludePlus
import           Reactive.Plugins.Core.Action.State.Global        (State)
import qualified Reactive.Plugins.Core.Action.State.Global        as Global
import qualified Reactive.Plugins.Core.Action.Commands.Graph      as Graph
import           Reactive.Plugins.Core.Action.Commands.AddNode    (addNode)
import           Reactive.Plugins.Core.Action.Commands.Command    (Command)
import           Reactive.Plugins.Core.Action.Commands.AutoLayout (layoutGraph)

import           Object.Node     (Node, PortRef)
import qualified Batch.Workspace as Workspace

renderGraph :: [Node] -> [(PortRef, PortRef)] -> Command State ()
renderGraph nodes edges = do
    mapM_ addNode nodes
    mapM_ (uncurry Graph.localConnectNodes) edges

    reLayout <- use $ Global.workspace . Workspace.shouldLayout
    Global.workspace . Workspace.shouldLayout .= False
    when reLayout layoutGraph

    Graph.updatePortAngles
    Graph.updateConnections
    Graph.updatePortAnglesUI
    Graph.updateConnectionsUI
