module Reactive.Commands.RenderGraph where

import           Utils.PreludePlus
import           Reactive.State.Global        (State)
import qualified Reactive.State.Global        as Global
import qualified Reactive.Commands.Graph      as Graph
import           Reactive.Commands.AddNode    (addNode)
import           Reactive.Commands.Command    (Command)
import           Reactive.Commands.AutoLayout (layoutGraph)

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
