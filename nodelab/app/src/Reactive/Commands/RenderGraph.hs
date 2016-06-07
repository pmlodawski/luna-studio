module Reactive.Commands.RenderGraph
    ( renderGraph
    ) where

import qualified Batch.Workspace               as Workspace

import           Empire.API.Data.Node          (Node)
import           Empire.API.Data.PortRef       (InPortRef, OutPortRef)

import           Reactive.Commands.Command     (Command)
import qualified Reactive.Commands.Graph       as Graph
import           Reactive.Commands.Node.Create (addNode)
import           Reactive.State.Global         (State)
import qualified Reactive.State.Global         as Global
import           Utils.PreludePlus

renderGraph :: [Node] -> [(OutPortRef, InPortRef)] -> Command State ()
renderGraph nodes edges = do
    mapM_ addNode nodes
    mapM_ (uncurry Graph.localConnectNodes) edges
    Graph.updateConnections
