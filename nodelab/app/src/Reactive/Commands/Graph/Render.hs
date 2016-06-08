module Reactive.Commands.Graph.Render
    ( renderGraph
    ) where

import           Empire.API.Data.Node            (Node)
import           Empire.API.Data.PortRef         (InPortRef, OutPortRef)

import           Reactive.Commands.Command       (Command)
import           Reactive.Commands.Graph.Connect (localConnectNodes)
import           Reactive.Commands.Graph         (updateConnections)
import           Reactive.Commands.Node.Create   (addNode)
import           Reactive.State.Global           (State)
import           Utils.PreludePlus

renderGraph :: [Node] -> [(OutPortRef, InPortRef)] -> Command State ()
renderGraph nodes edges = do
    mapM_ addNode nodes
    mapM_ (uncurry localConnectNodes) edges
    updateConnections
