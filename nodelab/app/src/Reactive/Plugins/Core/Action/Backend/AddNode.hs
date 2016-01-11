module Reactive.Plugins.Core.Action.Backend.AddNode where

import           Utils.PreludePlus
import           Event.Event        (Event(Batch))
import qualified Event.Batch        as Batch

import           Reactive.State.Global      (State)
import qualified Reactive.State.Global      as Global
import qualified Batch.Workspace            as Workspace
import           Reactive.Commands.AddNode  (addNode)
import           Reactive.Commands.Graph    (localConnectNodes, updateNodeMeta)
import           Reactive.Commands.Command  (Command)
import qualified Empire.API.Response as Response
import qualified Empire.API.Graph.UpdateNodeMeta as UpdateNodeMeta
import qualified Empire.API.Graph.AddNode as AddNode
import qualified Empire.API.Graph.Connect as Connect
import qualified Empire.API.Data.Node as Node
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.PortRef (InPortRef(..), OutPortRef(..))
import qualified Empire.API.Data.PortRef as PortRef

isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)

toAction :: Event -> Maybe (Command State ())
toAction (Batch (Batch.NodeAdded      response)) = Just $ addNode $ response ^. Response.update . AddNode.node
toAction (Batch (Batch.NodesConnected response)) = Just $ processConnection (response ^. Response.request)  where
    processConnection request = localConnectNodes (request ^. Connect.src) (request ^. Connect.dst)
toAction (Batch (Batch.NodeMetaUpdated response)) = Just $ do
    shouldProcess <- isCurrentLocation (response ^. Response.request . UpdateNodeMeta.location)
    when shouldProcess $ updateNodeMeta (response ^. Response.request . UpdateNodeMeta.nodeId) (response ^. Response.update ^. UpdateNodeMeta.newNodeMeta)

toAction _                              = Nothing
