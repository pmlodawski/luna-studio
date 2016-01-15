module Reactive.Plugins.Core.Action.Backend.AddNode where

import           Utils.PreludePlus
import           Event.Event        (Event(Batch))
import qualified Event.Batch        as Batch

import           Reactive.State.Global      (State)
import qualified Reactive.State.Global      as Global
import qualified Batch.Workspace            as Workspace
import           Reactive.Commands.AddNode  (addNode, updateNode, updateNodeValue)
import           Reactive.Commands.Graph    (localConnectNodes, updateNodeMeta)
import           Reactive.Commands.DisconnectNodes (disconnect)
import           Reactive.Commands.Command  (Command)
import qualified Reactive.Commands.RemoveNode as RemoveNode
import qualified Empire.API.Update as Update
import qualified Empire.API.Graph.UpdateNodeMeta as UpdateNodeMeta
import qualified Empire.API.Graph.AddNode as AddNode
import qualified Empire.API.Graph.RemoveNode as RemoveNode
import qualified Empire.API.Graph.Connect as Connect
import qualified Empire.API.Graph.Disconnect as Disconnect
import qualified Empire.API.Graph.NodeUpdate as NodeUpdate
import qualified Empire.API.Graph.NodeResultUpdate as NodeResultUpdate
import qualified Empire.API.Data.Node as Node
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.PortRef (InPortRef(..), OutPortRef(..))
import qualified Empire.API.Data.PortRef as PortRef

isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)

toAction :: Event -> Maybe (Command State ())
toAction (Batch (Batch.NodeAdded      response)) = Just $ addNode $ response ^. Update.result . AddNode.node
toAction (Batch (Batch.NodesConnected response)) = Just $ processConnection (response ^. Update.request)  where
    processConnection request = localConnectNodes (request ^. Connect.src) (request ^. Connect.dst)
toAction (Batch (Batch.NodesDisconnected response)) = Just $ disconnect (response ^. Update.request . Disconnect.dst) where
toAction (Batch (Batch.NodeMetaUpdated response)) = Just $ do
    shouldProcess <- isCurrentLocation (response ^. Update.request . UpdateNodeMeta.location)
    when shouldProcess $ updateNodeMeta (response ^. Update.request . UpdateNodeMeta.nodeId) (response ^. Update.result ^. UpdateNodeMeta.newNodeMeta)
toAction (Batch (Batch.NodeUpdated response)) = Just $ do
    shouldProcess <- isCurrentLocation (response ^. NodeUpdate.location)
    when shouldProcess $ updateNode $ response ^. NodeUpdate.node
toAction (Batch (Batch.NodeRemoved response)) = Just $ do
    shouldProcess <- isCurrentLocation (response ^. Update.request . RemoveNode.location)
    when shouldProcess $ RemoveNode.localRemoveNodes $ response ^. Update.request . RemoveNode.nodeId
toAction (Batch (Batch.NodeResultUpdated response)) = Just $ do
    shouldProcess <- isCurrentLocation (response ^. NodeResultUpdate.location)
    when shouldProcess $ updateNodeValue (response ^. NodeResultUpdate.nodeId) (response ^. NodeResultUpdate.value)

toAction _                              = Nothing
