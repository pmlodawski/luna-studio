module Reactive.Plugins.Core.Action.Backend.Graph where

import           Utils.PreludePlus

import qualified Batch.Workspace                   as Workspace
import           Event.Batch                       (Event (..))
import qualified Event.Batch                       as Batch
import qualified Event.Event                       as Event
import           Reactive.Commands.AddNode         (addNode, updateNode, updateNodeValue)
import           Reactive.Commands.Camera          (autoZoom)
import           Reactive.Commands.Command         (Command, performIO)
import           Reactive.Commands.DisconnectNodes (disconnect)
import           Reactive.Commands.Graph           (localConnectNodes, renameNode, updateNodeMeta)
import qualified Reactive.Commands.RemoveNode      as RemoveNode
import           Reactive.Commands.RenderGraph     (renderGraph)
import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global

import qualified Empire.API.Data.Graph             as Graph
import           Empire.API.Data.GraphLocation     (GraphLocation)
import qualified Empire.API.Data.Node              as Node
import           Empire.API.Data.PortRef           (InPortRef (..), OutPortRef (..))
import qualified Empire.API.Data.PortRef           as PortRef
import qualified Empire.API.Graph.AddNode          as AddNode
import qualified Empire.API.Graph.Connect          as Connect
import qualified Empire.API.Graph.Disconnect       as Disconnect
import qualified Empire.API.Graph.GetProgram       as GetProgram
import qualified Empire.API.Graph.NodeResultUpdate as NodeResultUpdate
import qualified Empire.API.Graph.NodeUpdate       as NodeUpdate
import qualified Empire.API.Graph.RemoveNode       as RemoveNode
import qualified Empire.API.Graph.RenameNode       as RenameNode
import qualified Empire.API.Graph.UpdateNodeMeta   as UpdateNodeMeta
import qualified Empire.API.Update                 as Update

import qualified JS.TextEditor                     as UI

isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)

toAction :: Event.Event -> Maybe (Command State ())
toAction (Event.Batch ev) = Just $ case ev of
    ProgramFetched response -> do
        let location = response ^. Update.request . GetProgram.location
        isGraphLoaded  <- use $ Global.workspace . Workspace.isGraphLoaded
        isGoodLocation <- isCurrentLocation location
        when (isGoodLocation && not isGraphLoaded) $ do
            let nodes       = response ^. Update.result . GetProgram.graph . Graph.nodes
                connections = response ^. Update.result . GetProgram.graph . Graph.connections
                code        = response ^. Update.result . GetProgram.code

            renderGraph nodes connections
            autoZoom
            performIO $ UI.setText code
            Global.workspace . Workspace.isGraphLoaded .= True

    NodeAdded      response -> addNode $ response ^. Update.result . AddNode.node

    NodesConnected response -> processConnection (response ^. Update.request)  where
        processConnection request = localConnectNodes (request ^. Connect.src) (request ^. Connect.dst)

    NodesDisconnected response -> disconnect (response ^. Update.request . Disconnect.dst) where

    NodeMetaUpdated response -> do
        shouldProcess <- isCurrentLocation (response ^. Update.request . UpdateNodeMeta.location)
        when shouldProcess $ updateNodeMeta (response ^. Update.request . UpdateNodeMeta.nodeId) (response ^. Update.result ^. UpdateNodeMeta.newNodeMeta)

    NodeUpdated response -> do
        shouldProcess <- isCurrentLocation (response ^. NodeUpdate.location)
        when shouldProcess $ updateNode $ response ^. NodeUpdate.node

    NodeRenamed response -> do
        shouldProcess <- isCurrentLocation (response ^. Update.request . RenameNode.location)
        when shouldProcess $ renameNode (response ^. Update.request . RenameNode.nodeId) (response ^. Update.request . RenameNode.name)

    NodeRemoved response -> do
        shouldProcess <- isCurrentLocation (response ^. Update.request . RemoveNode.location)
        when shouldProcess $ RemoveNode.localRemoveNodes $ response ^. Update.request . RemoveNode.nodeId

    NodeResultUpdated response -> do
        shouldProcess <- isCurrentLocation (response ^. NodeResultUpdate.location)
        when shouldProcess $ updateNodeValue (response ^. NodeResultUpdate.nodeId) (response ^. NodeResultUpdate.value)
    _ -> return ()

toAction _ = Nothing
