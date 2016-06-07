module Reactive.Plugins.Core.Action.Backend.Graph
    ( toAction
    ) where


import           Utils.PreludePlus

import qualified Batch.Workspace                             as Workspace
import qualified Data.Aeson                                  as JSON (ToJSON, encode)
import           Data.Text.Encoding                          (decodeUtf8)
import qualified Data.Text.Lazy                              as Text
import qualified Data.UUID.Types                             as UUID (toString)

import qualified Empire.API.Data.Graph                       as Graph
import           Empire.API.Data.GraphLocation               (GraphLocation)
import qualified Empire.API.Data.Node                        as Node
import           Empire.API.Data.PortRef                     (InPortRef (..), OutPortRef (..))
import qualified Empire.API.Data.PortRef                     as PortRef
import qualified Empire.API.Graph.AddNode                    as AddNode
import qualified Empire.API.Graph.Connect                    as Connect
import qualified Empire.API.Graph.Disconnect                 as Disconnect
import qualified Empire.API.Graph.CodeUpdate                 as CodeUpdate
import qualified Empire.API.Graph.GetProgram                 as GetProgram
import qualified Empire.API.Graph.NodeResultUpdate           as NodeResultUpdate
import qualified Empire.API.Graph.NodeSearcherUpdate         as NodeSearcherUpdate
import qualified Empire.API.Graph.NodeUpdate                 as NodeUpdate
import qualified Empire.API.Graph.RemoveNode                 as RemoveNode
import qualified Empire.API.Graph.RenameNode                 as RenameNode
import qualified Empire.API.Graph.UpdateNodeMeta             as UpdateNodeMeta
import qualified Empire.API.Response                         as Response
import qualified Empire.API.Topic                            as Topic

import           Event.Batch                                 (Event (..))
import qualified Event.Batch                                 as Batch
import qualified Event.Event                                 as Event

import           Reactive.Commands.Camera                    (autoZoom)
import           Reactive.Commands.Command                   (Command, performIO)
import           Reactive.Commands.Graph                     (localConnectNodes, updateConnections)
import           Reactive.Commands.Graph.Disconnect          (localDisconnectAll)
import           Reactive.Commands.Node                      (renameNode)
import           Reactive.Commands.Node.Create               (addDummyNode, addNode)
import           Reactive.Commands.Node.NodeMeta             (updateNodeMeta)
import           Reactive.Commands.Node.Remove               (localRemoveNodes)
import           Reactive.Commands.Node.Update               (updateNode, updateNodeProfilingData, updateNodeValue)
import           Reactive.Commands.RenderGraph               (renderGraph)
import           Reactive.Commands.UUID                      (isOwnRequest, unregisterRequest)
import           Reactive.Plugins.Core.Action.Backend.Common (doNothing, handleResponse)
import           Reactive.State.Global                       (State)
import qualified Reactive.State.Global                       as Global

import qualified JS.TextEditor                               as UI


isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)

isCurrentLocationAndGraphLoaded :: GraphLocation -> Command State Bool
isCurrentLocationAndGraphLoaded location = do
    icl <- isCurrentLocation location
    igl <- use $ Global.workspace . Workspace.isGraphLoaded
    return $ icl && igl

toAction :: Event.Event -> Maybe (Command State ())
toAction (Event.Batch ev) = Just $ case ev of
    ProgramFetched response -> handleResponse response $ \req result -> do
            let location = response ^. Response.request . GetProgram.location
            isGraphLoaded  <- use $ Global.workspace . Workspace.isGraphLoaded
            isGoodLocation <- isCurrentLocation location
            when (isGoodLocation && not isGraphLoaded) $ do
                let nodes       = result ^. GetProgram.graph . Graph.nodes
                    connections = result ^. GetProgram.graph . Graph.connections
                    code        = result ^. GetProgram.code
                    nsData      = result ^. GetProgram.nodeSearcherData

                Global.workspace . Workspace.nodeSearcherData .= nsData
                renderGraph nodes connections
                autoZoom
                performIO $ UI.setText code
                Global.workspace . Workspace.isGraphLoaded .= True

    NodesConnected update -> do
        whenM (isCurrentLocation $ update ^. Connect.location') $ do
            localConnectNodes (update ^. Connect.src') (update ^. Connect.dst')
            updateConnections

    NodesDisconnected update -> do
        whenM (isCurrentLocation $ update ^. Disconnect.location') $ do
            localDisconnectAll $ [update ^. Disconnect.dst']

    NodeMetaUpdated update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. UpdateNodeMeta.location')
        when shouldProcess $ updateNodeMeta (update ^. UpdateNodeMeta.nodeId') (update ^. UpdateNodeMeta.nodeMeta')

    NodeAdded update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. AddNode.location')
        when shouldProcess $ addDummyNode (update ^. AddNode.node')

    NodeUpdated update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. NodeUpdate.location)
        when shouldProcess $ updateNode $ update ^. NodeUpdate.node

    NodeRenamed update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. RenameNode.location')
        when shouldProcess $ renameNode (update ^. RenameNode.nodeId') (update ^. RenameNode.name')

    NodeRemoved update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. RemoveNode.location')
        when shouldProcess $ localRemoveNodes $ update ^. RemoveNode.nodeIds'

    NodeResultUpdated update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. NodeResultUpdate.location)
        when shouldProcess $ do
            updateNodeValue         (update ^. NodeResultUpdate.nodeId) (update ^. NodeResultUpdate.value)
            updateNodeProfilingData (update ^. NodeResultUpdate.nodeId) (update ^. NodeResultUpdate.execTime)

    NodeSearcherUpdated update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. NodeSearcherUpdate.location)
        when shouldProcess $ Global.workspace . Workspace.nodeSearcherData .= update ^. NodeSearcherUpdate.nodeSearcherData
    CodeUpdated update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. CodeUpdate.location)
        when shouldProcess $ performIO $ UI.setText $ update ^. CodeUpdate.code

    RemoveNodeResponse response -> handleResponse response doNothing
    AddNodeResponse    response -> handleResponse response doNothing
    ConnectResponse    response -> handleResponse response doNothing
    DisconnectResponse response -> handleResponse response doNothing
    NodeMetaResponse   response -> handleResponse response doNothing
    NodeRenameResponse response -> handleResponse response doNothing


    _ -> return ()

toAction _ = Nothing
