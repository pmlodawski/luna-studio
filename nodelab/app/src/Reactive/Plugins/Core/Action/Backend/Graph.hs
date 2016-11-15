module Reactive.Plugins.Core.Action.Backend.Graph
    ( toAction
    ) where


import           Utils.PreludePlus

import qualified Batch.Workspace                             as Workspace
import           Control.Monad.State                         (modify)

import qualified Empire.API.Data.Graph                       as Graph
import           Empire.API.Data.GraphLocation               (GraphLocation)
import qualified Empire.API.Graph.AddNode                    as AddNode
import qualified Empire.API.Graph.CodeUpdate                 as CodeUpdate
import qualified Empire.API.Graph.Connect                    as Connect
import qualified Empire.API.Graph.Disconnect                 as Disconnect
import qualified Empire.API.Graph.GetProgram                 as GetProgram
import qualified Empire.API.Graph.NodeResultUpdate           as NodeResultUpdate
import qualified Empire.API.Graph.NodeSearcherUpdate         as NodeSearcherUpdate
import qualified Empire.API.Graph.NodeUpdate                 as NodeUpdate
import qualified Empire.API.Graph.RemoveNode                 as RemoveNode
import qualified Empire.API.Graph.RenameNode                 as RenameNode
import qualified Empire.API.Graph.UpdateNodeMeta             as UpdateNodeMeta
import qualified Empire.API.Response                         as Response

import           Event.Batch                                 (Event (..))
import qualified Event.Event                                 as Event

import           Reactive.Commands.Batch                     (collaborativeModify, requestCollaborationRefresh)
import           Reactive.Commands.Camera                    (autoZoom)
import           Reactive.Commands.Command                   (Command, performIO)
import           Reactive.Commands.Graph                     (nodeIdToWidgetId, updateConnection)
import           Reactive.Commands.Graph.Connect             (localConnectNodes)
import           Reactive.Commands.Graph.Disconnect          (localDisconnectAll)
import           Reactive.Commands.Graph.Render              (renderGraph)
import           Reactive.Commands.Node                      (renameNode)
import           Reactive.Commands.Node.Create               (addDummyNode)
import           Reactive.Commands.Node.NodeMeta             (updateNodesMeta)
import           Reactive.Commands.Node.Remove               (localRemoveNodes)
import           Reactive.Commands.Node.Update               (updateNode, updateNodeProfilingData, updateNodeValue)
import           Reactive.Commands.UUID                      (isOwnRequest)
import           Reactive.Plugins.Core.Action.Backend.Common (doNothing, handleResponse)
import           Reactive.State.Global                       (State)
import qualified Reactive.State.Global                       as Global

import qualified JS.TextEditor                               as UI
import qualified UI.Handlers.Node                            as Node


isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)

isCurrentLocationAndGraphLoaded :: GraphLocation -> Command State Bool
isCurrentLocationAndGraphLoaded location = do
    icl <- isCurrentLocation location
    igl <- use $ Global.workspace . Workspace.isGraphLoaded
    return $ icl && igl

toAction :: Event.Event -> Maybe (Command State ())
toAction (Event.Batch ev) = Just $ case ev of
    ProgramFetched response -> handleResponse response $ \_ result -> do
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
                requestCollaborationRefresh

    AddNodeResponse response@(Response.Response uuid _ _) -> do
        shouldProcess <- isOwnRequest uuid
        handleResponse response $ \_ nodeId -> do
            collaborativeModify [nodeId]
            when shouldProcess $ do
              maybeWidgetId <- nodeIdToWidgetId $ nodeId
              case maybeWidgetId of Just widgetId -> Node.selectNode' Node.performSelect widgetId
                                    Nothing -> modify (& Global.nodeToSelect .~ Just nodeId)

    NodesConnected update -> do
        whenM (isCurrentLocation $ update ^. Connect.location') $ do
            connectionId <- localConnectNodes (update ^. Connect.src') (update ^. Connect.dst')
            updateConnection connectionId

    NodesDisconnected update -> do
        whenM (isCurrentLocation $ update ^. Disconnect.location') $ do
            localDisconnectAll $ [update ^. Disconnect.dst']

    NodeMetaUpdated update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. UpdateNodeMeta.location')
        when shouldProcess $ updateNodesMeta (update ^. UpdateNodeMeta.updates')

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

    -- CollaborationUpdate update -> -- handled in Collaboration.hs
    RemoveNodeResponse           response -> handleResponse response doNothing
    ConnectResponse              response -> handleResponse response doNothing
    DisconnectResponse           response -> handleResponse response doNothing
    NodeMetaResponse             response -> handleResponse response doNothing
    NodeRenameResponse           response -> handleResponse response doNothing
    UpdateNodeExpressionResponse response -> handleResponse response doNothing

    _ -> return ()
toAction _ = Nothing
