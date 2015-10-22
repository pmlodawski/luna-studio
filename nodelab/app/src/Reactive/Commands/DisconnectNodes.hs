module Reactive.Commands.DisconnectNodes where

import           Utils.PreludePlus
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Graph      as Graph
import           Reactive.Commands.Command (Command, performIO)
import qualified Reactive.State.UIRegistry as UIRegistry
import           Reactive.Commands.Graph   (updateConnNodes, updateConnections, updatePortAngles, updateConnectionsUI)

import           Reactive.Commands.UIRegistry.RemoveWidget (removeWidgets)

import           Object.Object           (ConnectionId, NodeId)
import           Object.Widget.Helpers   (connectionIdToWidgetId)

import qualified BatchConnector.Commands as BatchCmd
import           Control.Monad.State     hiding (State)


getChangedNodes :: Graph.State -> [ConnectionId] -> [NodeId]
getChangedNodes graph connIds = nIds1 ++ nIds2
    where (nIds1, nIds2) = connIds & map (flip Graph.getConnectionNodeIds graph)
                                   & catMaybes
                                   & unzip


localDisconnectAll :: [ConnectionId] -> Command State ()
localDisconnectAll connectionIds = do
    uiRegistry             <- use Global.uiRegistry
    graph                  <- use Global.graph
    let widgetIds          = catMaybes $ connectionIdToWidgetId uiRegistry <$> connectionIds
        changedNodes       = getChangedNodes graph connectionIds

    zoom Global.uiRegistry $ removeWidgets widgetIds
    Global.graph           %= Graph.removeConnections connectionIds
    updateConnNodes changedNodes
    updatePortAngles
    updateConnections
    updateConnectionsUI

disconnectAll :: [ConnectionId] -> Command State ()
disconnectAll connectionIds = do
    graph     <- use Global.graph
    workspace <- use Global.workspace
    let conns = catMaybes $ Graph.lookUpConnection graph <$> connectionIds
        refs  = Graph.connectionToRefs <$> conns
    performIO $ BatchCmd.disconnectNodes workspace refs
    localDisconnectAll connectionIds


