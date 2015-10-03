module Reactive.Plugins.Core.Action.Commands.DisconnectNodes where

import           Utils.PreludePlus
import           Reactive.Plugins.Core.Action.State.Global     (State)
import qualified Reactive.Plugins.Core.Action.State.Global     as Global
import qualified Reactive.Plugins.Core.Action.State.Graph      as Graph
import           Reactive.Plugins.Core.Action.Commands.Command (Command, performIO)
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry
import           Reactive.Plugins.Core.Action.Commands.Graph   (updateConnections, updatePortAngles, updateConnectionsUI)

import           Reactive.Plugins.Core.Action.Commands.UIRegistry.RemoveWidget (removeWidgets)

import           Object.Object           (ConnectionId)
import           Object.Widget.Helpers   (connectionIdToWidgetId)

import qualified BatchConnector.Commands as BatchCmd
import           Control.Monad.State     hiding (State)

localDisconnectAll :: [ConnectionId] -> Command State ()
localDisconnectAll connectionIds = do
    uiRegistry        <- use Global.uiRegistry
    let widgetIds     =  catMaybes $ connectionIdToWidgetId uiRegistry <$> connectionIds

    zoom Global.uiRegistry $ removeWidgets widgetIds
    Global.graph      %= Graph.removeConnections connectionIds
    modify $ updateConnections . updatePortAngles

    gets $ performIO . updateConnectionsUI

    state <- get
    performIO $ do
        updateConnectionsUI state

disconnectAll :: [ConnectionId] -> Command State ()
disconnectAll connectionIds = do
    graph <- use Global.graph
    workspace <- use Global.workspace
    let conns = catMaybes $ Graph.lookUpConnection graph <$> connectionIds
        refs  = Graph.connectionToRefs <$> conns
    performIO $ BatchCmd.disconnectNodes workspace refs
    localDisconnectAll connectionIds


