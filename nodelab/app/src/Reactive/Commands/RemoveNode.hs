module Reactive.Commands.RemoveNode where

import           Utils.PreludePlus
import           Event.Keyboard (KeyMods)
import           Object.Object  (NodeId)
import           Object.UITypes (WidgetId)
import           Object.Widget  (widget)
import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.UIRegistry         as UIRegistry
import qualified Reactive.State.Graph              as Graph
import           Reactive.Commands.Command         (Command, performIO)
import           Reactive.Commands.Selection       (selectedNodes)
import           Reactive.Commands.Graph           (nodeIdToWidgetId)
import           Reactive.Commands.DisconnectNodes (localDisconnectAll)

import           Reactive.Commands.UIRegistry (removeWidget)

import qualified BatchConnector.Commands as BatchCmd
import qualified JS.NodeGraph            as UIGraph
import           Object.Node             (Node, nodeId)
import qualified Object.Widget.Node      as NodeModel


removeSelectedNodes :: Command State ()
removeSelectedNodes = do
    selectedNodes <- zoom Global.uiRegistry selectedNodes
    mapM_ performRemoval $ (^. widget . NodeModel.nodeId) <$> selectedNodes

performRemoval :: NodeId -> Command State ()
performRemoval nodeId = do
    danglingConns <- uses Global.graph (Graph.connectionIdsContainingNode $ nodeId)
    localDisconnectAll danglingConns

    Global.graph %= Graph.removeNode (nodeId)

    uiRegistry <- use Global.uiRegistry
    workspace  <- use Global.workspace

    nodeWidgetId <- zoom Global.uiRegistry $ nodeIdToWidgetId nodeId
    zoom Global.uiRegistry $ mapM_ removeWidget $ maybeToList nodeWidgetId

    performIO $ BatchCmd.removeNodeById workspace nodeId
