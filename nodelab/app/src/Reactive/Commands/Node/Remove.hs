module Reactive.Commands.Node.Remove
    ( removeSelectedNodes
    , localRemoveNodes
    ) where

import           Object.Widget                      (widget)
import           Reactive.Commands.Command          (Command)
import           Reactive.Commands.Graph            (nodeIdToWidgetId)
import           Reactive.Commands.Graph.Disconnect (localDisconnectAll)
import           Reactive.Commands.Graph.Selection  (selectedNodes)
import           Reactive.State.Global              (State, inRegistry)
import qualified Reactive.State.Global              as Global
import qualified Reactive.State.Graph               as Graph
import           Utils.PreludePlus

import           Reactive.Commands.UIRegistry       (removeWidget)

import           Empire.API.Data.Node               (NodeId)
import qualified Object.Widget.Node                 as NodeModel
import qualified Reactive.Commands.Batch            as BatchCmd

import qualified JS.GoogleAnalytics                 as GA


removeSelectedNodes :: Command State ()
removeSelectedNodes = do
    selectedNodes <- zoom Global.uiRegistry selectedNodes
    performRemoval $ (^. widget . NodeModel.nodeId) <$> selectedNodes

performRemoval :: [NodeId] -> Command State ()
performRemoval nodeIds = do
    workspace  <- use Global.workspace
    BatchCmd.removeNode nodeIds
    GA.sendEvent $ GA.RemoveNode (length nodeIds)

localRemoveNodes :: [NodeId] -> Command State ()
localRemoveNodes nodeIds = forM_ nodeIds $ \nodeId -> do
    danglingConns <- uses Global.graph (Graph.connectionIdsContainingNode $ nodeId)
    localDisconnectAll danglingConns

    Global.graph %= Graph.removeNode nodeId

    inRegistry $ do
        nodeWidgetId <- nodeIdToWidgetId nodeId
        withJust nodeWidgetId removeWidget

