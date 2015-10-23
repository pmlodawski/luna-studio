module Reactive.Commands.Selection where

import           Utils.PreludePlus
import           Control.Monad.State hiding (State)

import           Object.Object  (NodeId)
import           Object.Node    (Node)
import qualified Object.Node    as Node
import           Event.Keyboard (KeyMods(..))
import qualified JS.NodeGraph   as UI

import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Selection  as Selection
import qualified Reactive.State.Graph      as Graph

import           Reactive.Commands.Command          (Command, performIO)
import           Reactive.Commands.UIRegistry.Focus (focusOnNode, focusOnTopNode)

handleSelection :: Node -> KeyMods -> Command State ()
handleSelection node (KeyMods False False False False) = performSelect node
handleSelection node (KeyMods False False True  False) = toggleSelect  node
handleSelection _ _ = return ()

performSelect :: Node -> Command State ()
performSelect node = do
    let nodeId = node ^. Node.nodeId
    isSelected <- uses (Global.selection . Selection.nodeIds) (elem nodeId)
    when (not isSelected) $ Global.selection . Selection.nodeIds .= [nodeId]
    updateSelectionUI
    zoom Global.uiRegistry $ focusOnNode nodeId


toggleSelect :: Node -> Command State ()
toggleSelect node = do
    let nodeId = node ^. Node.nodeId
    isSelected <- uses (Global.selection . Selection.nodeIds) (elem nodeId)
    if isSelected
        then do
            Global.selection . Selection.nodeIds %= delete nodeId
            updateSelectionUI
            focusOnTopNode
        else do
            Global.selection . Selection.nodeIds %= (nodeId :)
            updateSelectionUI
            zoom Global.uiRegistry $ focusOnNode nodeId

updateSelectionUI :: Command State ()
updateSelectionUI = do
    allNodes      <- use $ Global.graph . Graph.nodes
    selectedNodes <- use $ Global.selection . Selection.nodeIds
    let allNodeIds      = (view Node.nodeId) <$> allNodes
        unselectedNodes = allNodeIds \\ selectedNodes
    performIO $ do
        UI.selectNodes     selectedNodes
        UI.unselectNodes unselectedNodes

selectAll :: Command State ()
selectAll = do
    allNodes <- use $ Global.graph . Graph.nodes
    Global.selection . Selection.nodeIds .= (view Node.nodeId <$> allNodes)
    updateSelectionUI

unselectAll :: Command State ()
unselectAll = do
    Global.selection . Selection.nodeIds .= []
    updateSelectionUI
