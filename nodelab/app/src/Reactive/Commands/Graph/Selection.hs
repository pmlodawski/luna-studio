module Reactive.Commands.Graph.Selection
     ( selectedNodes
     , focusSelectedNode
     , selectAll
     , unselectAll
     ) where

import           Utils.PreludePlus

import           Object.Widget                (WidgetFile (..), objectId, widget)
import qualified Object.Widget.Node           as NodeModel

import           Reactive.Commands.Command    (Command)
import           Reactive.Commands.Graph      (allNodes)
import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.UIRegistry    as UIRegistry
import           Reactive.State.Global        (State, inRegistry)


unselectAll :: Command State ()
unselectAll = do
    widgets <- allNodes
    inRegistry $ forM_ widgets $ \wf -> do
        let widgetId = wf ^. objectId
        when (wf ^. widget . NodeModel.isSelected) $ UICmd.update_ widgetId $ NodeModel.isSelected .~ False

selectAll :: Command State ()
selectAll = do
    widgets <- allNodes
    let widgetIds = (^. objectId) <$> widgets
    inRegistry $ forM_ widgetIds $ (flip UICmd.update) (NodeModel.isSelected .~ True)
    focusSelectedNode


selectedNodes :: Command State [WidgetFile NodeModel.Node]
selectedNodes = do
    widgets <- allNodes
    return $ filter (^. widget . NodeModel.isSelected) widgets

focusSelectedNode :: Command State ()
focusSelectedNode = do
    widgets <- selectedNodes
    inRegistry $ UIRegistry.focusedWidget .= (view objectId <$> widgets ^? ix 0)
