module Reactive.Commands.Selection where

import           Utils.PreludePlus
import           Control.Monad.State hiding (State)

import           Object.Object  (NodeId)
import           Object.Node    (Node)
import qualified Object.Widget.Node as Model
import           Object.UITypes (WidgetId)
import           Object.Widget  (objectId, widget)
import           Event.Keyboard (KeyMods(..))
import qualified JS.NodeGraph   as UI

import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.UIRegistry as UIRegistry
import qualified Reactive.State.Graph      as Graph

import           Reactive.Commands.Command          (Command, performIO)
import           Reactive.Commands.Graph            (allNodes)
import           Reactive.Commands.UIRegistry.Focus (focusOnNode, focusOnTopNode)
import qualified Reactive.Commands.UIRegistry  as UICmd
import           UI.Widget.Node (updateSelectedState)

handleSelection :: WidgetId -> KeyMods -> Command State ()
handleSelection id (KeyMods False False False False) = zoom Global.uiRegistry $ performSelect id
handleSelection id (KeyMods False False True  False) = zoom Global.uiRegistry $ toggleSelect  id
handleSelection _ _ = return ()

performSelect :: WidgetId -> Command (UIRegistry.State a) ()
performSelect id = do
    isSelected <- UICmd.get id Model.isSelected
    unless isSelected $ do
        unselectAll
        UICmd.update id (Model.isSelected .~ True)

toggleSelect :: WidgetId -> Command (UIRegistry.State a) ()
toggleSelect id = UICmd.update id (Model.isSelected %~ not)

selectAll :: Command (UIRegistry.State a) ()
selectAll = do
    widgets <- allNodes
    let widgetIds = (^. objectId) <$> widgets
    forM_ widgetIds $ (flip UICmd.update) (Model.isSelected .~ True)


unselectAll :: Command (UIRegistry.State a) ()
unselectAll = do
    widgets <- allNodes
    let widgetIds = (^. objectId) <$> widgets
    forM_ widgetIds $ (flip UICmd.update) (Model.isSelected .~ False)
