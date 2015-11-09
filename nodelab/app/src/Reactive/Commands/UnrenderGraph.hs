module Reactive.Commands.UnrenderGraph where

import           Utils.PreludePlus
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.UIRegistry as UIRegistry
import           Reactive.Commands.Command (Command, performIO)

import           Reactive.Commands.UIRegistry (removeWidget)

import Object.Widget            (WidgetFile, objectId)
import Object.Widget.Node       (Node)
import Object.Widget.Connection (Connection)
import UI.Widget.Node ()

unrender :: Command State ()
unrender = do
    Global.graph .= def
    uiRegistry <- use Global.uiRegistry
    let nodeWidgets  = UIRegistry.lookupAll uiRegistry :: [WidgetFile Node]
        connWidgets  = UIRegistry.lookupAll uiRegistry :: [WidgetFile Connection]
        allWidgetIds = (view objectId <$> nodeWidgets) ++ (view objectId <$> connWidgets)

    zoom Global.uiRegistry $ mapM_ removeWidget allWidgetIds
