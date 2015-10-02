module Reactive.Plugins.Core.Action.Commands.UnrenderGraph where

import           Utils.PreludePlus
import           Reactive.Plugins.Core.Action.State.Global     (State)
import qualified Reactive.Plugins.Core.Action.State.Global     as Global
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry
import           Reactive.Plugins.Core.Action.Commands.Command (Command)

import Object.Widget            (WidgetFile, objectId)
import Object.Widget.Node       (Node)
import Object.Widget.Connection (Connection)
import JS.Widget                (removeWidget)

unrender :: Command State
unrender = do
    Global.graph .= def
    uiRegistry <- use Global.uiRegistry
    let nodeWidgets  = UIRegistry.lookupAll uiRegistry :: [WidgetFile State Node]
        connWidgets  = UIRegistry.lookupAll uiRegistry :: [WidgetFile State Connection]
        allWidgetIds = (view objectId <$> nodeWidgets) ++ (view objectId <$> connWidgets)
    idsToRemove <- zoom Global.uiRegistry $ UIRegistry.unregisterAllM allWidgetIds
    return $ sequence_ $ removeWidget <$> idsToRemove
