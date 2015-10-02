module Reactive.Plugins.Core.Action.Executors.UnrenderGraph where

import           Utils.PreludePlus
import           Reactive.Plugins.Core.Action.State.Global     (State)
import qualified Reactive.Plugins.Core.Action.State.Global     as Global
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry

import Object.Widget            (WidgetFile, objectId)
import Object.Widget.Node       (Node)
import Object.Widget.Connection (Connection)
import JS.Widget                (removeWidget)

unrender :: State -> (State, IO ())
unrender state = (newState, action) where
    newState                   = state & Global.graph .~ def
                                       & Global.uiRegistry .~ newRegistry
    uiRegistry                 = state ^. Global.uiRegistry
    nodeWidgets                = UIRegistry.lookupAll uiRegistry :: [WidgetFile State Node]
    connWidgets                = UIRegistry.lookupAll uiRegistry :: [WidgetFile State Connection]
    allWidgetIds               = (view objectId <$> nodeWidgets) ++ (view objectId <$> connWidgets)
    (newRegistry, idsToRemove) = UIRegistry.unregisterAll allWidgetIds uiRegistry
    action                     = sequence_ $ removeWidget <$> idsToRemove
