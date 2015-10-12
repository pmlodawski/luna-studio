module Reactive.Plugins.Core.Action.Commands.EnterNode where

import Utils.PreludePlus
import Reactive.Plugins.Core.Action.State.Global           (State)
import Reactive.Plugins.Core.Action.Commands.UnrenderGraph (unrender)
import Reactive.Plugins.Core.Action.Commands.Command       (Command, performIO)

import Object.Node (Node, isDef)
import Object.UITypes (WidgetId)

enterNode :: Node -> WidgetId -> Command State ()
enterNode node _ = if   isDef node
                   then performEnter node
                   else performIO $ putStrLn "not entering"

performEnter :: Node -> Command State ()
performEnter node = unrender
