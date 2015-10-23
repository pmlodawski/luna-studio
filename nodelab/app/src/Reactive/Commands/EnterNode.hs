module Reactive.Commands.EnterNode where

import Utils.PreludePlus
import Reactive.State.Global           (State)
import Reactive.Commands.UnrenderGraph (unrender)
import Reactive.Commands.Command       (Command, performIO)

import Object.Node (Node, isDef)
import Object.UITypes (WidgetId)

enterNode :: Node -> WidgetId -> Command State ()
enterNode node _ = if   isDef node
                   then performEnter node
                   else performIO $ putStrLn "not entering"

performEnter :: Node -> Command State ()
performEnter node = unrender
