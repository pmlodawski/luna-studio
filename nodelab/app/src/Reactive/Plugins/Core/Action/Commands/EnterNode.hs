module Reactive.Plugins.Core.Action.Commands.EnterNode where

import Utils.PreludePlus
import Reactive.Plugins.Core.Action.State.Global           (State)
import Reactive.Plugins.Core.Action.Commands.UnrenderGraph (unrender)
import Reactive.Plugins.Core.Action.Commands.Command       (Command, performIO)

import Object.Node (Node, isDef)

enterNode :: Node -> Command State ()
enterNode node = if   isDef node
                 then performEnter node
                 else performIO $ putStrLn "not entering"

performEnter :: Node -> Command State ()
performEnter node = unrender
