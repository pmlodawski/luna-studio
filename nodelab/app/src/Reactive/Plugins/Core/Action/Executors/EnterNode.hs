module Reactive.Plugins.Core.Action.Executors.EnterNode where

import Utils.PreludePlus
import Reactive.Plugins.Core.Action.State.Global            (State)
import Reactive.Plugins.Core.Action.Executors.UnrenderGraph (unrender)

import Object.Node (Node, isDef)

enterNode :: Node -> State -> (State, IO ())
enterNode node state = if   isDef node
                       then performEnter node state
                       else (state, print "would not enter")

performEnter :: Node -> State -> (State, IO ())
performEnter node state = (newState, action) where
    (newState, action) = unrender state
