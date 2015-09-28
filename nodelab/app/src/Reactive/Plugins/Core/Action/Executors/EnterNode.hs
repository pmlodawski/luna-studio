module Reactive.Plugins.Core.Action.Executors.EnterNode where

import Utils.PreludePlus
import Reactive.Plugins.Core.Action.State.Global (State)
import Object.Node

enterNode :: Node -> State -> (State, IO ())
enterNode node state = if   isDef node
                       then (state, print $ "would enter: " ++ (show node))
                       else (state, print "would not enter")
