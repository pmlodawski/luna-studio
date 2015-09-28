module Reactive.Plugins.Core.Action.Executors.EnterNode where

import Utils.PreludePlus
import Reactive.Plugins.Core.Action.State.Global (State)
import Object.Node

enterNode :: Node -> State -> (State, IO ())
enterNode node state = (state, print $ "would enter: " ++ (show node))
