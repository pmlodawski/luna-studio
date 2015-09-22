module Reactive.Plugins.Core.Action.Executors.EnterNode where

import Utils.PreludePlus
import Reactive.Plugins.Core.Action.State.Global (State)
import Object.Node
import Object.Widget (Position)

enterNode :: Node -> Position -> State -> (State, IO ())
enterNode node _ state = (state, print $ "would enter: " ++ (show node))
