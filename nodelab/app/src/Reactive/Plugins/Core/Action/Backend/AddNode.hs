module Reactive.Plugins.Core.Action.Backend.AddNode where

import           Utils.PreludePlus
import           Object.Node        (Node)
import           Event.Event        (Event(Batch))
import qualified Event.Batch        as Batch

import           Reactive.Plugins.Core.Action.State.Global      (State)
import           Reactive.Plugins.Core.Action.Commands.AddNode  (addNode)
import           Reactive.Plugins.Core.Action.Commands.Command  (Command)

toAction :: Event Node -> State -> Maybe (Command State ())
toAction (Batch (Batch.NodeAdded node)) state = Just $ addNode node
toAction _ _ = Nothing
