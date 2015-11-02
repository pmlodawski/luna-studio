module Reactive.Plugins.Core.Action.Backend.AddNode where

import           Utils.PreludePlus
import           Object.Node        (Node)
import           Event.Event        (Event(Batch))
import qualified Event.Batch        as Batch

import           Reactive.State.Global      (State)
import           Reactive.Commands.AddNode  (addNode)
import           Reactive.Commands.Command  (Command)

toAction :: Event -> Maybe (Command State ())
toAction (Batch (Batch.NodeAdded node)) = Just $ addNode node
toAction _                              = Nothing
