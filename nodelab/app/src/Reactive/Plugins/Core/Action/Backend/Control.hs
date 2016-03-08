module Reactive.Plugins.Core.Action.Backend.Control where

import           Utils.PreludePlus

import qualified Event.Batch                      as Batch
import           Event.Event                      (Event (Init, Batch))

import           Reactive.Commands.Command        (Command, execCommand, performIO)
import           Reactive.State.Global            (State)
import qualified Reactive.State.Global            as Global
import qualified Empire.API.Update                as Update



toAction :: Event -> Maybe (Command State ())
toAction (Batch (Batch.EmpireStarted _)) = Just $ do
    error "Server crashed." -- could have done that more politely, but… let it crash
toAction _ = Nothing

