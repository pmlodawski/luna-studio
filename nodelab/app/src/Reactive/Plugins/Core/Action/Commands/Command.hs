module Reactive.Plugins.Core.Action.Commands.Command where

import Utils.PreludePlus
import Control.Monad.State

type Command a = State a (IO ())

runCommand :: Command a -> a -> (IO (), a)
runCommand = runState
