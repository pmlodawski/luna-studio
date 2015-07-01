module Reactive.Plugins.Core.State where


import qualified Reactive.Plugins.Core.Action.Selection as Selection
import qualified Reactive.Plugins.Core.Action.Drag      as Drag


data State = State Selection.State Drag.State
