module Reactive.Plugins.Core.Action.Executor where

import           Data.Monoid          ( (<>) )
import           Data.Default
import           Data.Maybe           ( isJust )
import           Data.Functor
import           Control.Lens
import           Utils.PrettyPrinter

import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.AddRemove as AddRemove
import qualified Reactive.Plugins.Core.Action.Selection as Selection
import qualified Reactive.Plugins.Core.Action.Drag      as Drag
import           Reactive.Plugins.Core.Action.State.Global



execAll :: State -> Maybe AddRemove.Action -> Maybe Selection.Action -> Maybe Drag.Action ->
          (State, WithStateMaybe AddRemove.Action State, WithStateMaybe Selection.Action State, WithStateMaybe Drag.Action State)
execAll stInit addRem selection drag = (stFinal, wsAddRem, wsSel, wsDrag)
    where
    wsAddRem = tryExec addRem    stInit
    wsSel    = tryExec selection stAddRem
    wsDrag   = tryExec drag      stSel
    stAddRem, stSel, stDrag :: State
    stAddRem = wsAddRem ^. state
    stSel    = wsSel    ^. state
    stDrag   = wsDrag   ^. state
    stFinal  = stDrag
