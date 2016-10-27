module Reactive.Commands.Function.Input
    (registerInput
    ) where

import           Prologue

import           Empire.API.Data.Input        (Input)
import qualified Object.Widget.FunctionPort   as Model
import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (State, inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.Graph         as Graph
import qualified Reactive.State.UIElements    as UIElements
import UI.Handlers.FunctionPort ()




registerInput :: Int -> Input -> Command State ()
registerInput inputNo input = do
    let inputModel = Model.fromInput input
    inputsEdgeId <- use $ Global.uiElements . UIElements.inputsEdge
    inputWidget <- inRegistry $ UICmd.register inputsEdgeId inputModel def
    Global.graph . Graph.inputWidgetsMap . at inputNo ?= inputWidget
