module Reactive.Commands.Function.Output
    (registerOutput
    ) where

import           Prologue

import           Empire.API.Data.Output       (Output)
import qualified Object.Widget.FunctionPort   as Model
import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (State, inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.Graph         as Graph
import qualified Reactive.State.UIElements    as UIElements
import           UI.Handlers.FunctionPort     ()




registerOutput :: Output -> Command State ()
registerOutput output = do
    let outputModel = Model.fromOutput output
    outputsEdgeId <- use $ Global.uiElements . UIElements.outputsEdge
    outputWidget <- inRegistry $ UICmd.register outputsEdgeId outputModel def
    Global.graph . Graph.outputWidget ?= outputWidget
