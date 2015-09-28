module Reactive.Plugins.Core.Action.Backend.Runner where

import           Utils.PreludePlus
import           Object.Node
import           Event.Event
import qualified Event.Batch             as Batch
import           BatchConnector.Commands as BatchCmd
import           Batch.Workspace
import           Batch.Value
import           JS.NodeGraph            (setComputedValue)

import           Reactive.Plugins.Core.Action
import           Reactive.Plugins.Core.Action.State.Global as Global

data Action = RequestRerun
            | UpdateValue Int Value
            deriving (Show, Eq)

instance PrettyPrinter Action where
    display = show

newtype Reaction = PerformIO (IO ())

instance PrettyPrinter Reaction where
    display _ = "runner(PerfomIO)"

toAction :: Event Node -> Maybe Action
toAction (Batch (Batch.NodeAdded _))              = Just RequestRerun
toAction (Batch  Batch.NodesConnected)            = Just RequestRerun
toAction (Batch  Batch.NodeRemoved)               = Just RequestRerun
toAction (Batch (Batch.ValueUpdate nodeId value)) = Just $ UpdateValue nodeId value
toAction _                                        = Nothing

instance ActionStateUpdater Action where
    execSt RequestRerun state               = ActionUI (PerformIO BatchCmd.runMain) state
    execSt (UpdateValue nodeId value) state = ActionUI (PerformIO $ (setComputedValue nodeId $ show value)) state

instance ActionUIUpdater Reaction where
    updateUI (WithState (PerformIO act) st) = act
