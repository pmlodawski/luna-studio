module Reactive.Plugins.Core.Action.Backend where

import           Utils.PreludePlus
import           Object.Node
import           Event.Event
import qualified Event.Batch        as Batch
import           Batch.Workspace
import           Batch.Value

import           BatchConnector.Commands
import           BatchConnector.Connection
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.State.Global as Global
import           JS.NodeGraph (setComputedValue)

data Action = InsertSerializationMode Node
            | SetComputedValue Int Value
            deriving (Show, Eq)

data Reaction = PerformIO (IO ())

instance PrettyPrinter Reaction where
    display _ = "backend(Reaction)"

instance PrettyPrinter Action where
    display _ = "backend(InsertSerializationMode)"

toAction :: Event Node -> Maybe Action
toAction (Batch (Batch.NodeAdded node)) = Just $ InsertSerializationMode node
toAction (Batch (Batch.ValueUpdate nodeId value)) = Just $ SetComputedValue nodeId value
toAction _ = Nothing

instance ActionStateUpdater Action where
    execSt (InsertSerializationMode node) state = ActionUI (PerformIO action) state where
        action = sendMessage $ insertSerializationMode workspace node
        workspace = state ^. Global.workspace
    execSt (SetComputedValue nodeId value) state = ActionUI (PerformIO action) state where
        action = setComputedValue nodeId (show value)

instance ActionUIUpdater Reaction where
    updateUI (WithState (PerformIO act) st) = act
