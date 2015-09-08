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
import           JS.NodeGraph                              (setComputedValue)
import           Data.Text.Lazy.IO                         as TextIO

data Action = InsertSerializationMode Node
            | SetComputedValue Int Value
            | ShowCode Text
            | RequestCode
            deriving (Show, Eq)

data Reaction = PerformIO (IO ())

instance PrettyPrinter Reaction where
    display _ = "backend(Reaction)"

instance PrettyPrinter Action where
    display = show

toAction :: Event Node -> Maybe Action
toAction (Batch (Batch.NodeAdded node)) = Just $ InsertSerializationMode node
toAction (Batch (Batch.ValueUpdate nodeId value)) = Just $ SetComputedValue nodeId value
toAction (Batch (Batch.CodeUpdate code)) = Just $ ShowCode code
toAction (Batch Batch.RunFinished) = Just $ RequestCode
toAction _ = Nothing

instance ActionStateUpdater Action where
    execSt RequestCode state = ActionUI (PerformIO action) state where
        action    = sendMessage $ getCode workspace
        workspace = state ^. Global.workspace
    execSt (ShowCode code) state = ActionUI (PerformIO action) state where
        action = TextIO.putStr code
    execSt (InsertSerializationMode node) state = ActionUI (PerformIO action) state where
        action = do
            let workspace = state ^. Global.workspace
            sendMessage $ insertSerializationMode workspace node
            sendMessage $ getCode workspace
    execSt (SetComputedValue nodeId value) state = ActionUI (PerformIO action) state where
        action = setComputedValue nodeId (show value)

instance ActionUIUpdater Reaction where
    updateUI (WithState (PerformIO act) st) = act
