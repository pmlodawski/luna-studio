module Reactive.Plugins.Core.Action.Backend.Backend where

import           Utils.PreludePlus
import           Object.Node
import           Event.Event
import qualified Event.Batch        as Batch
import           Batch.Workspace
import           JS.Bindings        (displayRejectedMessage)

import qualified BatchConnector.Commands                   as BatchCmd
import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.State.Global as Global
import qualified Data.Text.Lazy.IO                         as TextIO
import           Batch.RunStatus
import           Reactive.Plugins.Core.Action.Commands.Command      (execCommand)
import           Reactive.Plugins.Core.Action.Commands.RefreshGraph (refreshGraph)

data Action = InsertSerializationMode Node
            | ShowCode Text
            | RequestCode
            | ConnectionTakeover
            | ShowProfilingInfo RunStatus
            | RefreshGraph
            deriving (Show, Eq)

data Reaction = PerformIO (IO ())

instance PrettyPrinter Reaction where
    display _ = "backend(Reaction)"

instance PrettyPrinter Action where
    display = show

toAction :: Event Node -> Maybe Action
toAction (Batch (Batch.NodeAdded node))         = Just $ InsertSerializationMode node
toAction (Batch (Batch.CodeUpdate code))        = Just $ ShowCode code
toAction (Batch (Batch.RunFinished status))     = Just $ ShowProfilingInfo status
toAction (Batch Batch.ConnectionDropped)        = Just $ ConnectionTakeover
toAction (Batch Batch.CodeSet)                  = Just RefreshGraph
toAction _ = Nothing

instance ActionStateUpdater Action where
    execSt RequestCode state = ActionUI (PerformIO action) state where
        action    = BatchCmd.getCode workspace
        workspace = state ^. Global.workspace

    execSt (ShowCode code) state = ActionUI (PerformIO action) state where
        action = TextIO.putStr code

    execSt (ShowProfilingInfo status) state = ActionUI (PerformIO action) state where
        action    = do
            BatchCmd.getCode workspace
            print status
        workspace = state ^. Global.workspace

    execSt ConnectionTakeover state = ActionUI (PerformIO displayRejectedMessage) state

    execSt (InsertSerializationMode node) state = ActionUI (PerformIO action) state where
        action = do
            let workspace = state ^. Global.workspace
            BatchCmd.insertSerializationMode workspace node

    execSt RefreshGraph state = ActionUI (PerformIO action) newState where
        (action, newState) = execCommand refreshGraph state

instance ActionUIUpdater Reaction where
    updateUI (WithState (PerformIO act) st) = act
