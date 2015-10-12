module Reactive.Plugins.Core.Action.Backend.Backend where

import           Utils.PreludePlus
import           Object.Node
import           Event.Event
import qualified Event.Batch        as Batch
import           Batch.Workspace
import           JS.Bindings        (displayRejectedMessage, writeToTerminal)

import qualified BatchConnector.Commands                   as BatchCmd
import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.State.Global as Global
import qualified Reactive.Plugins.Core.Action.State.Graph  as Graph
import qualified Data.Text.Lazy.IO                         as TextIO
import           Batch.RunStatus
import           Reactive.Plugins.Core.Action.Commands.Command      (execCommand)
import           Reactive.Plugins.Core.Action.Commands.RefreshGraph (refreshGraph)

data Action = InsertSerializationMode Node
            | RequestCode
            | ConnectionTakeover
            | ShowProfilingInfo RunStatus
            | RefreshGraph
            | DisplayError String
            | NextSerializationMode
            deriving (Show, Eq)

data Reaction = PerformIO (IO ())

instance PrettyPrinter Reaction where
    display _ = "backend(Reaction)"

instance PrettyPrinter Action where
    display = show

toAction :: Event Node -> Maybe Action
toAction (Batch (Batch.NodeAdded node))          = Just $ InsertSerializationMode node
toAction (Batch (Batch.RunFinished status))      = Just $ ShowProfilingInfo status
toAction (Batch Batch.ConnectionDropped)         = Just $ ConnectionTakeover
toAction (Batch Batch.CodeSet)                   = Just RefreshGraph
toAction (Batch (Batch.CodeSetError msg))        = Just $ DisplayError msg
toAction (Batch Batch.SerializationModeInserted) = Just NextSerializationMode
toAction _ = Nothing

instance ActionStateUpdater Action where
    execSt NextSerializationMode state = ActionUI (PerformIO action) newState where
        newState   = state & Global.workspace . interpreterState %~ (addSerializationMode nodesCount)
        nodesCount = length $ state ^. Global.graph . Graph.nodes
        action     = case newState ^. Global.workspace . interpreterState of
            AllSet -> BatchCmd.runMain
            _      -> return ()

    execSt RequestCode state = ActionUI (PerformIO action) state where
        action    = BatchCmd.getCode workspace
        workspace = state ^. Global.workspace

    execSt (ShowProfilingInfo status) state = ActionUI (PerformIO action) state where
        action    = BatchCmd.getCode workspace
        workspace = state ^. Global.workspace

    execSt ConnectionTakeover state = ActionUI (PerformIO displayRejectedMessage) state

    execSt (InsertSerializationMode node) state = ActionUI (PerformIO action) state where
        action = do
            let workspace = state ^. Global.workspace
            when (not $ isModule node) $ BatchCmd.insertSerializationMode workspace node

    execSt RefreshGraph state = ActionUI (PerformIO action) newState where
        (action, newState) = execCommand refreshGraph state

    execSt (DisplayError msg) state = ActionUI (PerformIO action) state where
        action = writeToTerminal msg

instance ActionUIUpdater Reaction where
    updateUI (WithState (PerformIO act) st) = act
