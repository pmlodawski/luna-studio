module Reactive.Plugins.Core.Action.Backend.Backend where

import           Utils.PreludePlus
import           Object.Node        (Node, isModule)
import           Event.Event        (Event(Batch))
import qualified Event.Batch        as Batch
import qualified Batch.Workspace    as Workspace
import           Batch.Workspace    (InterpreterState(..))
import           JS.Bindings        (displayRejectedMessage, writeToTerminal)

import qualified BatchConnector.Monadic.Commands as BatchCmd

import           Reactive.State.Global          (State)
import qualified Reactive.State.Global          as Global
import qualified Reactive.State.Graph           as Graph
import           Reactive.Commands.Command      (Command, performIO)
import           Reactive.Commands.RefreshGraph (refreshGraph)

toAction :: Event Node -> Maybe (Command State ())
toAction (Batch (Batch.NodeAdded node))          = Just $ insertSerializationMode node
toAction (Batch (Batch.RunFinished status))      = Just $ zoom Global.workspace $ BatchCmd.getCode
toAction (Batch Batch.ConnectionDropped)         = Just $ performIO displayRejectedMessage
toAction (Batch Batch.CodeSet)                   = Just refreshGraph
toAction (Batch (Batch.CodeSetError msg))        = Just $ performIO $ writeToTerminal msg
toAction (Batch Batch.SerializationModeInserted) = Just handleNextSerializationMode
toAction _ = Nothing

insertSerializationMode :: Node -> Command State ()
insertSerializationMode node = when (not $ isModule node)
                                    $ zoom Global.workspace $ BatchCmd.insertSerializationMode node

handleNextSerializationMode :: Command State ()
handleNextSerializationMode = do
    nodes <- use $ Global.graph . Graph.nodes
    let nodesCount = length $ filter (not . isModule) nodes
    Global.workspace . Workspace.interpreterState %= (Workspace.addSerializationMode nodesCount)
    readyState <- use $ Global.workspace . Workspace.interpreterState
    when (readyState == AllSet) $ zoom Global.workspace BatchCmd.runMain
