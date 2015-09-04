module Reactive.Plugins.Core.Action.Backend where

import           Utils.PreludePlus
import           Object.Node
import           Event.Event
import           Batch.Workspace
import qualified Event.AddNode as AddNode

import           BatchConnector.Commands
import           BatchConnector.Connection
import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.Global

data Action = InsertSerializationMode Node Workspace deriving (Show, Eq)

data Reaction = PerformIO (IO ())

instance PrettyPrinter Reaction where
    display _ = "backend(Reaction)"

instance PrettyPrinter Action where
    display _ = "backend(InsertSerializationMode)"

toAction :: Workspace -> Event Node -> Maybe Action
toAction workspace (AddNode (AddNode.AddNode node)) = Just $ InsertSerializationMode node workspace
toAction _ _ = Nothing

instance ActionStateUpdater Action where
    execSt (InsertSerializationMode node workspace) state = ActionUI (PerformIO action) state where
        action = do
            sendMessage $ insertSerializationMode workspace node
            print "reacting"

instance ActionUIUpdater Reaction where
    updateUI (WithState (PerformIO act) st) = act
