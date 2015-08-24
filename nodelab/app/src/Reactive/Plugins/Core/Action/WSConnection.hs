module Reactive.Plugins.Core.Action.WSConnection where

import           Utils.PreludePlus
import           Reactive.Plugins.Core.Action.Action
import qualified Event.WebSocket as WSEvent
import           Event.Event
import           Object.Node

data Action = MessageAction { _event   :: WSEvent.Event }
            | ApplyUpdates  { _actions :: [IO ()] }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "WebSocketAction"

toAction :: Event Node -> Maybe Action
toAction (WebSocket e) = Just $ MessageAction e
toAction _             = Nothing

instance ActionStateUpdater Action where
    execSt (MessageAction event) = ActionUI (ApplyUpdates [print $ event ^. WSEvent.message])

instance ActionUIUpdater Action where
    updateUI (WithState (ApplyUpdates updates) state) = sequence_ updates
