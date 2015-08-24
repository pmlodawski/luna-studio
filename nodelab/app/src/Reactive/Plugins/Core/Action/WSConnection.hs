module Reactive.Plugins.Core.Action.WSConnection where

import           Utils.PreludePlus
import           Reactive.Plugins.Core.Action.Action
import           Event.Event
import           Object.Node
import           BatchConnector.Connection (WSMessage)
import qualified Event.WebSocket as WSEvent

data Action = MessageAction { _event   :: WSMessage }
            | OpenedAction
            | ApplyUpdates  { _actions :: [IO ()] }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "WebSocketAction"

toAction :: Event Node -> Maybe Action
toAction (WebSocket event) = Just $ case event of
    WSEvent.Message msg -> MessageAction msg
    WSEvent.Opened      -> OpenedAction
toAction _                 = Nothing

instance ActionStateUpdater Action where
    execSt (MessageAction msg) = ActionUI $ ApplyUpdates [print msg]
    execSt OpenedAction        = ActionUI $ ApplyUpdates [putStrLn "Connection Opened!"]

instance ActionUIUpdater Action where
    updateUI (WithState (ApplyUpdates updates) state) = sequence_ updates
