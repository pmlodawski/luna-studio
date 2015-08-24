module Event.WebSocket where

import Utils.PreludePlus
import BatchConnector.Connection (WSMessage)

data Event = Event { _message :: WSMessage } deriving (Show, Typeable)

makeLenses ''Event

instance PrettyPrinter Event where
    display e = show (e ^. message)
