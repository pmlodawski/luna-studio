module Event.WebSocket where

import Utils.PreludePlus
import BatchConnector.Connection (WSMessage)

data Event = Message { _message :: WSMessage }
           | Opened
             deriving (Show, Typeable)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Message m) = show m
    display Opened      = "Opened"
