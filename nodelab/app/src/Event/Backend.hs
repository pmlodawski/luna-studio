module Event.Backend where

import Utils.PreludePlus
import BatchConnector.Connection (WebMessage)

data Event = Message { _message :: WebMessage }
           | Opened
             deriving (Show, Typeable)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Message m) = show m
    display Opened      = "Opened"
