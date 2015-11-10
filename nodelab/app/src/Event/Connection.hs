module Event.Connection where

import Utils.PreludePlus
import BatchConnector.Connection (WebMessage)

data Event = Message { _message :: WebMessage }
           | Opened
           | Closed  { _code :: Int }
           | Error
             deriving (Show, Typeable)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Message m)   = show m
    display Opened        = "Opened"
    display (Closed code) = "Closed " <> (show code)
    display Error         = "Error"
