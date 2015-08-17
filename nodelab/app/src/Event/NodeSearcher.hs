module Event.NodeSearcher where


import           Utils.PreludePlus


data Event = Event { _action     :: Text
                   , _expression :: Text
                   } deriving (Eq, Show)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Event action expr) = "NSev(" <> show action <> " -> " <> show expr <> ")"

