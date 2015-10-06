module Event.NodeSearcher where


import           Utils.PreludePlus


data Event = Event { _action     :: Text
                   , _expression :: Text
                   , _node       :: Maybe Int
                   } deriving (Eq, Show)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Event action expr node) = "NSev(" <> show action <> " " <> show node <> " -> " <> show expr <> ")"

