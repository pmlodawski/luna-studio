module Event.NodeSearcher where


import           Data.Aeson        (ToJSON)
import           Utils.PreludePlus


data Event = Event { _action     :: Text
                   , _expression :: Text
                   , _node       :: Maybe Int
                   } deriving (Eq, Show, Generic)

makeLenses ''Event

instance ToJSON Event
