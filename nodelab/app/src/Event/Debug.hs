module Event.Debug where


import           Data.Aeson        (ToJSON)
import           Utils.PreludePlus


data Event = GetState deriving (Eq, Show, Generic)

makeLenses ''Event

instance ToJSON Event
