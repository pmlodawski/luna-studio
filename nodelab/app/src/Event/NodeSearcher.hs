module Event.NodeSearcher where


import           Data.Aeson           (FromJSON, ToJSON)
import           Empire.API.Data.Node (NodeId)
import           Utils.PreludePlus

data Event = Query     Text
           | Tree      Text
           | Create    Text
           | QueryCmd  Text
           | TreeCmd   Text
           | CreateCmd Text
           deriving (Eq, Show, Generic)

makeLenses ''Event

instance ToJSON Event
instance FromJSON Event

