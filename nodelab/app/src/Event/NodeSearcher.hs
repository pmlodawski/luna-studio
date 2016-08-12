module Event.NodeSearcher where


import           Data.Aeson           (FromJSON, ToJSON)
import           Utils.PreludePlus

data Event = Query     Text
           | Tree      Text
           | Create    {_expression :: Text, _nodeId :: Maybe Int}
           | QueryCmd  Text
           | TreeCmd   Text
           | CreateCmd {_expression :: Text, _nodeId :: Maybe Int}
           deriving (Eq, Show, Generic)

makeLenses ''Event

instance ToJSON Event
instance FromJSON Event

