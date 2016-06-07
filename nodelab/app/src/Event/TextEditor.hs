module Event.TextEditor where


import Utils.PreludePlus
import Object.UITypes
import Data.Aeson (ToJSON)

data Event = CodeModified { _code :: Text } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Event

instance ToJSON Event
