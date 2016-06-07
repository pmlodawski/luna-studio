module Event.CustomEvent where


import           Data.Aeson        (ToJSON, toJSON)
import           Utils.PreludePlus
import           GHCJS.Types (JSVal)


data Event = RawEvent String JSVal deriving (Generic)

makeLenses ''Event

instance ToJSON Event where
    toJSON (RawEvent topic _) = toJSON $ "Event: " <> topic

instance Show Event where
    show (RawEvent topic _) = show $ "Event: " <> topic
