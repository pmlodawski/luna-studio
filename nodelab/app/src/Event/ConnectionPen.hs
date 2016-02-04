module Event.ConnectionPen where


import Utils.PreludePlus
import Object.UITypes
import Data.Aeson (ToJSON)


data Event = Segment { _widgets :: [WidgetId] } deriving (Show, Typeable, Generic)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Segment m) = show m
instance ToJSON Event
