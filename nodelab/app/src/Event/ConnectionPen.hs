module Event.ConnectionPen where


import Utils.PreludePlus
import Object.UITypes

data Event = Segment { _widgets :: [WidgetId] } deriving (Show, Typeable)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Segment m) = show m
