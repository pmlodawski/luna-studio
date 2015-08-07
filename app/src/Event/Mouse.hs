module Event.Mouse where


import           Utils.PreludePlus

import           Object.Dynamic
import           Object.Object
import           Event.Keyboard  ( KeyMods(..) )
import           Utils.Vector

type WidgetId    = Int

data MouseButton = NoButton | LeftButton | MiddleButton | RightButton deriving (Show, Eq)

toMouseButton :: Int -> MouseButton
toMouseButton   1  = LeftButton
toMouseButton   2  = MiddleButton
toMouseButton   3  = RightButton
toMouseButton   _  = NoButton

type MousePosition = Vector2 Int

data Type = Pressed | Released | Moved | Clicked | DblClicked deriving (Eq, Show)

data EventWidget = EventWidget { _widgetId :: WidgetId, _relativePos :: Vector2 Int } deriving (Show, Eq)

makeLenses ''EventWidget

data Event = Event { _tpe         :: Type
                   , _position    :: MousePosition
                   , _button      :: MouseButton
                   , _keyMods     :: KeyMods
                   , _widget      :: Maybe EventWidget
                   } deriving (Eq, Show, Typeable)

makeLenses ''Event

instance PrettyPrinter MouseButton where
    display = show

instance PrettyPrinter EventWidget where
    display = show

instance PrettyPrinter Type where
    display = show

instance PrettyPrinter Event where
    display (Event tpe pos button keyMods widget) = "ev(" <> display tpe     <>
                                                    " "   <> display pos     <>
                                                    " "   <> display button  <>
                                                    " "   <> display keyMods <>
                                                    " "   <> display widget  <>
                                                    ")"
