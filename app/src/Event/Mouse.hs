module Event.Mouse where


import           Utils.PreludePlus

import           Object.Dynamic
import           Object.Object
import           Event.Keyboard       ( KeyMods(..) )
import           Utils.Vector


data MouseButton = NoButton | LeftButton | MiddleButton | RightButton deriving (Show, Eq)

toMouseButton :: Int -> MouseButton
toMouseButton   1  = LeftButton
toMouseButton   2  = MiddleButton
toMouseButton   3  = RightButton
toMouseButton   _  = NoButton

data Type = Pressed | Released | Moved | Clicked | DblClicked deriving (Eq, Show)

data Event = Event { _tpe         :: Type
                   , _position    :: Vector2 Int
                   , _button      :: MouseButton
                   , _keyMods     :: KeyMods
                   , _widgetId    :: Maybe Int
                   , _relativePos :: Maybe (Vector2 Int)
                   } deriving (Eq, Show, Typeable)

makeLenses ''Event

instance PrettyPrinter MouseButton where
    display = show

instance PrettyPrinter Type where
    display = show

instance PrettyPrinter Event where
    display (Event tpe pos button keyMods widget relpos) = "ev(" <> display tpe <>
                                                           " "   <> display pos     <>
                                                           " "   <> display button  <>
                                                           " "   <> display keyMods <>
                                                           " "   <> display widget  <>
                                                           " "   <> display relpos  <>
                                                           ")"
