module Event.Mouse where


import           Utils.PreludePlus

import           Object.Dynamic
import           Object.Object
import           Event.Keyboard       ( KeyMods(..) )
import           Utils.Vector


data MouseButton =  NoButton | LeftButton | MiddleButton | RightButton deriving (Show, Eq)

toMouseButton :: Word -> MouseButton
toMouseButton (-1) = NoButton
toMouseButton   0  = LeftButton
toMouseButton   1  = MiddleButton
toMouseButton   2  = RightButton
toMouseButton   _  = NoButton


data Type = Pressed | Released | Moved | Clicked | DblClicked deriving (Eq, Show, Typeable)

data Event = Event { _tpe      :: Type
                   , _position :: Vector2 Int
                   , _button   :: MouseButton
                   , _keyMods  :: KeyMods
                   } deriving (Eq, Show, Typeable)

makeLenses ''Event

instance PrettyPrinter MouseButton where
    display = show

instance PrettyPrinter Type where
    display = show

instance PrettyPrinter Event where
    display (Event tpe pos button keyMods) = "ev(" <> display tpe <>
                                             " "   <> display pos <>
                                             " "   <> display button <>
                                             " "   <> display keyMods <>
                                             ")"
