module Event.Mouse where


import           Data.Word
import           Data.Dynamic
import           Data.Default
import           Data.Monoid
import           Control.Lens

import           Object.Dynamic
import           Object.Object
import           Event.Keyboard       ( KeyMods(..) )
import           Utils.Vector
import           Utils.PrettyPrinter


data Type = Pressed | Released | Moved deriving (Eq, Show, Typeable)

data Event = Event { _tpe      :: Type
                   , _position :: Vector2 Int
                   , _button   :: Int
                   , _keyMods  :: KeyMods
                   } deriving (Eq, Show, Typeable)

makeLenses ''Event

instance PrettyPrinter Type where
    display = show

instance PrettyPrinter Event where
    display (Event tpe pos button keyMods) = "ev( " <> display tpe <>
                                             " -> " <> display pos <>
                                             " "    <> display button <>
                                             " : "  <> display keyMods <>
                                             " )"
