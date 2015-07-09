module Event.Mouse where


import           Data.Word
import           Data.Dynamic
import           Data.Default
import           Data.Maybe     ( fromJust, isJust )
import           Data.Monoid
import           Control.Lens

import           Object.Dynamic
import           Object.Object  ( ID, Point(..), Object(..), Selectable(..) )
import           Object.Node    --( Node(..), isNode )
import           Event.Keyboard ( KeyMods(..) )
import qualified Event.WithObjects as WithObjects
import           Utils.PrettyPrinter


data Type = Pressed | Released | Moved deriving (Eq, Show, Typeable)

data Event = Event { _tpe      :: Type
                   , _position :: Point
                   , _button   :: Int
                   , _keyMods  :: KeyMods
                   } deriving (Eq, Show, Typeable)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Event tpe pos butt keyMods) = "ev( " <> show tpe <> " -> " <> display pos <> " " <> show butt <>
                                           " : " <> display keyMods <> " )"

newEvent :: Type -> Point -> Int -> KeyMods -> Event
newEvent t p b k = Event t p b k



type MEvent obj = WithObjects.WithObjects Event obj


newWithObjects :: Typeable a => Event -> [Object a] -> WithObjects.WithObjects Event a
newWithObjects event objects = WithObjects.WithObjects event objects
