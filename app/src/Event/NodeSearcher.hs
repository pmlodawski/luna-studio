module Event.NodeSearcher where


import           Data.Word
import           Data.Dynamic
import           Data.Default
import           Data.Maybe     ( fromJust, isJust )
import           Data.Monoid
import           Control.Lens

import           Object.Dynamic
import           Object.Object  ( ID, Point(..), Object(..), Selectable(..) )
import           Event.Keyboard ( KeyMods(..) )
import qualified Event.WithObjects as WithObjects
import           Utils.PrettyPrinter

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)


data Event = Event { _action       :: Text
                   , _expression :: Text
                   } deriving (Eq, Show, Typeable)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Event action expr) = "NSev( " <> show action <> " -> " <> show expr <> " )"

newEvent :: Text -> Text -> Event
newEvent t e = Event t e

type MEvent obj = WithObjects.WithObjects Event obj

newWithObjects :: Typeable a => Event -> [Object a] -> WithObjects.WithObjects Event a
newWithObjects event objects = WithObjects.WithObjects event objects
