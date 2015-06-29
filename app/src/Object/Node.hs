module Object.Node where

import           Control.Applicative
import           Control.Lens
import           Data.Dynamic
import           Data.Monoid
import           Data.Maybe    ( isJust, catMaybes )

import           Utils.Wrapper
import           Utils.PrettyPrinter
import           Object.Dynamic
import           Object.Object ( ID, Point, Object(..), Selectable(..) )

data Node = Node { _ident    :: ID
                 , _selected :: Bool
                 , _position :: Point
                 } deriving (Eq, Show, Typeable)

type NodeSelection = [Node]

makeLenses ''Node

instance PrettyPrinter Node where
    display (Node id sel pos) = "n( " <> show id <> " " <> show sel <> " " <> display pos <> " )"

instance Selectable Node where
    setSelected n selected = n { _selected = selected }
    isSelected  n          = _selected n


isNode :: Object Dynamic -> Bool
isNode obj = isJust (unpackDynamic obj :: Maybe Node)
