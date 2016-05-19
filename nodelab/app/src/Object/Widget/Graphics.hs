module Object.Widget.Graphics where

import            Utils.PreludePlus hiding (Item)
import            Utils.Vector
import Data.Aeson (ToJSON)
import Style.Types

import            Object.Widget

data Box = Box { _boxPosition :: Vector2 Double
               , _boxSize     :: Vector2 Double
               } deriving (Eq, Show, Typeable, Generic)

data Item = Item { _shader    :: Text
                 , _boxes     :: [Box]
                 } deriving (Eq, Show, Typeable, Generic)

data Graphics = Graphics { _position  :: Vector2 Double
                         , _size      :: Vector2 Double
                         , _items     :: [Item]
                         } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Box
makeLenses ''Item
makeLenses ''Graphics
instance ToJSON Box
instance ToJSON Item
instance ToJSON Graphics

create :: Size -> [Item] -> Graphics
create = Graphics def

instance IsDisplayObject Graphics where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True
