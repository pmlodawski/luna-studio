module Object.Widget.Group where

import            Utils.PreludePlus
import            Utils.Vector
import            Object.Widget
import Data.Aeson (ToJSON)

data Group = Group { _position :: Vector2 Double
                   , _size     :: Vector2 Double
                   , _visible  :: Bool
                   } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Group
instance ToJSON Group

create :: Group
create = Group def def True

instance IsDisplayObject Group where
    widgetPosition = position
    widgetSize     = size
