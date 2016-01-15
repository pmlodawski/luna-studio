module Object.Widget.Label where

import Utils.PreludePlus
import Utils.Vector
import Object.Widget
import Data.Aeson (ToJSON)

data Label = Label { _position :: Vector2 Double
                   , _size     :: Vector2 Double
                   , _label    :: Text
                   } deriving (Eq, Show, Typeable, Generic)

makeLenses    ''Label
instance ToJSON Label

create :: Text -> Label
create t = Label def (Vector2 20.0 20.0) t

instance IsDisplayObject Label where
    widgetPosition = position
    widgetSize     = size
