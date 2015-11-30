module Object.Widget.Toggle where

import            Utils.PreludePlus
import            Utils.Vector
import            Object.Widget
import Data.Aeson (ToJSON)

data Toggle = Toggle { _position :: Vector2 Double
                     , _size     :: Vector2 Double
                     , _label    :: Text
                     , _value    :: Bool
                     , _enabled  :: Bool
                     , _focused  :: Bool
                     } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Toggle
instance ToJSON Toggle

create :: Size -> Text -> Bool -> Toggle
create s l v = Toggle def s l v True False

instance IsDisplayObject Toggle where
    widgetPosition = position
    widgetSize     = size
