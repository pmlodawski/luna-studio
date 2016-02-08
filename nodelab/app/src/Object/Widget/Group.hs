module Object.Widget.Group where

import            Utils.PreludePlus
import            Utils.Vector
import            Object.Widget
import Data.Aeson (ToJSON)

type Color = (Double, Double, Double)

data Group = Group { _position   :: Vector2 Double
                   , _size       :: Vector2 Double
                   , _visible    :: Bool
                   , _background :: Maybe Color
                   } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Group
instance ToJSON Group
instance ToJSON Color

create :: Group
create = Group def def True Nothing

createWithBg :: Color -> Group
createWithBg color = Group def def True (Just color)

instance IsDisplayObject Group where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = visible
