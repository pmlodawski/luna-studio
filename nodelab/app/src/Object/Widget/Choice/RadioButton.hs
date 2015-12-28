module Object.Widget.Choice.RadioButton where

import            Utils.PreludePlus
import            Utils.Vector
import            Object.Widget
import Data.Aeson (ToJSON)

data RadioButton = RadioButton { _position :: Vector2 Double
                               , _size     :: Vector2 Double
                               , _label    :: Text
                               , _selected :: Bool
                               } deriving (Eq, Show, Typeable, Generic)

makeLenses ''RadioButton

instance ToJSON RadioButton
instance IsDisplayObject RadioButton where
    widgetPosition = position
    widgetSize     = size
