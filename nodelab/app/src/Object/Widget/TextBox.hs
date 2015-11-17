module Object.Widget.TextBox where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.Widget
import           Data.Text (Text)

data TextBox = TextBox { _position :: Vector2 Double
                       , _size     :: Vector2 Double
                       , _label    :: Text
                       , _value    :: Text
                       } deriving (Eq, Show, Typeable)

makeLenses ''TextBox

instance IsDisplayObject TextBox where
    widgetPosition = position

