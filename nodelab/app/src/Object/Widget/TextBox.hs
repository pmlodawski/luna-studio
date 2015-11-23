module Object.Widget.TextBox where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.Widget
import           Data.Aeson (ToJSON)

data TextBox = TextBox { _position  :: Vector2 Double
                       , _size      :: Vector2 Double
                       , _label     :: Text
                       , _value     :: Text
                       , _isEditing :: Bool
                       } deriving (Eq, Show, Typeable, Generic)

makeLenses ''TextBox
instance ToJSON TextBox

instance IsDisplayObject TextBox where
    widgetPosition = position
    widgetSize     = size

