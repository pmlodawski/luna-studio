module Object.Widget.Label where

import           Data.Aeson        (ToJSON)
import           Object.Widget
import           Utils.PreludePlus hiding (Either(..))
import           Utils.Vector

data TextAlignment = Left | Center | Right deriving (Eq, Show, Generic)
instance ToJSON TextAlignment

data Label = Label { _position  :: Vector2 Double
                   , _size      :: Vector2 Double
                   , _alignment :: TextAlignment
                   , _label     :: Text
                   } deriving (Eq, Show, Typeable, Generic)

makeLenses    ''Label
instance ToJSON Label

create :: Size -> Text -> Label
create size = Label def size Left

instance IsDisplayObject Label where
    widgetPosition = position
    widgetSize     = size
