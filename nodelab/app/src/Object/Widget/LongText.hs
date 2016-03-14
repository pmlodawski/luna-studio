module Object.Widget.LongText where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.Widget
import           Data.Aeson (ToJSON)

data TextAlignment = Left | Center | Right deriving (Eq, Show, Generic)
instance ToJSON TextAlignment

data LongText = LongText { _position  :: Vector2 Double
                         , _size      :: Vector2 Double
                         , _value     :: Text
                         , _alignment :: TextAlignment
                         } deriving (Eq, Show, Typeable, Generic)

makeLenses ''LongText
instance ToJSON LongText

create :: Size -> Text -> TextAlignment -> LongText
create s v a = LongText def s v a

instance IsDisplayObject LongText where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True
