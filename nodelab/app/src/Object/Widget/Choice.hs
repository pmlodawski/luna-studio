module Object.Widget.Choice where

import           Utils.PreludePlus hiding (Choice)
import           Utils.Vector
import           Object.Widget
import           Object.UITypes
import           Data.Aeson (ToJSON)

data Choice = Choice { _position :: Vector2 Double
                     , _size     :: Vector2 Double
                     , _label    :: Text
                     , _options  :: [Text]
                     , _value    :: Word
                     } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Choice

create :: Size -> Text -> [Text] -> Word -> Choice
create s l o v = Choice def s l o v

instance ToJSON          Choice
instance IsDisplayObject Choice where
    widgetPosition = position
    widgetSize     = size
