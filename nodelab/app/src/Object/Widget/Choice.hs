module Object.Widget.Choice where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.Widget
import           Data.Text (Text)

data Choice = Choice { _position :: Vector2 Double
                     , _size     :: Vector2 Double
                     , _label    :: Text
                     , _options  :: [Text]
                     , _value    :: Word
                     } deriving (Eq, Show, Typeable)

makeLenses ''Choice

instance IsDisplayObject Choice where
    widgetPosition = position

