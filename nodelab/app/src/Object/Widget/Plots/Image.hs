module Object.Widget.Plots.Image where

import           Data.Aeson        (ToJSON)
import           Utils.PreludePlus
import           Utils.Vector

import           Object.Widget


data Image = Image { _position   :: Vector2 Double
                   , _size       :: Vector2 Double
                   , _image      :: Text
                   } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Image
instance ToJSON Image

create :: Size -> Text -> Image
create = Image def

instance IsDisplayObject Image where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True
