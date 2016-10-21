module Object.Widget.DefinitionPort where

import           Data.Aeson        (ToJSON)
import           Object.Widget
import           Utils.PreludePlus
import           Utils.Vector



data DefinitionPort = DefinitionPort
                    { _position :: Vector2 Double
                    , _size     :: Vector2 Double
                    , _label    :: Text
                    , _selected :: Bool
                    } deriving (Eq, Show, Typeable, Generic)

makeLenses ''DefinitionPort

instance ToJSON DefinitionPort
instance IsDisplayObject DefinitionPort where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True
