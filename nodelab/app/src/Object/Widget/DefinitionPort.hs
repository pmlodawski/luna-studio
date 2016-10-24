module Object.Widget.DefinitionPort where

import           Data.Aeson        (ToJSON)
import           Object.Widget
import           Utils.PreludePlus
import           Utils.Vector


data InputOutput = Input | Output
        deriving (Eq, Show, Typeable, Generic)


data DefinitionPort = DefinitionPort
                    { _position    :: Vector2 Double
                    , _size        :: Vector2 Double
                    , _focused     :: Bool
                    , _labelValue  :: Text
                    , _inputOutput :: InputOutput
                    } deriving (Eq, Show, Typeable, Generic)

makeLenses ''DefinitionPort

instance ToJSON InputOutput
instance ToJSON DefinitionPort
instance IsDisplayObject DefinitionPort where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True


create :: Vector2 Double -> Bool -> Text -> InputOutput -> DefinitionPort
create = DefinitionPort def
