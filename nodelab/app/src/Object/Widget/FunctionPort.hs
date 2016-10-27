{-# LANGUAGE OverloadedStrings #-}
module Object.Widget.FunctionPort where

import           Data.Aeson            (ToJSON)
import           Empire.API.Data.Input (Input)
import qualified Empire.API.Data.Input as Input
import           Empire.API.Data.Output (Output)
import qualified Empire.API.Data.Output as Output
import           Object.Widget
import           Utils.PreludePlus
import           Utils.Vector


data InputOutput = Input | Output
        deriving (Eq, Show, Typeable, Generic)


data FunctionPort = FunctionPort
                    { _position    :: Vector2 Double
                    , _size        :: Vector2 Double
                    , _hovered     :: Bool
                    , _labelValue  :: Text
                    , _inputOutput :: InputOutput
                    } deriving (Eq, Show, Typeable, Generic)

makeLenses ''FunctionPort

instance ToJSON InputOutput
instance ToJSON FunctionPort
instance IsDisplayObject FunctionPort where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True


create :: Vector2 Double -> Bool -> Text -> InputOutput -> FunctionPort
create = FunctionPort def

--TODO prooer initialisation
fromInput :: Input -> FunctionPort
fromInput input = create def False (input ^. Input.name) Input

--TODO prooer initialisation
fromOutput :: Output -> FunctionPort
fromOutput output = create def False "" Output
