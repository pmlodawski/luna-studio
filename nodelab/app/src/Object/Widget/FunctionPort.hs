{-# LANGUAGE OverloadedStrings #-}
module Object.Widget.FunctionPort where

import           Data.Aeson             (ToJSON)
import           Empire.API.Data.Input  (Input)
import qualified Empire.API.Data.Input  as Input
import           Empire.API.Data.Output (Output)
import qualified Empire.API.Data.Output as Output
import           Object.Widget
import           Utils.PreludePlus
import           Utils.Vector


data InputOutput = Input | Output
        deriving (Eq, Show, Typeable, Generic)


data FunctionPort = FunctionPort
                    { _position    :: Vector2 Double
                    , _hovered     :: Bool
                    , _size        :: Vector2 Double
                    , _inputOutput :: InputOutput
                    , _labelValue  :: Text
                    } deriving (Eq, Show, Typeable, Generic)

makeLenses ''FunctionPort

instance ToJSON InputOutput
instance ToJSON FunctionPort
instance IsDisplayObject FunctionPort where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True


create :: InputOutput -> Text -> FunctionPort
create = FunctionPort def False (Vector2 50 50)

--TODO prooer initialisation
fromInput :: Input -> FunctionPort
fromInput input = create Input (input ^. Input.name)

--TODO prooer initialisation
fromOutput :: Output -> FunctionPort
fromOutput output = create Output "-> Result"
