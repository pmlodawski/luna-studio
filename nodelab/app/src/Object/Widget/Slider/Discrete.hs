module Object.Widget.Slider.Discrete where

import Utils.PreludePlus
import Utils.Vector
import Object.Object
import Object.Widget
import Numeric
import Data.Aeson (ToJSON)

data DiscreteSlider = DiscreteSlider { _position       :: Vector2 Double
                                     , _size           :: Vector2 Double
                                     , _label          :: Text
                                     , _enabled        :: Bool
                                     , _minValue       :: Int
                                     , _maxValue       :: Int
                                     , _value          :: Int
                                     , _dragStartValue :: Maybe Int
                                     } deriving (Eq, Show, Typeable, Generic)

makeLenses    ''DiscreteSlider
instance ToJSON DiscreteSlider

instance IsDisplayObject DiscreteSlider where
    widgetPosition = position
    widgetSize     = size
    
displayValue' :: DiscreteSlider -> String
displayValue' slider = show $ slider ^. value

displayValue :: Getter DiscreteSlider String
displayValue = to displayValue'

range' :: DiscreteSlider -> Int
range' slider = (slider ^. maxValue) - (slider ^. minValue)

range :: Getter DiscreteSlider Int
range = to range'

getNormValue :: DiscreteSlider -> Double
getNormValue slider = (fromIntegral $ (slider ^. value) - (slider ^. minValue)) / (fromIntegral $ slider ^. range)

setNormValue :: DiscreteSlider -> Double -> DiscreteSlider
setNormValue slider val = slider & value .~ newValue where
    boundedValue = max 0.0 $ min 1.0 val
    newValue = (round $ boundedValue * (fromIntegral $ slider ^. range)) + (slider ^. minValue)

boundedNormValue :: Lens' DiscreteSlider Double
boundedNormValue = lens getNormValue setNormValue

getValue :: DiscreteSlider -> Int
getValue slider = slider ^. value

setValue :: DiscreteSlider -> Int -> DiscreteSlider
setValue slider val = slider & value .~ boundedValue where
    boundedValue = max (slider ^. minValue) $ min (slider ^. maxValue) val

boundedValue :: Lens' DiscreteSlider Int
boundedValue = lens getValue setValue
