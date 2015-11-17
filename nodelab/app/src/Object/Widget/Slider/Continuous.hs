module Object.Widget.Slider.Continuous where

import Utils.PreludePlus
import Utils.Vector
import Data.Fixed
import Object.Object
import Object.Widget
import Numeric
import Data.Aeson (ToJSON)

data ContinuousSlider = ContinuousSlider { _position       :: Vector2 Double
                                         , _size           :: Vector2 Double
                                         , _label          :: Text
                                         , _enabled        :: Bool
                                         , _minValue       :: Double
                                         , _maxValue       :: Double
                                         , _value          :: Double
                                         , _dragStartValue :: Maybe Double
                                         } deriving (Eq, Show, Typeable, Generic)

makeLenses    ''ContinuousSlider
instance ToJSON ContinuousSlider

instance IsDisplayObject ContinuousSlider where
    widgetPosition = position

displayValue' :: ContinuousSlider -> String
displayValue' slider = showFFloat (Just $ precision) val "" where
    val             = slider ^. value
    precision       = sliderPrecision slider

displayValue :: Getter ContinuousSlider String
displayValue = to displayValue'

sliderPrecision :: ContinuousSlider -> Int
sliderPrecision s = displayPrecision (s ^. minValue) (s ^. maxValue)

displayPrecision :: Double -> Double -> Int
displayPrecision minV maxV = numDigits $ ceiling $ max (alog10 minV) (alog10 maxV) where
    alog10 n               = logBase 10 $ abs n
    numDigits n            = max (3 - n) 0 where

range' :: ContinuousSlider -> Double
range' slider = (slider ^. maxValue) - (slider ^. minValue)

range :: Getter ContinuousSlider Double
range = to range'

getNormValue :: ContinuousSlider -> Double
getNormValue slider = ((slider ^. value) - (slider ^. minValue)) / (slider ^. range)

setNormValue :: ContinuousSlider -> Double -> ContinuousSlider
setNormValue slider val = slider & value .~ newValue where
    boundedValue = max 0.0 $ min 1.0 val
    newValue     = boundedValue * (slider ^. range) + (slider ^. minValue)

boundedNormValue :: Lens' ContinuousSlider Double
boundedNormValue = lens getNormValue setNormValue

getValue :: ContinuousSlider -> Double
getValue slider = slider ^. value

setValue :: ContinuousSlider -> Double -> ContinuousSlider
setValue slider val = slider & value .~ boundedValue where
    boundedValue = max (slider ^. minValue) $ min (slider ^. maxValue) val

boundedValue :: Lens' ContinuousSlider Double
boundedValue = lens getValue setValue
