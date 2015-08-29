{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Slider where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Fixed
import           Object.Widget
import           Numeric


data Slider a =  Slider { _refId     :: Int
                        , _pos       :: Vector2 Double
                        , _size      :: Vector2 Double
                        , _label     :: Text
                        , _minValue  :: a
                        , _maxValue  :: a
                        , _normValue :: Double
                        } deriving (Eq, Show, Typeable)

makeLenses ''Slider

class (Num a, Show a, Typeable a) => IsSlider a where
    displayValue ::           Slider a -> String
    value        ::           Slider a -> a
    setValue     :: a      -> Slider a -> Slider a

instance IsDisplayObject (Slider a) where
    objectId       b = b ^. refId
    objectPosition b = b ^. pos
    objectSize     b = b ^. size
    objectIdLens     = refId

instance IsSlider Double where
    displayValue slider = showFFloat (Just $ precision) val "" where
        val       = value slider
        precision = sliderPrecision slider
    value slider = min + val * range where
            min   = slider ^. minValue
            max   = slider ^. maxValue
            val   = slider ^. normValue
            range = max - min
    setValue val slider = slider & normValue .~ newVal where
            newVal = (val - min) / range
            min    = slider ^. minValue
            max    = slider ^. maxValue
            range  = max - min

setNormValue :: Double -> Slider a -> Slider a
setNormValue val slider = slider & normValue .~ boundedVal where
    boundedVal = max 0.0 $ min 1.0 val

instance IsSlider Int where
    displayValue s = show $ value s
    value        slider = round $ min + val * range where
            min   = fromIntegral $ slider ^. minValue
            max   = fromIntegral $ slider ^. maxValue
            val   = slider ^. normValue
            range = max - min
    setValue val slider = slider & normValue .~ newVal where
            newVal = ((fromIntegral val) - min) / range
            min    = fromIntegral $ slider ^. minValue
            max    = fromIntegral $ slider ^. maxValue
            range  = max - min

sliderPrecision :: Slider Double -> Int
sliderPrecision s = displayPrecision (s ^. minValue) (s ^. maxValue)

displayPrecision :: Double -> Double -> Int
displayPrecision minV maxV = numDigits $ ceiling $ max (alog10 minV) (alog10 maxV) where
    alog10 n               = logBase 10 $ abs n
    numDigits n            = max (3 - n) 0 where
