{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Number where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Fixed
import           GHCJS.Types (JSRef)
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

import           Object.Widget
import           Numeric


data Number a =  Number { _refId     :: Int
                        , _pos       :: Vector2 Double
                        , _size      :: Vector2 Double
                        , _label     :: Text
                        , _value     :: a
                        } deriving (Eq, Show, Typeable)

makeLenses ''Number

-- class (Num a, Show a, Typeable a) => IsSlider a where
--     displayValue ::           Slider a -> String
--     value        ::           Slider a -> a
--     setValue     :: a      -> Slider a -> Slider a

instance IsDisplayObject (Number a) where
    objectId       b = b ^. refId
    objectPosition b = b ^. pos
    objectSize     b = b ^. size
    objectIdLens     = refId
-- instance IsSlider Double where
--     displayValue slider = showFFloat (Just $ precision) val "" where
--         val       = value slider
--         precision = sliderPrecision slider
--     value slider = min + val * range where
--             min   = slider ^. minValue
--             max   = slider ^. maxValue
--             val   = slider ^. normValue
--             range = max - min
--     setValue val slider = slider & normValue .~ newVal where
--             newVal = (val - min) / range
--             min    = slider ^. minValue
--             max    = slider ^. maxValue
--             range  = max - min

