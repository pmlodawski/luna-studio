{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Slider where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Fixed
import           GHCJS.Types (JSRef)
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

import           Object.Widget


data Slider = Slider { _refId    :: Int
                     , _pos      :: Vector2 Double
                     , _size     :: Vector2 Double
                     , _label    :: Text
                     , _minValue :: Double
                     , _maxValue :: Double
                     , _value    :: Double
                     } deriving (Eq, Show, Typeable)

makeLenses ''Slider

instance IsDisplayObject Slider where
    objectId       b = b ^. refId


sliderPosition :: Slider -> Double
sliderPosition (Slider _ _ _ _ minv maxv v) = (v - minv) / (maxv - minv)

setValueNorm :: Double -> Slider -> Slider
setValueNorm normVal slider = slider & value .~ val where
    boundedValue  = max 0.0 $ min 1.0 normVal
    val           = slider ^. minValue + boundedValue * (slider ^. maxValue - slider ^. minValue)
