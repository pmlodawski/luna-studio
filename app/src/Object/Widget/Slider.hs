{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Slider where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Fixed
import           GHCJS.Types (JSRef)

import           Object.Widget
import qualified Object.Widget.Types as T


data Slider = Slider { _refId    :: Int
                     , _pos      :: Vector2 Double
                     , _size     :: Vector2 Double
                     , _minValue :: Double
                     , _maxValue :: Double
                     , _value    :: Double
                     } deriving (Eq, Show, Typeable)

makeLenses ''Slider

instance IsDisplayObject Slider where
    objectId       b = b ^. refId


sliderPosition :: Slider -> Double
sliderPosition (Slider _ _ _ minv maxv v) = (v - minv) / (maxv - minv)