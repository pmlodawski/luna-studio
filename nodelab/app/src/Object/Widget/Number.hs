{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Number where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Fixed
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

import           Object.Widget
import           Numeric


data Number a =  Number { _pos       :: Vector2 Double
                        , _size      :: Vector2 Double
                        , _label     :: Text
                        , _value     :: a
                        } deriving (Eq, Show, Typeable)

makeLenses ''Number
--
-- class (Num a, Show a, Typeable a) => IsSlider a where
--     displayValue ::           Slider a -> String
--     value        ::           Slider a -> a
--     setValue     :: a      -> Slider a -> Slider a
