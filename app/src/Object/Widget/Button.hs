{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Button where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Fixed
import           GHCJS.Types (JSRef)

-- import           Object.Widget


data State  = Normal | Focused | Disabled | Pressed deriving (Eq, Show, Enum)

data Button = Button { _refId   :: Int
                     , _label   :: Text
                     , _state   :: State
                     , _pos     :: Vector2 Double
                     , _size    :: Vector2 Double
                     } deriving (Eq, Show, Typeable)

makeLenses ''Button

posHit :: Double -> Double -> Double -> Bool
posHit center range position
    | position >= rangeStart && position <= rangeEnd = True
    | otherwise                                      = False
    where
        rangeStart = center
        rangeEnd   = center + range

isOver :: Vector2 Double -> Button -> Bool
isOver mouse (Button  _ _ _ pos size) = (posHit (pos ^. x) (size ^. x) (mouse ^. x)) && (posHit (pos ^. y) (size ^. y) (mouse ^. y))

