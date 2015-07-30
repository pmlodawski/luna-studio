{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Button where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Fixed
import           GHCJS.Types (JSRef)

import           Object.Widget
import qualified Object.Widget.Types as T


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


instance IsDisplayObject Button where
    objectId       b = b ^. refId
    isOver   mouse b = (posHit (pos' ^. x) (size' ^. x) (mouse' ^. x)) && (posHit (pos' ^. y) (size' ^. y) (mouse' ^. y)) where
        pos'  = b ^. pos
        size' = b ^. size
        mouse' = fromIntegral <$> mouse