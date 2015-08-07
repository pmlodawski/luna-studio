{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Button where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Fixed
import           GHCJS.Types (JSRef)

import           Object.Widget

data State  = Normal | Focused | Disabled | Pressed deriving (Eq, Show, Enum)

data Button = Button { _refId   :: Int
                     , _label   :: Text
                     , _state   :: State
                     , _pos     :: Vector2 Double
                     , _size    :: Vector2 Double
                     } deriving (Eq, Show, Typeable)

makeLenses ''Button

instance IsDisplayObject Button where
    objectId       b = b ^. refId
