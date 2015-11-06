{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Button where

import Utils.PreludePlus
import Utils.Vector

import Object.Widget

data State  = Normal | Focused | Disabled | Pressed deriving (Eq, Show, Enum)

data Button = Button { _label    :: Text
                     , _state    :: State
                     , _position :: Vector2 Double
                     , _size     :: Vector2 Double
                     } deriving (Eq, Show, Typeable)

makeLenses ''Button

instance IsDisplayObject Button where
    widgetPosition = position
