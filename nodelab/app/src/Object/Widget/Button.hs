{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Button where

import Utils.PreludePlus
import Utils.Vector

import Object.Widget
import Data.Aeson (ToJSON)

data State  = Normal | Focused | Disabled | Pressed deriving (Eq, Show, Enum, Generic)

data Button = Button { _label    :: Text
                     , _state    :: State
                     , _position :: Vector2 Double
                     , _size     :: Vector2 Double
                     } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Button
instance ToJSON Button
instance ToJSON State

instance IsDisplayObject Button where
    widgetPosition = position
