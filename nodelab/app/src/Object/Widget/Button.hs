{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Button where

import Utils.PreludePlus
import Utils.Vector

import Object.Widget
import Data.Aeson (ToJSON)

data Button = Button { _label    :: Text
                     , _position :: Vector2 Double
                     , _size     :: Vector2 Double
                     , _enabled  :: Bool
                     } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Button
instance ToJSON Button

instance IsDisplayObject Button where
    widgetPosition = position
    widgetSize     = size
