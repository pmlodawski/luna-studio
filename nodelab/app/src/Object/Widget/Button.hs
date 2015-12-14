{-# LANGUAGE OverloadedStrings #-}
module Object.Widget.Button where

import Utils.PreludePlus
import Utils.Vector

import Object.Widget
import Data.Aeson (ToJSON)

data Button = Button { _position :: Vector2 Double
                     , _size     :: Vector2 Double
                     , _label    :: Text
                     , _icon     :: Maybe Text
                     , _enabled  :: Bool
                     } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Button
instance ToJSON Button

instance IsDisplayObject Button where
    widgetPosition = position
    widgetSize     = size

create :: Size -> Text -> Button
create s l = Button def s l Nothing True

createIcon :: Size -> Text -> Button
createIcon s i = Button def s "" (Just i) True
