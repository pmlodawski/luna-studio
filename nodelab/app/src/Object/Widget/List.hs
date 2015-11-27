{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Object.Widget.List where

import           Utils.PreludePlus hiding (Choice)
import           Utils.Vector
import           Object.Widget
import           Object.UITypes
import           Object.LunaValue
import           Data.Aeson (ToJSON, toJSON, object, Value)

data List = List { _position :: Vector2 Double
                 , _size     :: Vector2 Double
                 , _label    :: Text
                 , _value    :: [AnyLunaValue]
                 , _empty    :: AnyLunaValue
                 } deriving (Show, Typeable, Generic)

makeLenses ''List
instance ToJSON List

instance IsDisplayObject List where
    widgetPosition = position
    widgetSize     = size

create :: Text -> [AnyLunaValue] -> AnyLunaValue -> List
create l v e = List def def l v e
