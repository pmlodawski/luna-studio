{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Object.Widget.List where

import           Utils.PreludePlus hiding (Choice)
import           Utils.Vector
import           Data.Aeson (ToJSON, toJSON, object, Value)

import           Object.Widget
import           Object.UITypes
import           Object.LunaValue
import           Object.Widget.Group (Group(..))

data List = List { _position    :: Vector2 Double
                 , _size        :: Vector2 Double
                 , _label       :: Text
                 , _value       :: [AnyLunaValue]
                 , _empty       :: AnyLunaValue
                 , _fixedLength :: Bool
                 } deriving (Show, Typeable, Generic)

makeLenses ''List
instance ToJSON List

instance IsDisplayObject List where
    widgetPosition = position
    widgetSize     = size

createList :: Double -> Text -> [AnyLunaValue] -> AnyLunaValue -> List
createList w l v e = List def (Vector2 w 0) l v e False

createTuple :: Text -> [AnyLunaValue] -> AnyLunaValue -> List
createTuple l v e = List def def l v e True

toGroup :: List -> Group
toGroup l = Group (l ^. position) (l ^. size) True Nothing