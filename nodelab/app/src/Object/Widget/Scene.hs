{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Scene where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Fixed
import           Object.UITypes
import           Object.Widget
import           Numeric


data Scene = Scene { _refId :: WidgetId } deriving (Show)

makeLenses ''Scene

instance IsDisplayObject Scene where
    objectId i = i ^. refId
    objectPosition = undefined
    objectSize = undefined
    idLens = refId

instance DisplayObjectContainer Scene

sceneInterfaceId, sceneGraphId :: Int
sceneInterfaceId = 1
sceneGraphId     = 2
