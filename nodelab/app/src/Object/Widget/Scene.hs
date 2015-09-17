{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Scene where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Fixed
import           Object.UITypes
import           Object.Widget
import           Numeric


data Scene = Scene deriving (Show)

makeLenses ''Scene

instance IsDisplayObject Scene where
    objectPosition = undefined
    objectSize     = undefined

instance DisplayObjectContainer Scene
