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
    widgetPosition = error "Scene has no position"
instance UIDisplayObject Scene where
    createUI _ _ _ = error "Scene has no creator"
    updateUI _ _ _ = error "Scene has no updater"
