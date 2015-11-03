{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Scene where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           GHCJS.Types         (JSVal)
import           Object.Widget
import qualified Data.JSString as JSString
import           Object.UITypes
import           GHCJS.Marshal.Pure(PToJSVal(..), PFromJSVal(..))
import           UI.Widget

newtype Scene = Scene { unScene :: JSVal } deriving (PFromJSVal, PToJSVal)

instance UIWidget Scene
instance UIContainer Scene

foreign import javascript unsafe "{container: common.scene }"    scene    :: IO Scene
foreign import javascript unsafe "{container: common.sceneHUD }" sceneHUD :: IO Scene
