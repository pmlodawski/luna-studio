{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Types where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           GHCJS.Types         (JSVal)
import           Object.Widget
import qualified Data.JSString as JSString
import           Object.UITypes
import           GHCJS.Marshal.Pure(PToJSVal(..), PFromJSVal(..))
import           UI.Types

newtype Scene = Scene { unScene :: JSVal } deriving (PFromJSVal, PToJSVal)

instance UIWidget Scene

foreign import javascript unsafe "{container: common.scene }"    scene    :: IO Scene
foreign import javascript unsafe "{container: common.sceneHUD }" sceneHUD :: IO Scene
