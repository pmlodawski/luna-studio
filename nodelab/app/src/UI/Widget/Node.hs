{-# LANGUAGE OverloadedStrings #-}

module UI.Widget.Node where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           GHCJS.Types      ( JSVal, JSString )
import           Object.Widget
import           GHCJS.Prim
import           Utils.CtxDynamic
import           JS.Bindings (setCursor)
import           Object.UITypes
import           GHCJS.Marshal.Pure(PToJSVal(..), PFromJSVal(..))

import           UI.Types (UIWidget(..))

newtype Node = Node { unSlider :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget Node
