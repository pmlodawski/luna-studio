{-# LANGUAGE OverloadedStrings #-}

module UI.Widget.Node where

import           Utils.PreludePlus
import           GHCJS.Types      (JSVal)
import           GHCJS.Marshal.Pure(PToJSVal(..), PFromJSVal(..))
import           UI.Types (UIWidget(..))

newtype Node = Node { unSlider :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget Node
