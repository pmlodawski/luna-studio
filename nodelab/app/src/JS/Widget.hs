{-# LANGUAGE JavaScriptFFI #-}

module JS.Widget where

import           Utils.PreludePlus
import           Utils.Vector

import           Data.Dynamic
import           Data.Text.Lazy      (Text)

import           GHCJS.Foreign

foreign import javascript unsafe "$$.registry[$1].setFocused($2)"
    setWidgetFocused :: Int -> Bool -> IO ()
