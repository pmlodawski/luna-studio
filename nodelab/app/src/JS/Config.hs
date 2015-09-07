{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module JS.Config where

import           Utils.PreludePlus

import           Data.Dynamic
import           Data.Text.Lazy      ( Text )

import           GHCJS.Foreign
import           GHCJS.Types         ( JSRef, JSString )

foreign import javascript safe "config.fontSize" fontSize :: Double

foreign import javascript safe "features.widget_sandbox" widgetSandboxEnabled :: Bool
