{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module JS.Config where

import           Utils.PreludePlus

import           Data.Dynamic
import           Data.Text.Lazy      ( Text )

import           GHCJS.Foreign
import           GHCJS.Types         ( JSRef, JSString )
import           Data.JSString       (unpack)


foreign import javascript safe "config.fontSize" fontSize :: Double

foreign import javascript safe "config.backendAddress"
    getBackendAddress' :: IO JSString

getBackendAddress :: IO String
getBackendAddress  = unpack <$> getBackendAddress'
