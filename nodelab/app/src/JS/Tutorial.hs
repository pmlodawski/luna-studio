{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.Tutorial
    ( shouldRunTutorial
    , showStep
    ) where

import           Utils.PreludePlus
import           GHCJS.Types (JSString)
import           Data.JSString.Text  (lazyTextToJSString)

foreign import javascript safe "localStorage.getItem('tutorial') == '1'" shouldRunTutorial :: IO Bool
foreign import javascript safe "console.log('show step', $1)" showStep :: Int -> IO ()
