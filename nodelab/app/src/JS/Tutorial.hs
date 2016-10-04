{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.Tutorial
    ( shouldRunTutorial
    , showStep
    , closeOnboarding
    ) where

import           Utils.PreludePlus
import           GHCJS.Types (JSString)
import           Data.JSString.Text  (lazyTextToJSString)

foreign import javascript safe "localStorage.getItem('onboarding') != '1'" shouldRunTutorial :: IO Bool
foreign import javascript safe "app.showOnboarding($1)" showStep :: Int -> IO ()
foreign import javascript safe "app.closeOnboarding()" closeOnboarding :: IO ()
