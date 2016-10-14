{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.UI
    ( displayConnectionClosedMessage
    , calculateTextWidth
    , shouldRender
    , initializeGl
    , render
    , triggerWindowResize
    ) where

import           Utils.PreludePlus
import           GHCJS.Types (JSString)
import           Data.JSString.Text  (lazyTextToJSString)

foreign import javascript safe "app.initializeGl()"
    initializeGl :: IO ()

foreign import javascript safe "app.render()"
    render :: IO ()

foreign import javascript safe "window.dispatchEvent(new Event('resize'))"
    triggerWindowResize :: IO ()

foreign import javascript safe "app.shouldRender()"
    shouldRender :: IO ()

foreign import javascript safe "app.displayConnectionClosedMessage()"
    displayConnectionClosedMessage :: IO ()

foreign import javascript safe "breadcrumb.calculateTextWidth($1)" calculateTextWidth' :: JSString -> Int

calculateTextWidth :: Text -> Int
calculateTextWidth = calculateTextWidth' . lazyTextToJSString
