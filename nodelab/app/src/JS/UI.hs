{-# LANGUAGE JavaScriptFFI #-}

module JS.UI where

import           Utils.PreludePlus
import           Utils.Vector
import           GHCJS.Foreign
import           GHCJS.Types (JSString)

foreign import javascript safe "window.innerWidth"
    innerWidth :: IO Int

foreign import javascript safe "window.innerHeight"
    innerHeight :: IO Int

foreign import javascript safe "app.initializeGl()"
    initializeGl :: IO ()

foreign import javascript safe "app.render()"
    render :: IO ()

foreign import javascript safe "window.dispatchEvent(new Event('resize'))"
    triggerWindowResize :: IO ()

foreign import javascript safe "app.shouldRender()"
    shouldRender :: IO ()

foreign import javascript safe "$('#htmlcanvas-pan').css({cursor: $1})"
    setCursor :: JSString -> IO ()

foreign import javascript safe "app.displayRejectedMessage()"
    displayRejectedMessage :: IO ()

foreign import javascript safe "app.displayConnectionClosedMessage()"
    displayConnectionClosedMessage :: IO ()

