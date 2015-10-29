{-# LANGUAGE JavaScriptFFI #-}

module JS.UI where

import           Utils.PreludePlus
import           Utils.Vector
import           GHCJS.Foreign
import           GHCJS.Types (JSString)

foreign import javascript unsafe "window.innerWidth"
    innerWidth :: IO Int

foreign import javascript unsafe "window.innerHeight"
    innerHeight :: IO Int

foreign import javascript unsafe "app.initializeGl()"
    initializeGl :: IO ()

foreign import javascript unsafe "app.render()"
    render :: IO ()

foreign import javascript unsafe "app.updateMouse($1, $2)"
    updateMouse' :: Double -> Double -> IO ()

updateMouse :: Vector2 Double -> IO ()
updateMouse (Vector2 x y) = updateMouse' x y

foreign import javascript unsafe "window.dispatchEvent(new Event('resize'))"
    triggerWindowResize :: IO ()

foreign import javascript unsafe "app.shouldRender()"
    shouldRender :: IO ()

foreign import javascript unsafe "$('#htmlcanvas-pan').css({cursor: $1})"
    setCursor :: JSString -> IO ()

foreign import javascript unsafe "app.displayRejectedMessage()"
    displayRejectedMessage :: IO ()

