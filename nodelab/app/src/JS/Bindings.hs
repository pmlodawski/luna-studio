{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module JS.Bindings where

import           Utils.PreludePlus
import           Utils.Vector

import           Data.Dynamic
import           Data.Text.Lazy      (Text)

import           GHCJS.Foreign
import           GHCJS.DOM.EventM
import           GHCJS.DOM           (currentDocument)
import           GHCJS.DOM.Element   (Element, IsElement)
import           GHCJS.Types         (JSRef, JSString)
import           GHCJS.DOM.Types     (UIEvent, IsUIEvent, unUIEvent, toUIEvent)
import           JavaScript.Array    (JSArray)
import qualified JavaScript.Array    as JSArray
import           Data.JSString.Text  (lazyTextToJSString)
import           Data.JSString       (unpack)


foreign import javascript unsafe "window.innerWidth"
    innerWidth :: IO Int

foreign import javascript unsafe "window.innerHeight"
    innerHeight :: IO Int

foreign import javascript unsafe "app.initializeGl()"
    initializeGl :: IO ()

foreign import javascript unsafe "app.render()"
    render :: IO ()


foreign import javascript unsafe "common.camera.updateProjectionMatrix()"
    updateProjectionMatrix :: IO ()

foreign import javascript unsafe "common.cameraHUD.updateProjectionMatrix()"
    updateHUDProjectionMatrix :: IO ()

foreign import javascript unsafe "app.updateScreenSize($1, $2)"
    updateScreenSize :: Int -> Int -> IO ()

foreign import javascript unsafe "app.updateHtmCanvasPanPos($1, $2, $3)"
    updateHtmCanvasPanPos :: Double -> Double -> Double -> IO ()

foreign import javascript unsafe "app.updateCamera($1, $2, $3, $4, $5)"
    updateCamera :: Double -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "app.updateCameraHUD($1, $2, $3, $4)"
    updateCameraHUD :: Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "app.updateMouse($1, $2)"
    updateMouse :: Double -> Double -> IO ()


foreign import javascript unsafe "app.displaySelectionBox($1, $2, $3, $4)"
    displaySelectionBoxJS :: Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "app.hideSelectionBox()"
    hideSelectionBox :: IO ()



foreign import javascript unsafe "window.dispatchEvent(new Event('resize'))"
    triggerWindowResize :: IO ()

foreign import javascript unsafe "app.shouldRender()"
    shouldRender :: IO ()


foreign import javascript unsafe "$('#htmlcanvas-pan').css({cursor: $1})"
    setCursor :: JSString -> IO ()

foreign import javascript unsafe "require('exampleData')" getExampleData :: IO JSArray

foreign import javascript unsafe "app.displayRejectedMessage()"
    displayRejectedMessage :: IO ()

foreign import javascript unsafe "common.term.write($1)"
    writeToTerminal' :: JSString -> IO ()

writeToTerminal :: Text -> IO ()
writeToTerminal = writeToTerminal' . lazyTextToJSString
