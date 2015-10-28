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
import           Data.JSString       (unpack, pack)


foreign import javascript unsafe "window.innerWidth"
    innerWidth :: IO Int

foreign import javascript unsafe "window.innerHeight"
    innerHeight :: IO Int

foreign import javascript unsafe "app.initializeGl()"
    initializeGl :: IO ()

foreign import javascript unsafe "app.render()"
    render :: IO ()

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

foreign import javascript unsafe "app.writeToTerminal($1)"
    writeToTerminal' :: JSString -> IO ()

writeToTerminal :: String -> IO ()
writeToTerminal = writeToTerminal' . pack
