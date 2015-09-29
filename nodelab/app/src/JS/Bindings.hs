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

foreign import javascript unsafe "app.moveToTopZ($1)"
    moveToTopZ :: Int -> IO ()

foreign import javascript unsafe "app.newNodeAt($1, $2, $3, $4, $5)"
    newNodeAtJS :: Int -> Double -> Double -> JSString -> Int -> IO ()

newNodeAt :: Int -> Double -> Double -> Text -> Int -> IO ()
newNodeAt nodeId px py expr widgetId = newNodeAtJS nodeId px py (lazyTextToJSString expr) widgetId

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


data NodeJS

foreign import javascript unsafe "app.getNode($1)"
    getNode :: Int -> IO (JSRef NodeJS)

foreign import javascript unsafe "app.getNodes()"
    getNodesJSArray :: IO JSArray

getNodes :: IO [JSRef NodeJS]
getNodes = getNodesJSArray >>= return . JSArray.toList

foreign import javascript unsafe "$1.moveTo($2, $3)"
    moveTo :: JSRef NodeJS -> Double -> Double -> IO ()

foreign import javascript unsafe "$1.label($2)"
    showLabel :: JSRef NodeJS -> JSString -> IO ()

foreign import javascript unsafe "$1.setValue($2)"
    setValue :: JSRef NodeJS -> JSString -> IO ()

foreign import javascript unsafe "$1.addInputPort($2, $3, $4)"
    addInputPortJS :: JSRef NodeJS -> Int -> Int -> Double -> IO ()

foreign import javascript unsafe "$1.addOutputPort($2, $3, $4)"
    addOutputPortJS :: JSRef NodeJS -> Int -> Int -> Double -> IO ()

foreign import javascript unsafe "$1.setInputPortAngle($2, $3)"
    setInputPortAngleJS :: JSRef NodeJS -> Int -> Double -> IO ()

foreign import javascript unsafe "$1.setOutputPortAngle($2, $3)"
    setOutputPortAngleJS :: JSRef NodeJS -> Int -> Double -> IO ()

foreign import javascript unsafe "$1.setInputPortColor($2, $3, $4, $5)"
    setInputPortColor :: JSRef NodeJS -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.setOutputPortColor($2, $3, $4, $5)"
    setOutputPortColor :: JSRef NodeJS -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.toggleExpandState()"
    toggleExpandState :: JSRef NodeJS -> IO ()

foreign import javascript unsafe "$1.uniforms.selected.value"
    getSelectionValue :: JSRef NodeJS -> IO Int

foreign import javascript unsafe "$1.uniforms.selected.value = $2"
    setSelectionValue :: JSRef NodeJS -> Int -> IO ()

foreign import javascript unsafe "$1.htmlContainer"
    getHTMLContainer :: JSRef NodeJS -> IO Element

setUnselected, setSelected, setFocused, setUnfocused :: JSRef NodeJS -> IO ()
setUnselected = flip setSelectionValue 0
setSelected   = flip setSelectionValue 1
setFocused    = flip setSelectionValue 2
setUnfocused node = do
    focused <- isFocused node
    if focused then setSelected node
               else return ()

hasSelectionValue :: JSRef NodeJS -> Int -> IO Bool
hasSelectionValue node value = getSelectionValue node >>= return . (== value)

isUnselected, isSelected, isFocused :: JSRef NodeJS -> IO Bool
isUnselected = flip hasSelectionValue 0
isSelected   = flip hasSelectionValue 1
isFocused    = flip hasSelectionValue 2



foreign import javascript unsafe "$1.renderExamplePlot()"
    renderExamplePlot :: JSRef NodeJS -> IO ()



foreign import javascript unsafe "config.logging"
    isLoggerEnabled :: IO Bool

foreign import javascript unsafe "config.backend"
    isBackendEnabled :: IO Bool

foreign import javascript unsafe "config.backendAddress"
    getBackendAddress' :: IO JSString

getBackendAddress :: IO String
getBackendAddress  = unpack <$> getBackendAddress'

foreign import javascript unsafe "app.displaySelectionBox($1, $2, $3, $4)"
    displaySelectionBoxJS :: Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "app.hideSelectionBox()"
    hideSelectionBox :: IO ()


foreign import javascript unsafe "app.displayCurrentConnection($1, $2, $3, $4)"
    displayCurrentConnection :: Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "app.removeCurrentConnection()"
    removeCurrentConnection :: IO ()


foreign import javascript unsafe "app.createConnection($1, $2)"
    createConnection :: Int -> Int -> IO ()

foreign import javascript unsafe "app.updateConnection($1, $2, $3, $4, $5, $6)"
    updateConnection :: Int -> Bool -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "app.removeWidget($1)"
    removeWidget :: Int -> IO ()

foreign import javascript unsafe "window.dispatchEvent(new Event('resize'))"
    triggerWindowResize :: IO ()

foreign import javascript unsafe "app.shouldRender()"
    shouldRender :: IO ()


foreign import javascript unsafe "$('#htmlcanvas-pan').css({cursor: $1})"
    setCursor :: JSString -> IO ()

foreign import javascript unsafe "require('exampleData')" getExampleData :: IO JSArray

foreign import javascript unsafe "app.displayRejectedMessage()"
    displayRejectedMessage :: IO ()
