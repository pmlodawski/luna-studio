{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module JS.Node where

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


foreign import javascript unsafe "app.moveToTopZ($1)"
    moveToTopZ :: Int -> IO ()

foreign import javascript unsafe "app.newNodeAt($1, $2, $3, $4, $5)"
    newNodeAtJS :: Int -> Double -> Double -> JSString -> Int -> IO ()

newNodeAt :: Int -> Double -> Double -> Text -> Int -> IO ()
newNodeAt nodeId px py expr widgetId = newNodeAtJS nodeId px py (lazyTextToJSString expr) widgetId

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

foreign import javascript unsafe "console.log($1); $1.uniforms.selected.value = $2"
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
