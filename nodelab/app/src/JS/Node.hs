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
import           GHCJS.Types         (JSVal, JSString)
import           GHCJS.DOM.Types     (UIEvent, IsUIEvent, unUIEvent, toUIEvent)
import           JavaScript.Array    (JSArray)
import qualified JavaScript.Array    as JSArray
import           Data.JSString.Text  (lazyTextToJSString)
import           Data.JSString       (unpack)

newtype Node = Node { unNode :: JSVal }

foreign import javascript unsafe "app.moveToTopZ($1)"
    moveToTopZ :: Int -> IO ()

foreign import javascript unsafe "app.getNode($1)"
    getNode :: Int -> IO Node

foreign import javascript unsafe "app.getNodes()"
    getNodesJSArray :: IO JSArray

getNodes :: IO [Node]
getNodes = do
    nodes <- getNodesJSArray
    return $ Node <$> JSArray.toList nodes

foreign import javascript unsafe "$1.moveTo($2, $3)"
    moveTo :: Node -> Double -> Double -> IO ()

foreign import javascript unsafe "$1.addInputPort($2, $3, $4, $5)"
    addInputPortJS :: Node -> Int -> Int -> Int -> Double -> IO ()

foreign import javascript unsafe "$1.addOutputPort($2, $3, $4, $5)"
    addOutputPortJS :: Node -> Int -> Int -> Int -> Double -> IO ()

foreign import javascript unsafe "$1.setInputPortAngle($2, $3)"
    setInputPortAngleJS :: Node -> Int -> Double -> IO ()

foreign import javascript unsafe "$1.setOutputPortAngle($2, $3)"
    setOutputPortAngleJS :: Node -> Int -> Double -> IO ()

foreign import javascript unsafe "$1.setInputPortColor($2, $3, $4, $5)"
    setInputPortColor :: Node -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.setOutputPortColor($2, $3, $4, $5)"
    setOutputPortColor :: Node -> Int -> Int -> Int -> Int -> IO ()

foreign import javascript unsafe "$1.uniforms.selected.value"
    getSelectionValue :: Node -> IO Int

foreign import javascript unsafe "$1.uniforms.selected.value = $2"
    setSelectionValue :: Node -> Int -> IO ()


foreign import javascript unsafe "$1.renderExamplePlot()"
    renderExamplePlot :: Node -> IO ()

foreign import javascript unsafe "app.createPendingNode($1, $2, $3, $4)"
    createPendingNode' :: Int -> JSString -> Double -> Double -> IO ()

createPendingNode :: Int -> Text -> Vector2 Double -> IO ()
createPendingNode oid expr (Vector2 x y) = createPendingNode' oid (lazyTextToJSString expr) x y

foreign import javascript unsafe "$1.displayVector($2)" displayVector :: Node -> JSArray -> IO ()
