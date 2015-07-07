{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module JS.Bindings where

import Data.IORef         ( IORef, newIORef )
import Data.Maybe         ( maybeToList )
import Data.Monoid        ( (<>) )
import Data.Dynamic

import Control.Monad.Trans ( liftIO )

import GHCJS.Foreign
import GHCJS.DOM.EventM
import GHCJS.DOM          ( currentDocument )
import GHCJS.DOM.Document ( documentGetBody )
import GHCJS.DOM.Element  ( Element, IsElement )
import GHCJS.DOM.Node     ( nodeAppendChild )
import GHCJS.Types        ( JSRef, JSArray, JSString )
import GHCJS.DOM.Types    ( UIEvent, IsDOMWindow, IsUIEvent, unUIEvent, toUIEvent )
import JS.Converters

import Object.Object      ( Point(..), Object(..) )
import Object.Node        ( Node(..) )
import Utils.PrettyPrinter

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

-----------------------------------------------------------------------
-- Enough to interact with virtual-dom
-----------------------------------------------------------------------
data VNode
data VElement
data Diff

-- === foreigns ===

foreign import javascript unsafe "window.virtualDom.h($1, [$2])"
    mkNode :: JSString -> JSString -> IO (JSRef VNode)

foreign import javascript unsafe "window.virtualDom.create($1)"
    createElement :: JSRef VNode -> IO Element

foreign import javascript unsafe "window.virtualDom.diff($1, $2)"
    diff :: JSRef VNode -> JSRef VNode -> IO (JSRef Diff)

foreign import javascript unsafe "window.virtualDom.patch($1, $2)"
    patch :: Element -> JSRef Diff -> IO ()


foreign import javascript unsafe "window.innerWidth"
    innerWidth :: IO Int

foreign import javascript unsafe "window.innerHeight"
    innerHeight :: IO Int



foreign import javascript unsafe "app.init()"
    init :: IO ()

foreign import javascript unsafe "app.render()"
    render :: IO ()

foreign import javascript unsafe "app.create($1)"
    create :: Int -> IO ()

foreign import javascript unsafe "app.moveToTopZ($1)"
    moveToTopZ :: Int -> IO ()

foreign import javascript unsafe "app.getNodeAt($1, $2)"
    getNodeAtJSArray :: Int -> Int -> IO (JSArray Int)

foreign import javascript unsafe "app.newNodeAt($1, $2, $3)"
    newNodeAt :: Int -> Int -> Int -> IO ()

foreign import javascript unsafe "app.removeNode($1)"
    removeNode :: Int -> IO ()




data FunctionNode








foreign import javascript unsafe "common.commonUniforms.screenSize.value.x"
    getScreenSizeX :: IO Double

foreign import javascript unsafe "common.commonUniforms.screenSize.value.y"
    getScreenSizeY :: IO Double

foreign import javascript unsafe "common.commonUniforms.camPan.value.x"
    getCamPanX :: IO Double

foreign import javascript unsafe "common.commonUniforms.camPan.value.y"
    getCamPanY :: IO Double

foreign import javascript unsafe "common.commonUniforms.camFactor.value"
    getCamFactor :: IO Double


foreign import javascript unsafe "common.commonUniforms.samFactor.value = $1"
    setCamFactor :: Double -> IO ()







foreign import javascript unsafe "app.getNode($1)"
    getNode :: Int -> IO (JSRef FunctionNode)

foreign import javascript unsafe "app.getNodes()"
    getNodesJSArray :: IO (JSArray FunctionNode)

getNodes :: IO [JSRef FunctionNode]
getNodes = getNodesJSArray >>= fromArray

foreign import javascript unsafe "$1.moveTo($2, $3)"
    moveTo :: JSRef FunctionNode -> Double -> Double -> IO ()

foreign import javascript unsafe "$1.label($2)"
    showLabel :: JSRef FunctionNode -> JSString -> IO ()



foreign import javascript unsafe "$1.uniforms.selected.value"
    getSelectionValue :: JSRef FunctionNode -> IO Int

foreign import javascript unsafe "$1.uniforms.selected.value = $2"
    setSelectionValue :: JSRef FunctionNode -> Int -> IO ()

setUnselected, setSelected, setFocused, setUnfocused :: JSRef FunctionNode -> IO ()
setUnselected = flip setSelectionValue 0
setSelected   = flip setSelectionValue 1
setFocused    = flip setSelectionValue 2
setUnfocused node = do
    focused <- isFocused node
    if focused then setSelected node
               else return ()

hasSelectionValue :: JSRef FunctionNode -> Int -> IO Bool
hasSelectionValue node value = getSelectionValue node >>= return . (== value)

isUnselected, isSelected, isFocused :: JSRef FunctionNode -> IO Bool
isUnselected = flip hasSelectionValue 0
isSelected   = flip hasSelectionValue 1
isFocused    = flip hasSelectionValue 2

foreign import javascript unsafe "$1.renderExamplePlot()"
    renderExamplePlot :: JSRef FunctionNode -> IO ()



getNodeFromTuple4 :: (Int, Int, Int, Int) -> Maybe Node
getNodeFromTuple4 (nodeId, sel, x, y)
    | nodeId >= 0 = Just $ Node nodeId (sel >= 1) (Point x y)
    | otherwise = Nothing

(.:)  :: (x -> y) -> (a -> b -> x) -> a -> b -> y
(.:)   = (.) . (.)

getNodeAt :: Int -> Int -> IO (Maybe Node)
getNodeAt = (fmap getNodeFromTuple4 . getTuple4FromJSArray) .: getNodeAtJSArray

-- temporary implementation
getObjectsAt :: Int -> Int -> IO [Object Dynamic]
getObjectsAt x y = getNodeAt x y >>= return . maybeToList . fmap (Object . toDyn)



logAs :: PrettyPrinter a => String -> a -> IO ()
logAs title a = putStrLn $ title <> (display a)

--

data VNodePresentation = VNodePresentation (IORef (JSRef VNode)) Element

data HTML = Text String deriving (Show)


newTopLevelContainer :: IO VNodePresentation
newTopLevelContainer = do
    initialVNode <- mkNode "div" ""
    currentVNode <- newIORef initialVNode
    el <- createElement initialVNode
    Just doc <- currentDocument
    Just body <- documentGetBody doc
    nodeAppendChild body (Just el)
    return (VNodePresentation currentVNode el)
