{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module JS.Bindings where

import Data.IORef         ( IORef, newIORef )
import Data.Maybe         ( maybeToList )
import Data.Monoid        ( (<>) )
import Data.Dynamic

import GHCJS.DOM          ( currentDocument )
import GHCJS.DOM.Document ( documentGetBody )
import GHCJS.DOM.Element  ( Element )
import GHCJS.DOM.Node     ( nodeAppendChild )
import GHCJS.Types        ( JSRef, JSArray, JSString )
import JS.Converters      ( getFromJSRef, getTuple4FromJSArray )

import Object.Object      ( Point(..), Object(..) )
import Object.Node        ( Node(..) )
import Utils.PrettyPrinter
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



foreign import javascript unsafe "app.init()"
    init :: IO ()

foreign import javascript unsafe "app.render()"
    render :: IO ()

foreign import javascript unsafe "app.create($1)"
    create :: Int -> IO ()

foreign import javascript unsafe "app.dragNode($1, $2, $3)"
    dragNode :: Int -> Int -> Int -> IO ()

foreign import javascript unsafe "app.moveToTopZ($1)"
    moveToTopZ :: Int -> IO ()

-- foreign import javascript unsafe "app.unselectAllNodes()"
--     unselectAllNodes :: IO ()

foreign import javascript unsafe "app.unfocusAllNodes()"
    unfocusAllNodes :: IO ()

foreign import javascript unsafe "app.getNodeAt($1, $2)"
    getNodeAtJSArray :: Int -> Int -> IO (JSArray Int)


data FunctionNode



foreign import javascript unsafe "app.getNode($1)"
    getNode :: Int -> IO (JSRef FunctionNode)

foreign import javascript unsafe "app.getNodes()"
    getNodes :: IO (JSArray FunctionNode)


foreign import javascript unsafe "$1.label($2)"
    showLabel :: JSRef FunctionNode -> JSString -> IO ()


foreign import javascript unsafe "$2.uniforms.selected.value = $1"
    setSelectionValue :: Int -> JSRef FunctionNode -> IO ()

setUnselected = setSelectionValue 0
setSelected   = setSelectionValue 1
setFocused    = setSelectionValue 2

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
