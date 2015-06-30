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

foreign import javascript unsafe "app.setNodeFocused($1)"
  setNodeFocused :: Int -> IO ()

foreign import javascript unsafe "app.setNodeSelected($1)"
  setNodeSelected :: Int -> IO ()

foreign import javascript unsafe "app.setNodeUnselected($1)"
  setNodeUnselected :: Int -> IO ()

foreign import javascript unsafe "app.unselectAllNodes()"
  unselectAllNodes :: IO ()

foreign import javascript unsafe "app.getNodeAt($1, $2)"
  getNodeAtJSArray :: Int -> Int -> IO (JSArray Int)


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



data VNodePresentation = VNodePresentation (IORef (JSRef VNode)) Element

data HTML = Text String
  deriving (Show)

logAs :: Show a => String -> a -> IO ()
logAs title a =
  putStrLn $ title <> (show a)

newTopLevelContainer :: IO VNodePresentation
newTopLevelContainer = do
  initialVNode <- mkNode "div" ""
  currentVNode <- newIORef initialVNode
  el <- createElement initialVNode
  Just doc <- currentDocument
  Just body <- documentGetBody doc
  nodeAppendChild body (Just el)
  return (VNodePresentation currentVNode el)
