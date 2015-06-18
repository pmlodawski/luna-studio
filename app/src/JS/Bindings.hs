{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module JS.Bindings where
 
import Data.IORef         ( IORef, newIORef )
import Data.Monoid        ( (<>) )
import GHCJS.DOM          ( currentDocument )
import GHCJS.DOM.Document ( documentGetBody )
import GHCJS.DOM.Element  ( Element )
import GHCJS.DOM.Node     ( nodeAppendChild )
import GHCJS.Types        ( JSRef, JSArray, JSString )
import JS.Converters      ( getFromJSRef, getTuple4FromJSArray )

-----------------------------------------------------------------------
-- Enough to interact with virtual-dom
-----------------------------------------------------------------------
data VNode
data VElement
data Diff

-- === foreigns ===

foreign import javascript unsafe "virtualDom.h($1, [$2])"
  mkNode :: JSString -> JSString -> IO (JSRef VNode)

foreign import javascript unsafe "virtualDom.create($1)"
  createElement :: JSRef VNode -> IO Element

foreign import javascript unsafe "virtualDom.diff($1, $2)"
  diff :: JSRef VNode -> JSRef VNode -> IO (JSRef Diff)

foreign import javascript unsafe "virtualDom.patch($1, $2)"
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

-- foreign import javascript unsafe "isNodeSelected($1)"
--   isNodeSelectedJSRef :: Int -> IO (JSRef Bool) 

-- foreign import javascript unsafe "toggleNodeSelection($1)"
--   toggleNodeSelection :: Int -> IO () 

foreign import javascript unsafe "app.unselectAllNodes()"
  unselectAllNodes :: IO ()

foreign import javascript unsafe "app.unfocusNode()"
  unfocusNode :: IO ()

-- foreign import javascript unsafe "dragNodes($1, $2, $3)"
--   dragNodes :: JSArray Int -> Int -> Int -> IO ()

foreign import javascript unsafe "app.getNodeAt($1, $2)"
  getNodeAtJSArray :: Int -> Int -> IO (JSArray Int)

getNodeAt :: Int -> Int -> IO (Int, Bool, Int, Int)
getNodeAt x y = fmap (\(id, sel, x, y) -> (id, sel >= 1, x, y)) . getTuple4FromJSArray $ getNodeAtJSArray x y
-- getNodeAt = (getTuple4FromJSArray .) . getNodeIdWithOffsetOnPositionJSArray

-- isNodeSelected :: Int -> IO Bool
-- isNodeSelected = getFromJSRef . isNodeSelectedJSRef

--------------------------------------------------------------------------------
-- An element in the DOM that we can render virtualdom elements to
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
