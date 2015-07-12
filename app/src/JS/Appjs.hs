module JS.Appjs where

import           Control.Monad
import           Control.Lens

import           JS.Bindings
import           JS.Converters
import           JS.Utils
import           Object.Node
import           Utils.Vector

setNodeUnselected :: Int -> IO ()
setNodeUnselected nodeId =
    getNode nodeId >>= setUnselected

setNodeSelected :: Int -> IO ()
setNodeSelected nodeId = do
    getNode nodeId >>= setSelected
    moveToTopZ nodeId

setNodeFocused :: Int -> IO ()
setNodeFocused nodeId = do
    unfocusAllNodes
    getNode nodeId >>= setFocused
    moveToTopZ nodeId

setNodeUnfocused :: Int -> IO ()
setNodeUnfocused nodeId =
    getNode nodeId >>= setUnfocused

unselectAllNodes :: IO ()
unselectAllNodes =
    getNodes >>= mapM_ setUnselected

selectAllNodes :: IO ()
selectAllNodes =
    getNodes >>= mapM_ setSelected

unfocusAllNodes :: IO ()
unfocusAllNodes =
    getNodes >>= mapM_ setUnfocused


dragNode :: Camera -> Node -> IO ()
dragNode camera node = do
    let (Vector2 wx wy) = screenToWorkspace camera $ node ^. position
    nodeRef <- getNode $ node ^. ident
    moveTo nodeRef wx wy

