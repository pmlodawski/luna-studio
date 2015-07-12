module JS.Appjs where

import Control.Monad

import           JS.Bindings
import           JS.Converters
import           JS.Utils
import           Object.Object  hiding ( setSelected )

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


dragNode :: Int -> Int -> Int -> IO ()
dragNode nodeId x y = when (nodeId >= 0) $ do
    node      <- getNode nodeId  -- TODO: take all values from state
    width     <- innerWidth
    height    <- innerHeight
    camFactor <- getCamFactor
    camPan    <- getCamPan
    let (Vector2 wx wy) = screenToWorkspace (Vector2 width height) camFactor camPan (Vector2 x y)
    moveTo node wx wy

