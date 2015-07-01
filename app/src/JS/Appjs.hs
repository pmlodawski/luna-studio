module JS.Appjs where

import Control.Monad

import JS.Bindings
import JS.Converters

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

unfocusAllNodes :: IO ()
unfocusAllNodes =
    getNodes >>= mapM_ setUnfocused





-- dragNode :: Int -> Int -> Int -> IO ()
-- dragNode nodeId x y = when nodeId >= 0 do
--     node <- getNode nodeId
--     (wx, wy) <- utils.screenToWorkspace(x,y)
--     moveTo node wx wy

