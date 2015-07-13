module JS.Appjs where

import           Control.Monad
import           Control.Lens
import           Data.Monoid

import           JS.Bindings
import           JS.Converters
import           Object.Node
import           Utils.Vector
import           Utils.PrettyPrinter


logAs :: PrettyPrinter a => String -> a -> IO ()
logAs title a = putStrLn $ title <> (display a)


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

selectNodes :: NodeIdCollection -> IO ()
selectNodes nodeIds =
    mapM_ setNodeSelected nodeIds

unselectNodes :: NodeIdCollection -> IO ()
unselectNodes nodeIds =
    mapM_ setNodeUnselected nodeIds

unfocusAllNodes :: IO ()
unfocusAllNodes =
    getNodes >>= mapM_ setUnfocused


moveNode :: Node -> IO ()
moveNode node = do
    let (Vector2 wx wy) = node ^.position
    nodeRef <- getNode $ node ^. ident
    moveTo nodeRef wx wy

dragNode :: Vector2 Double -> Node -> IO ()
dragNode delta node = do
    let (Vector2 wx wy) = node ^.position + delta
    nodeRef <- getNode $ node ^. ident
    moveTo nodeRef wx wy

displaySelectBox :: Vector2 Double -> Vector2 Double -> IO ()
displaySelectBox a b = displaySelectBoxJS mx my w h where
    (Vector2 mx my) = a
    (Vector2 w  h)  = b - a
