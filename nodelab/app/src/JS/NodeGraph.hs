module JS.NodeGraph where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle

import           JS.Bindings
import           Object.Object
import           Object.Node


logAs :: PrettyPrinter a => String -> a -> IO ()
logAs title a = putStrLn $ title <> (display a)


setNodeUnselected :: NodeId -> IO ()
setNodeUnselected nodeId =
    getNode nodeId >>= setUnselected

setNodeSelected :: NodeId -> IO ()
setNodeSelected nodeId = do
    getNode nodeId >>= setSelected
    moveToTopZ nodeId

setNodeFocused :: NodeId -> IO ()
setNodeFocused nodeId = do
    unfocusAllNodes
    getNode nodeId >>= setFocused
    moveToTopZ nodeId

setNodeUnfocused :: NodeId -> IO ()
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
    let (Vector2 wx wy) = node ^. nodePos
    nodeRef  <- getNode $ node ^. nodeId
    moveTo nodeRef wx wy

dragNode :: Vector2 Double -> Node -> IO ()
dragNode delta node = do
    let (Vector2 wx wy) = node ^. nodePos + delta
    nodeRef  <- getNode $ node ^. nodeId
    moveTo nodeRef wx wy

displaySelectionBox :: Vector2 Double -> Vector2 Double -> IO ()
displaySelectionBox (Vector2 x0 y0) (Vector2 x1 y1) = displaySelectionBoxJS x0 y0 x1 y1

addInputPort :: NodeId -> PortId -> Angle -> IO ()
addInputPort nodeId portId angle = do
    nodeRef <- getNode nodeId
    addInputPortJS nodeRef portId angle

addOutputPort :: NodeId -> PortId -> Angle -> IO ()
addOutputPort nodeId portId angle = do
    nodeRef <- getNode nodeId
    addOutputPortJS nodeRef portId angle

setInputPortAngle :: NodeId -> PortId -> Angle -> IO ()
setInputPortAngle nodeId portId angle = do
    nodeRef <- getNode nodeId
    setInputPortAngleJS nodeRef portId angle

setOutputPortAngle :: NodeId -> PortId -> Angle -> IO ()
setOutputPortAngle nodeId portId angle = do
    nodeRef <- getNode nodeId
    setOutputPortAngleJS nodeRef portId angle


createNodeAt :: Int -> Vector2 Double -> Text -> Int -> IO ()
createNodeAt nodeId (Vector2 px py) expr wid = do
    newNodeAt nodeId px py expr wid
    putStrLn $ "nodeId " <> show nodeId

