module JS.NodeGraph where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle

import           JS.Bindings
import           JS.Node
import           Object.Object
import           Object.Port
import           Object.Node
import           Object.UITypes

import           Data.JSString (pack)
import           Data.JSString.Text  (lazyTextToJSString)

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

updateLabel :: Node -> IO ()
updateLabel node = do
    nodeRef  <- getNode $ node ^. nodeId
    showLabel nodeRef (lazyTextToJSString $ node ^. expression)

displaySelectionBox :: Vector2 Double -> Vector2 Double -> IO ()
displaySelectionBox (Vector2 x0 y0) (Vector2 x1 y1) = displaySelectionBoxJS x0 y0 x1 y1

addInputPort :: NodeId -> WidgetId -> PortId -> ColorNum -> Angle -> IO ()
addInputPort nodeId widgetId portId color angle = do
    putStrLn $ "input color " <> show color
    nodeRef <- getNode nodeId
    let portIdNum = portIdToNum portId
    addInputPortJS nodeRef widgetId portIdNum color angle

addOutputPort :: NodeId -> WidgetId ->  PortId -> ColorNum -> Angle -> IO ()
addOutputPort nodeId widgetId portId color angle = do
    putStrLn $ "output color " <> show color
    nodeRef <- getNode nodeId
    let portIdNum = portIdToNum portId
    addOutputPortJS nodeRef widgetId portIdNum color angle

setInputPortAngle :: NodeId -> PortId -> Angle -> IO ()
setInputPortAngle nodeId portId angle = do
    nodeRef <- getNode nodeId
    let portIdNum = portIdToNum portId
    setInputPortAngleJS nodeRef portIdNum angle

setOutputPortAngle :: NodeId -> PortId -> Angle -> IO ()
setOutputPortAngle nodeId portId angle = do
    nodeRef <- getNode nodeId
    let portIdNum = portIdToNum portId
    setOutputPortAngleJS nodeRef portIdNum angle

setComputedValue :: NodeId -> String -> IO ()
setComputedValue nodeId value = do
    nodeRef <- getNode nodeId
    setValue nodeRef $ pack value

createNodeAt :: Int -> Vector2 Double -> Text -> Int -> IO ()
createNodeAt nodeId (Vector2 px py) expr wid = do
    newNodeAt nodeId px py expr wid

