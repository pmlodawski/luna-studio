module JS.NodeGraph where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle

import           JS.Node
import           Object.Object
import           Object.Port
import           Object.Node
import           Object.UITypes

import           Data.JSString (pack)
import           Data.JSString.Text  (lazyTextToJSString)

import           JavaScript.Array    (JSArray)
import qualified JavaScript.Array    as JSArray

import GHCJS.Marshal

-- unselectAllNodes :: IO ()
-- unselectAllNodes =
--     getNodes >>= mapM_ setUnselected

-- selectNodes :: NodeIdCollection -> IO ()
-- selectNodes nodeIds =
--     mapM_ setNodeSelected nodeIds
--
-- unfocusAllNodes :: IO ()
-- unfocusAllNodes =
--     getNodes >>= mapM_ setUnfocused

-- moveNode :: Node -> IO ()
-- moveNode node = do
--     let (Vector2 wx wy) = node ^. nodePos
--     nodeRef  <- getNode $ node ^. nodeId
--     moveTo nodeRef wx wy

-- updateLabel :: Node -> IO ()
-- updateLabel node = do
--     nodeRef  <- getNode $ node ^. nodeId
--     showLabel nodeRef (lazyTextToJSString $ node ^. expression)

addInputPort :: NodeId -> WidgetId -> PortId -> ColorNum -> Angle -> IO ()
addInputPort nodeId widgetId portId color angle = putStrLn "addInputPort"
-- do
--     nodeRef <- getNode nodeId
--     let portIdNum = portIdToNum portId
--     addInputPortJS nodeRef widgetId portIdNum color angle

addOutputPort :: NodeId -> WidgetId ->  PortId -> ColorNum -> Angle -> IO ()
addOutputPort nodeId widgetId portId color angle = putStrLn "addOutputPort"
-- do
--     nodeRef <- getNode nodeId
--     let portIdNum = portIdToNum portId
--     addOutputPortJS nodeRef widgetId portIdNum color angle

setInputPortAngle :: NodeId -> PortId -> Angle -> IO ()
setInputPortAngle nodeId portId angle = putStrLn "setInputPortAngle"
-- do
--     nodeRef <- getNode nodeId
--     let portIdNum = portIdToNum portId
--     setInputPortAngleJS nodeRef portIdNum angle

setOutputPortAngle :: NodeId -> PortId -> Angle -> IO ()
setOutputPortAngle nodeId portId angle = putStrLn "setOutputPortAngle"
    -- nodeRef <- getNode nodeId
    -- let portIdNum = portIdToNum portId
    -- setOutputPortAngleJS nodeRef portIdNum angle

-- setComputedValue :: NodeId -> String -> IO ()
-- setComputedValue nodeId value = do
--     nodeRef <- getNode nodeId
--     setValue nodeRef $ pack value

displayNodeVector :: NodeId -> [Float] -> IO ()
displayNodeVector nodeId vals = putStrLn "displayNodeVector" -- do
--     nodeRef <- getNode nodeId
--     valsRef <- mapM toJSVal_pure vals
--     displayVector nodeRef $ JSArray.fromList valsRef

-- createNodeAt :: Int -> Vector2 Double -> Text -> Int -> IO ()
-- createNodeAt nodeId (Vector2 px py) expr wid = do
--     newNodeAt nodeId px py expr wid
--
