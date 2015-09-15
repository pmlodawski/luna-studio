module Reactive.Plugins.Core.Action.Executors.Graph where


import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle

import qualified Data.IntMap.Lazy as IntMap

import           Object.Object
import           Object.Node

import qualified JS.Bindings    as UI
import qualified JS.NodeGraph   as UI


import           Reactive.Plugins.Core.Action.State.Graph
import qualified Reactive.Plugins.Core.Action.State.Connect   as Connect


displayConnections :: NodesMap -> ConnectionsMap -> IO ()
displayConnections nodesMap connectionsMap = mapM_ (displayConnectionLine nodesMap) $ IntMap.elems connectionsMap

getNodePos :: NodesMap -> NodeId -> Vector2 Double
getNodePos nodesMap nodeId = node ^. nodePos where
    node = IntMap.findWithDefault (error $ "Node " <> show nodeId <> " not found") nodeId nodesMap

-- displayConnectionLine :: NodesMap -> (Int, (PortRef, PortRef)) -> IO ()
displayConnectionLine :: NodesMap -> Connection -> IO ()
displayConnectionLine nodesMap (Connection lineId srcPortRef dstPortRef) = do
    let srcNWs@(Vector2 xSrcN ySrcN) = getNodePos nodesMap $ srcPortRef ^. refPortNodeId
        dstNWs@(Vector2 xDstN yDstN) = getNodePos nodesMap $ dstPortRef ^. refPortNodeId
        outerPos                     = portOuterBorder + distFromPort
        angleSrc                     = calcAngle dstNWs srcNWs
        angleDst                     = calcAngle srcNWs dstNWs
        srcWs@(Vector2 xSrc ySrc)    = Vector2 (xSrcN + outerPos * cos angleSrc) (ySrcN + outerPos * sin angleSrc)
        dstWs@(Vector2 xDst yDst)    = Vector2 (xDstN + outerPos * cos angleDst) (yDstN + outerPos * sin angleDst)
        delta                        = dstNWs - srcNWs
        draw                         = lengthSquared delta > 4 * portOuterBorderSquared
    setAnglePortRef angleSrc srcPortRef
    setAnglePortRef angleDst dstPortRef
    if draw then UI.displayConnection lineId xSrc ySrc xDst yDst
            else UI.removeConnection lineId


displayDragLine :: NodesMap -> Angle -> Vector2 Double -> Connect.Connecting -> IO ()
displayDragLine nodesMap angle ptWs@(Vector2 cx cy) connecting = do
    let portRef              = connecting ^. Connect.sourcePort
        ndWs@(Vector2 nx ny) = getNodePos nodesMap $ portRef ^. refPortNodeId
        outerPos             = portOuterBorder + distFromPort
        sy                   = ny + outerPos * sin angle
        sx                   = nx + outerPos * cos angle
        (Vector2 vx vy)      = ptWs - ndWs
        draw                 = vx * vx + vy * vy > portOuterBorderSquared
    setAnglePortRef angle portRef
    if draw then UI.displayCurrentConnection sx sy cx cy
            else UI.removeCurrentConnection


-- displayDragLine :: NodesMap -> PortRef -> Vector2 Double -> IO ()
-- displayDragLine nodesMap portRef ptWs@(Vector2 cx cy) = do
--     let angle = calcAngle ptWs ndWs
--     -- let portRef              = connecting ^. Connect.sourcePort
--         ndWs@(Vector2 nx ny) = getNodePos nodesMap $ portRef ^. refPortNodeId
--         outerPos             = portOuterBorder + distFromPort
--         sy                   = ny + outerPos * sin angle
--         sx                   = nx + outerPos * cos angle
--         (Vector2 vx vy)      = ptWs - ndWs
--         draw                 = vx * vx + vy * vy > portOuterBorderSquared
--     setAnglePortRef angle portRef
--     if draw then UI.displayCurrentConnection sx sy cx cy
--             else UI.removeCurrentConnection

setAnglePortRef :: Angle -> PortRef -> IO ()
setAnglePortRef refAngle portRef = setAngle (portRef ^. refPortType)
                                            (portRef ^. refPortNodeId)
                                            (portRef ^. refPortId)
                                            refAngle

setAngle :: PortType -> NodeId -> PortId -> Angle -> IO ()
setAngle  InputPort = UI.setInputPortAngle
setAngle OutputPort = UI.setOutputPortAngle
