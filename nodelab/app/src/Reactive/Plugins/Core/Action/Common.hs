module Reactive.Plugins.Core.Action.Common where


import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle

import           Object.Object
import           Object.Node

import qualified JS.Bindings    as UI
import qualified JS.NodeGraph   as UI


import qualified Reactive.Plugins.Core.Action.State.Graph     as Graph
import qualified Reactive.Plugins.Core.Action.State.Connect   as Connect


displayConnections :: NodeCollection -> Graph.ConnectionsCollections -> IO ()
displayConnections nodes connections = mapM_ (displayConnectionLine nodes) $ zip [0..] connections

getNodePos :: NodeCollection -> NodeId -> Vector2 Double
getNodePos nodes findNodeId = case find (\node -> (node ^. nodeId) == findNodeId) nodes of
    Just node -> node ^. nodePos
    Nothing   -> error $ "Node " <> show findNodeId <> " not found"

displayConnectionLine :: NodeCollection -> (Int, (PortRef, PortRef)) -> IO ()
displayConnectionLine nodes (lineId, (srcPortRef, dstPortRef)) = do
    let srcNWs@(Vector2 xSrcN ySrcN) = getNodePos nodes $ srcPortRef ^. refPortNodeId
        dstNWs@(Vector2 xDstN yDstN) = getNodePos nodes $ dstPortRef ^. refPortNodeId
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


displayDragLine :: NodeCollection -> Angle -> Vector2 Double -> Connect.Connecting -> IO ()
displayDragLine nodes angle ptWs@(Vector2 cx cy) connecting = do
    let portRef              = connecting ^. Connect.sourcePort
        ndWs@(Vector2 nx ny) = getNodePos nodes $ portRef ^. refPortNodeId
        outerPos             = portOuterBorder + distFromPort
        sy                   = ny + outerPos * sin angle
        sx                   = nx + outerPos * cos angle
        (Vector2 vx vy)      = ptWs - ndWs
        draw                 = vx * vx + vy * vy > portOuterBorderSquared
    setAnglePortRef angle portRef
    if draw then UI.displayCurrentConnection sx sy cx cy
            else UI.removeCurrentConnection


-- displayDragLine :: NodeCollection -> PortRef -> Vector2 Double -> IO ()
-- displayDragLine nodes portRef ptWs@(Vector2 cx cy) = do
--     let angle = calcAngle ptWs ndWs
--     -- let portRef              = connecting ^. Connect.sourcePort
--         ndWs@(Vector2 nx ny) = getNodePos nodes $ portRef ^. refPortNodeId
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
