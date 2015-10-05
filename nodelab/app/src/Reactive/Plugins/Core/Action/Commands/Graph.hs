module Reactive.Plugins.Core.Action.Commands.Graph where


import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle

import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Map as Map
import           Object.Object
import           Object.Node
import           Object.Port
import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Connection as UIConnection

import qualified JS.Bindings    as UI
import qualified JS.Connection  as UI
import qualified JS.NodeGraph   as UI


import           Reactive.Plugins.Core.Action.State.Graph
import qualified Reactive.Plugins.Core.Action.State.Connect        as Connect
import qualified Reactive.Plugins.Core.Action.State.Graph          as Graph
import qualified Reactive.Plugins.Core.Action.State.UIRegistry     as UIRegistry
import qualified Reactive.Plugins.Core.Action.State.Camera         as Camera
import qualified Reactive.Plugins.Core.Action.State.Global         as Global
import           Reactive.Plugins.Core.Action.Commands.Command     (Command, command, pureCommand, ioCommand)
import qualified Control.Monad.State                               as MState

updateConnections :: Command Global.State ()
updateConnections = pureCommand $ \state -> let
    newState           = state & Global.uiRegistry .~ newRegistry
    oldRegistry        = state ^. Global.uiRegistry
    updateWidgets      = forM_ allConnections updateWidget
    updateWidget file  = case connectionUI of
            Just conn -> UIRegistry.updateM (file ^. objectId) conn
            Nothing   -> return ()
        where
        connectionId   = file ^. widget . UIConnection.connectionId
        connection     = IntMap.lookup connectionId connectionsMap
        connectionUI   = (getConnectionLine nodesMap) <$> connection
    (_, (newRegistry, _)) = MState.runState updateWidgets (oldRegistry, return ())
    allConnections     = UIRegistry.lookupAll oldRegistry :: [WidgetFile Global.State UIConnection.Connection]
    nodesMap           = Graph.getNodesMap       $ state ^. Global.graph
    connectionsMap     = Graph.getConnectionsMap $ state ^. Global.graph
    in newState

updateConnectionsUI :: Command Global.State ()
updateConnectionsUI = ioCommand $ \state -> let
    oldRegistry        = state ^. Global.uiRegistry
    updateWidgets      = forM_ allConnections updateWidget
    updateWidget file  = UI.updateConnection (file ^. objectId) visible fromX fromY toX toY where
        UIConnection.Connection connId visible (Vector2 fromX fromY) (Vector2 toX toY) = file ^. widget
    allConnections     = UIRegistry.lookupAll oldRegistry :: [WidgetFile Global.State UIConnection.Connection]
    in updateWidgets


createConnectionWidget :: WidgetId -> UIConnection.Connection -> ColorNum -> IO ()
createConnectionWidget widgetId connection color = UI.createConnection widgetId (connection ^. UIConnection.connectionId) color

updateConnectionWidget :: WidgetId -> UIConnection.Connection -> IO ()
updateConnectionWidget widgetId connection = UI.updateConnection widgetId visible fromX fromY toX toY where
    UIConnection.Connection _ visible (Vector2 fromX fromY) (Vector2 toX toY) = connection

getNodePos :: NodesMap -> NodeId -> Vector2 Double
getNodePos nodesMap nodeId = node ^. nodePos where
    node = IntMap.findWithDefault (error $ "Node " <> show nodeId <> " not found") nodeId nodesMap

getPortByRef :: PortRef -> NodesMap -> Port
getPortByRef portRef nodesMap = port where
    nodeId = portRef ^. refPortNodeId
    node   = IntMap.findWithDefault (error $ "Node " <> show nodeId <> " not found") nodeId nodesMap
    ports' = case portRef ^. refPortType of
        InputPort  -> node ^. ports . inputPorts
        OutputPort -> node ^. ports . outputPorts
    port   = fromMaybe (error $ "Port " <> show portRef <> "  not found") $ find (\p -> p ^. portId == portRef ^. refPortId) ports' -- FIXME unsafe

getPortAngle :: PortRef -> NodesMap -> Double
getPortAngle portRef nodesMap = getPortByRef portRef nodesMap ^. angle

tryGetSrcDst :: PortRef -> PortRef -> Maybe (PortRef, PortRef)
tryGetSrcDst portRef1 portRef2 = case (portRef1 ^. refPortType, portRef2 ^. refPortType) of
    (OutputPort, InputPort)  -> Just (portRef1, portRef2)
    (InputPort,  OutputPort) -> Just (portRef2, portRef1)
    _                        -> Nothing

connectNodes :: PortRef -> PortRef -> Command Global.State ()
connectNodes src dst = command $ \state -> let
    oldGraph                     = state ^. Global.graph
    oldRegistry                  = state ^. Global.uiRegistry
    newState                     = state  & Global.graph      .~ newGraph
                                          & Global.uiRegistry .~ newRegistry
    valueType                    = view portValueType $ getPort oldGraph src
    uiUpdate                     = forM_ file $ \f -> createConnectionWidget (f ^. objectId) (f ^. widget) (colorVT valueType)
    newNodesMap = oldNodesMap
    oldNodesMap                  = Graph.getNodesMap oldGraph
    updSourceGraph               = Graph.updateNodes newNodesMap oldGraph
    (connId, newGraph)           = Graph.addConnection src dst updSourceGraph
    (file, newRegistry)          = case connId of
        Just connId             -> (Just widget, newRegistry) where
            (widget, newRegistry)= UIRegistry.register UIRegistry.sceneGraphId uiConnection def oldRegistry
            uiConnection         = getConnectionLine newNodesMap $ Graph.Connection connId src dst
        Nothing                 -> (Nothing, oldRegistry)
    in (uiUpdate, newState)

getConnectionLine :: NodesMap -> Connection -> UIConnection.Connection
getConnectionLine nodesMap (Connection lineId srcPortRef dstPortRef) = UIConnection.Connection lineId visible srcWs dstWs
    where
    srcNWs@(Vector2 xSrcN ySrcN) = getNodePos nodesMap $ srcPortRef ^. refPortNodeId
    dstNWs@(Vector2 xDstN yDstN) = getNodePos nodesMap $ dstPortRef ^. refPortNodeId
    outerPos                     = portOuterBorder + distFromPort
    angleSrc                     = getPortAngle srcPortRef nodesMap
    angleDst                     = getPortAngle dstPortRef nodesMap
    srcWs                        = Vector2 (xSrcN + outerPos * cos angleSrc) (ySrcN + outerPos * sin angleSrc)
    dstWs                        = Vector2 (xDstN + outerPos * cos angleDst) (yDstN + outerPos * sin angleDst)
    delta                        = dstNWs - srcNWs
    visible                      = lengthSquared delta > 4 * portOuterBorderSquared


sortAndGroup assocs = Map.fromListWith (++) [(k, [v]) | (k, v) <- assocs]

connectionVector :: NodesMap -> PortRef -> PortRef -> Vector2 Double
connectionVector nodesMap src dst  = explode (dstNWs - srcNWs) where
    srcNWs@(Vector2 xSrcN ySrcN) = getNodePos nodesMap $ src ^. refPortNodeId
    dstNWs@(Vector2 xDstN yDstN) = getNodePos nodesMap $ dst ^. refPortNodeId


updatePortAngles :: Command Global.State ()
updatePortAngles = pureCommand $ \state -> let
    newState              = state & Global.graph %~ updateNodes newNodes
    oldNodes              = getNodesMap $ state ^. Global.graph
    newNodes              = foldl processPort oldNodes angles
    processPort nodes (portRef, angle) = IntMap.insert nodeId newNode nodes where
        nodeId  = portRef ^.refPortNodeId
        node    = nodes IntMap.! nodeId
        newNode = updatePortAngle portRef angle node

    connectionTuples conn = [ (conn ^. source,      conn ^. destination)
                            , (conn ^. destination, conn ^. source     ) ]
    connections           = IntMap.elems $ Graph.getConnectionsMap $ state ^. Global.graph
    portsMap              = sortAndGroup . concat $ connectionTuples <$> connections
    calculateAngle portRef targets = toAngle . foldl (+) (Vector2 0.0 0.0) $ connectionVector oldNodes portRef <$> targets
    angles                = Map.toList $ Map.mapWithKey calculateAngle portsMap
    in newState


updatePortAnglesUI :: Command Global.State ()
updatePortAnglesUI = ioCommand $ \state -> let
    processNode node = mapM_ (processPort InputPort ) (getPorts InputPort  node)
                    >> mapM_ (processPort OutputPort) (getPorts OutputPort node)
        where
        processPort :: PortType -> Port -> IO ()
        processPort tpe port = setAngle tpe (node ^. nodeId) (port ^. portId) (port ^. angle)
    nodes   = getNodes $ state ^. Global.graph
    in mapM_ processNode nodes


displayDragLine :: NodesMap -> Angle -> Vector2 Double -> Connect.Connecting -> IO ()
displayDragLine nodesMap angle ptWs@(Vector2 cx cy) connecting = do
    let portRef              = connecting ^. Connect.sourcePortRef
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


moveNodesUI :: NodesMap -> IO ()
moveNodesUI nodesMap = mapM_ UI.moveNode $ IntMap.elems nodesMap
                  -- >> performGC
