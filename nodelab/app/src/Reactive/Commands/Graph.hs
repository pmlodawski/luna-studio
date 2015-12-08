module Reactive.Commands.Graph where


import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle
import qualified Utils.MockTC as MockTC
import qualified Utils.Nodes  as NodeUtils

import           Data.IntMap.Lazy (IntMap(..))
import qualified Data.IntMap.Lazy as IntMap
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ord (comparing)
import           Object.Object
import           Object.Node
import           Object.Port
import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Node       as Model
import qualified Object.Widget.Connection as ConnectionModel
import qualified Object.Widget.Port       as PortModel

import qualified JS.NodeGraph   as UI

import           Reactive.State.Graph
import qualified Reactive.State.Connect        as Connect
import qualified Reactive.State.Graph          as Graph
import qualified Reactive.State.UIRegistry     as UIRegistry
import qualified Reactive.State.Camera         as Camera
import qualified Reactive.State.Global         as Global
import           Reactive.Commands.Command     (Command, command, pureCommand, ioCommand)
import qualified Reactive.Commands.UIRegistry  as UICmd
import           Reactive.State.UIRegistry     (sceneGraphId)

import           Control.Monad.State

import qualified BatchConnector.Commands                           as BatchCmd

import qualified UI.Widget.Node as UINode
import qualified UI.Widget.Port as UIPort
import qualified UI.Widget.Connection as UIConnection
import qualified UI.Generic as UIGeneric
import           Reactive.State.Camera (Camera, screenToWorkspace)
import           UI.Instances ()
import           UI.Widget.Connection.Instances ()

updateConnNodes :: [NodeId] -> Command Global.State ()
updateConnNodes nodeIds = pureCommand $ \state -> let
    noConns nid   = not $ hasConnections nid (state ^. Global.graph)
    changeFun     = nodeIds & filter noConns
                            & map   (IntMap.adjust MockTC.revertNode)
                            & foldl (.) id
    nodesMap      = changeFun . Graph.getNodesMap . view Global.graph $ state
    newState      = state &  Global.graph %~ Graph.updateNodes nodesMap
    in newState

lookupAllConnections :: Command UIRegistry.State [WidgetFile ConnectionModel.Connection]
lookupAllConnections = gets UIRegistry.lookupAll

updateConnections :: Command Global.State ()
updateConnections = do
    allConnections <- zoom Global.uiRegistry lookupAllConnections
    nodePositions  <- zoom Global.uiRegistry nodePositionMap
    portAngles     <- zoom Global.uiRegistry portRefToAngleMap
    portTypes      <- portTypes
    connectionsMap <- uses Global.graph getConnectionsMap

    forM_ allConnections $ \widgetFile -> do
        let connectionId   = widgetFile ^. widget . ConnectionModel.connectionId
            connection     = IntMap.lookup connectionId connectionsMap
            connectionLine = (\conn -> getConnectionLine nodePositions portAngles portTypes (conn ^. source) (conn ^. destination)) <$> connection
        forM_ connectionLine $ \(posFrom, posTo, visible, color) -> do
            zoom Global.uiRegistry $ do
                UICmd.update (widgetFile ^. objectId) $ (ConnectionModel.from    .~ posFrom)
                                                      . (ConnectionModel.to      .~ posTo)
                                                      . (ConnectionModel.visible .~ visible)
                                                      . (ConnectionModel.color   .~ color)

getConnectionLine :: IntMap (Vector2 Double) -> Map PortRef Double -> Map PortRef ValueType -> PortRef  -> PortRef -> (Vector2 Double, Vector2 Double, Bool, Int)
getConnectionLine nodePos portAngles portTypes srcPortRef dstPortRef = (srcWs, dstWs, visible, color) where
    srcNWs@(Vector2 xSrcN ySrcN) = nodePos IntMap.! (srcPortRef ^. refPortNodeId)
    dstNWs@(Vector2 xDstN yDstN) = nodePos IntMap.! (dstPortRef ^. refPortNodeId)
    outerPos                     = portOuterBorder + distFromPort
    angleSrc                     = Map.findWithDefault missingPortPos srcPortRef portAngles
    angleDst                     = Map.findWithDefault missingPortPos dstPortRef portAngles
    srcWs                        = Vector2 (xSrcN + outerPos * cos angleSrc) (ySrcN + outerPos * sin angleSrc)
    dstWs                        = Vector2 (xDstN + outerPos * cos angleDst) (yDstN + outerPos * sin angleDst)
    delta                        = dstNWs - srcNWs
    visible                      = lengthSquared delta > 4 * portOuterBorderSquared
    color                        = fromMaybe missingPortColor $ colorVT <$> portTypes ^? ix srcPortRef
    missingPortPos               = -pi / 2.0
    missingPortColor             = 13

connectNodes :: PortRef -> PortRef -> Command Global.State ()
connectNodes src dst = do
    batchConnectNodes src dst
    localConnectNodes src dst

batchConnectNodes :: PortRef -> PortRef -> Command Global.State ()
batchConnectNodes src dst = ioCommand $ \state -> let
    workspace = state ^. Global.workspace
    in BatchCmd.connectNodes workspace src dst

addConnectionM :: PortRef -> PortRef -> Command Global.State (Maybe ConnectionId)
addConnectionM src dst = do
    graph          <- use Global.graph
    let (connId, newGraph) = Graph.addConnection src dst graph
    Global.graph .= newGraph
    return connId

localConnectNodes :: PortRef -> PortRef -> Command Global.State ()
localConnectNodes src dst = do
    connectionId <- addConnectionM src dst
    forM_ connectionId $ \connectionId -> do
        nodePositions  <- zoom Global.uiRegistry nodePositionMap
        portAngles     <- zoom Global.uiRegistry portRefToAngleMap
        zoom Global.uiRegistry $ UICmd.register sceneGraphId (ConnectionModel.Connection connectionId False def def def) def
        return ()
    updatePortAngles
    updateConnections

sortAndGroup assocs = Map.fromListWith (++) [(k, [v]) | (k, v) <- assocs]

portRefToWidgetMap :: Command UIRegistry.State (Map PortRef WidgetId)
portRefToWidgetMap = do
    ports <- allPorts
    return $ Map.fromList $ (\file -> (file ^. widget . PortModel.portRef, file ^. objectId)) <$> ports

portRefToAngleMap :: Command UIRegistry.State (Map PortRef Double)
portRefToAngleMap = do
    ports <- allPorts
    return $ Map.fromList $ (\file -> (file ^. widget . PortModel.portRef, file ^. widget . PortModel.angle)) <$> ports

nodePositionMap :: Command UIRegistry.State (IntMap (Vector2 Double))
nodePositionMap = do
    nodes <- allNodes
    return $ IntMap.fromList $ (\file -> (file ^. widget . Model.nodeId, file ^. widget . widgetPosition)) <$> nodes

connectionVector :: IntMap (Vector2 Double) -> PortRef -> PortRef -> Vector2 Double
connectionVector map src dst = (dstPos - srcPos) where
    srcPos = map IntMap.! (src ^. refPortNodeId)
    dstPos = map IntMap.! (dst ^. refPortNodeId)

defaultAngles :: Command Global.State (Map PortRef (Vector2 Double))
defaultAngles = do
    nodes <- use $ Global.graph . Graph.nodes

    let inputAngles = concat $ calculateInputAngles <$> nodes where
            calculateInputAngles node = portAngle <$> node ^. ports . inputPorts where
                inputPortNum = length $ node ^. ports . inputPorts
                portAngle port = (portRef, portDefaultAngle InputPort inputPortNum $ port ^. portId) where
                    portRef = PortRef (node ^. nodeId) InputPort (port ^. portId)

    let outputAngles = concat $ calculateOutputAngles <$> nodes where
            calculateOutputAngles node = portAngle <$> node ^. ports . outputPorts where
                outputPortNum = length $ node ^. ports . outputPorts
                portAngle port = (portRef, portDefaultAngle OutputPort outputPortNum $ port ^. portId) where
                    portRef = PortRef (node ^. nodeId) OutputPort (port ^. portId)

    return $ Map.fromList $ inputAngles ++ outputAngles

portTypes :: Command Global.State (Map PortRef ValueType)
portTypes = do
    nodes <- use $ Global.graph . Graph.nodes

    return $ Map.fromList $ concat $ getPorts <$> nodes where
            getPorts node = ins ++ outs where
                ins   = (getPort InputPort)  <$> node ^. ports . inputPorts
                outs  = (getPort OutputPort) <$> node ^. ports . outputPorts
                getPort tpe port = (portRef, port ^. portValueType) where
                    portRef = PortRef (node ^. nodeId) tpe (port ^. portId)

updatePortAngles :: Command Global.State ()
updatePortAngles = do
    connectionsMap <- use $ Global.graph . Graph.connectionsMap
    nodePositions  <- zoom Global.uiRegistry nodePositionMap

    let connectionTuples conn          = [ (conn ^. source,      conn ^. destination)
                                         , (conn ^. destination, conn ^. source     ) ]
        connections                    = sortAndGroup . concat $ connectionTuples <$> connectionsMap

    let calculateAngle portRef targets = sum $ connectionVector nodePositions portRef <$> targets
        connectedAngles                = Map.mapWithKey calculateAngle connections

    defAngles <- defaultAngles

    let angles = Map.union connectedAngles defAngles
    portWidgets <- zoom Global.uiRegistry portRefToWidgetMap

    forM_ (Map.toList angles) $ \(portRef, vector) -> do
        let widgetId = portWidgets ^? ix portRef
        forM_ widgetId $ \widgetId -> zoom Global.uiRegistry $ UICmd.update widgetId (PortModel.angleVector .~ vector)

allNodes :: Command UIRegistry.State [WidgetFile Model.Node]
allNodes = UIRegistry.lookupAllM

allPorts :: Command UIRegistry.State [WidgetFile PortModel.Port]
allPorts = UIRegistry.lookupAllM

allConnections :: Command UIRegistry.State [WidgetFile ConnectionModel.Connection]
allConnections = UIRegistry.lookupAllM

nodeIdToWidgetId :: NodeId -> Command UIRegistry.State (Maybe WidgetId)
nodeIdToWidgetId nodeId = do
    files <- allNodes
    let matching = find (\file -> (file ^. widget . Model.nodeId) == nodeId) files
    return (view objectId <$> matching)

connectionIdToWidgetId :: ConnectionId -> Command UIRegistry.State (Maybe WidgetId)
connectionIdToWidgetId connectionId = do
    files <- allConnections
    let matching = find (\file -> (file ^. widget . ConnectionModel.connectionId) == connectionId) files
    return (view objectId <$> matching)

-- TODO: Clever algorithm taking radius into account
getNodesInRect :: Vector2 Int -> Vector2 Int -> Command Global.State [WidgetId]
getNodesInRect (Vector2 x1 y1) (Vector2 x2 y2) = do
    widgets <- zoom Global.uiRegistry allNodes
    camera  <- use $ Global.camera . Camera.camera
    let leftBottom = screenToWorkspace camera (Vector2 (min x1 x2) (min y1 y2)) - Vector2 radiusShadow radiusShadow
        rightTop   = screenToWorkspace camera (Vector2 (max x1 x2) (max y1 y2)) + Vector2 radiusShadow radiusShadow
        isNodeInBounds file = let pos = file ^. widget . widgetPosition in
                              leftBottom ^. x <= pos ^. x && pos ^. x <= rightTop ^. x &&
                              leftBottom ^. y <= pos ^. y && pos ^. y <= rightTop ^. y
        nodesInBounds = filter isNodeInBounds widgets
    return $ (view objectId) <$> nodesInBounds

focusNode :: WidgetId -> Command UIRegistry.State ()
focusNode id = do
    nodes <- allNodes
    let sortedNodes = sortBy (comparing $ negate . (view $ widget . Model.zPos)) nodes
        sortedIds   = (view objectId) <$> sortedNodes
        newOrder    = id : (delete id sortedIds)
    forM_ (zip newOrder [1..]) $ \(id, ix) -> do
        let newZPos = negate $ (fromIntegral ix) / 100.0
        UICmd.update id $ Model.zPos .~ newZPos


