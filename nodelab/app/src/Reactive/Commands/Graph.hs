module Reactive.Commands.Graph where


import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle
import qualified Utils.Nodes                   as NodeUtils

import           Data.IntMap.Lazy              (IntMap(..))
import qualified Data.IntMap.Lazy              as IntMap
import           Data.Map                      (Map)
import           Data.Hashable                 (hash)
import qualified Data.Map                      as Map
import           Data.Ord                      (comparing)
import           Data.Fixed                    (mod')
import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Node            as Model
import qualified Object.Widget.Connection      as ConnectionModel
import qualified Object.Widget.Port            as PortModel

import           Reactive.State.Graph
import qualified Reactive.State.Connect        as Connect
import qualified Reactive.State.Graph          as Graph
import qualified Reactive.State.UIRegistry     as UIRegistry
import qualified Reactive.State.Camera         as Camera
import qualified Reactive.State.Global         as Global
import           Reactive.State.Global         (inRegistry)
import           Reactive.Commands.Command     (Command, command, pureCommand, ioCommand)
import qualified Reactive.Commands.UIRegistry  as UICmd
import           Reactive.State.UIRegistry     (sceneGraphId)

import           Control.Monad.State

import qualified BatchConnector.Commands       as BatchCmd

import qualified UI.Widget.Node                as UINode
import qualified UI.Widget.Port                as UIPort
import qualified UI.Widget.Connection          as UIConnection
import qualified UI.Generic                    as UIGeneric
import           Reactive.State.Camera         (Camera, screenToWorkspace)
import           UI.Instances                  ()
import           Empire.API.Data.Node          (Node, NodeId)
import qualified Empire.API.Data.Node          as Node
import           Empire.API.Data.NodeMeta      (NodeMeta)
import qualified Empire.API.Data.NodeMeta      as NodeMeta
import           Empire.API.Data.PortRef       (AnyPortRef(..), InPortRef(..), OutPortRef(..))
import qualified Empire.API.Data.PortRef       as PortRef
import           Empire.API.Data.Connection    (Connection, ConnectionId)
import qualified Empire.API.Data.Connection    as Connection
import           Empire.API.Data.ValueType     (ValueType (..))
import           Empire.API.Data.Port          (PortId(..))
import qualified Empire.API.Data.Port          as Port
import qualified Empire.API.Data.ValueType     as ValueType


vtToColor AnyType = 0
vtToColor (TypeIdent tn) = case tn of
     "Int"      -> 1
     "Bool"     -> 2
     "Double"   -> 3
     "String"   -> 4
     "[Int]"    -> 5
     "[Double]" -> 6
     "[Bool]"   -> 7
     "[String]" -> 8
     _          -> 9 + hash tn `mod` 8
colorPort port = vtToColor $ port ^. Port.valueType


updateConnNodes :: [NodeId] -> Command Global.State ()
updateConnNodes nodeIds = pureCommand $ \state -> let
    noConns nid   = not $ hasConnections nid (state ^. Global.graph)
    changeFun     = nodeIds & filter noConns
                            & map   (IntMap.adjust id)
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
            connection     = Map.lookup connectionId connectionsMap
            connectionLine = (\conn -> getConnectionLine nodePositions portAngles portTypes (conn ^. Connection.src) (conn ^. Connection.dst)) <$> connection
        forM_ connectionLine $ \(posFrom, posTo, visible, color) -> do
            zoom Global.uiRegistry $ do
                UICmd.update (widgetFile ^. objectId) $ (ConnectionModel.from    .~ posFrom)
                                                      . (ConnectionModel.to      .~ posTo)
                                                      . (ConnectionModel.visible .~ visible)
                                                      . (ConnectionModel.color   .~ color)

outerPos (InPortRef _ Port.Self) = 0.0
outerPos _ = 22.0

getConnectionLine :: IntMap (Vector2 Double) -> Map AnyPortRef (Double, Int) -> Map AnyPortRef ValueType -> OutPortRef  -> InPortRef -> (Vector2 Double, Vector2 Double, Bool, Int)
getConnectionLine nodePos portAngles portTypes srcPortRef dstPortRef = (srcWs, dstWs, visible, color) where
    srcNWs@(Vector2 xSrcN ySrcN) = nodePos IntMap.! (srcPortRef ^. PortRef.srcNodeId)
    dstNWs@(Vector2 xDstN yDstN) = nodePos IntMap.! (dstPortRef ^. PortRef.dstNodeId)
    outerSrcPos                  = 20.0
    outerDstPos                  = outerPos dstPortRef
    (angleSrcPort, srcPortCount) = Map.findWithDefault missingPortPos (OutPortRef' srcPortRef) portAngles
    (angleDstPort, dstPortCount) = Map.findWithDefault missingPortPos (InPortRef' dstPortRef) portAngles
    angleSrc                     = boundedAngle angleSrcPort srcPortCount srcNWs dstNWs
    angleDst                     = boundedAngle angleDstPort dstPortCount dstNWs srcNWs
    srcWs                        = Vector2 (xSrcN + outerSrcPos * cos angleSrc) (ySrcN + outerSrcPos * sin angleSrc)
    dstWs                        = Vector2 (xDstN + outerDstPos * cos angleDst) (yDstN + outerDstPos * sin angleDst)
    delta                        = dstNWs - srcNWs
    visible                      = lengthSquared delta > 4 * 25
    color                        = fromMaybe missingPortColor $ vtToColor <$> portTypes ^? ix (OutPortRef' srcPortRef)
    missingPortPos               = (-pi / 2.0, 1)
    missingPortColor             = 13


batchConnectNodes :: OutPortRef -> InPortRef -> Command Global.State ()
batchConnectNodes src dst = ioCommand $ \state -> let
    workspace = state ^. Global.workspace
    in BatchCmd.connectNodes workspace src dst

addConnectionM :: OutPortRef -> InPortRef -> Command Global.State (Maybe ConnectionId)
addConnectionM src dst = do
    graph          <- use Global.graph
    let (connId, newGraph) = Graph.addConnection src dst graph
    Global.graph .= newGraph
    return connId

withArrow :: Getter InPortRef Bool
withArrow = to $ \ref -> case ref of
    InPortRef _ Port.Self -> False
    _                     -> True

localConnectNodes :: OutPortRef -> InPortRef -> Command Global.State ()
localConnectNodes src dst = do
    connectionId <- addConnectionM src dst
    forM_ connectionId $ \connectionId -> do
        nodePositions  <- zoom Global.uiRegistry nodePositionMap
        portAngles     <- zoom Global.uiRegistry portRefToAngleMap
        zoom Global.uiRegistry $ UICmd.register_ sceneGraphId (ConnectionModel.Connection connectionId True def def (dst ^. withArrow) def) def
    updateConnections

sortAndGroup assocs = Map.fromListWith (++) [(k, [v]) | (k, v) <- assocs]

portRefToWidgetMap :: Command UIRegistry.State (Map AnyPortRef WidgetId)
portRefToWidgetMap = do
    ports <- allPorts
    return $ Map.fromList $ (\file -> (file ^. widget . PortModel.portRef, file ^. objectId)) <$> ports

portRefToAngleMap :: Command UIRegistry.State (Map AnyPortRef (Double, Int))
portRefToAngleMap = do
    ports <- allPorts
    return $ Map.fromList $ (\file -> (file ^. widget . PortModel.portRef, (file ^. widget . PortModel.angle, file ^. widget . PortModel.portCount))) <$> ports

nodePositionMap :: Command UIRegistry.State (IntMap (Vector2 Double))
nodePositionMap = do
    nodes <- allNodes
    return $ IntMap.fromList $ (\file -> (file ^. widget . Model.nodeId, file ^. widget . widgetPosition)) <$> nodes

connectionVector :: IntMap (Vector2 Double) -> AnyPortRef -> AnyPortRef -> Vector2 Double
connectionVector map src dst = dstPos - srcPos where
    srcPos = map IntMap.! (src ^. PortRef.nodeId)
    dstPos = map IntMap.! (dst ^. PortRef.nodeId)

angleToDimVec :: Double -> Vector2 Double
angleToDimVec angle = (/ 10.0) <$> Vector2 (cos angle) (-sin angle)

portDefaultAngle :: Int -> PortId -> Vector2 Double
portDefaultAngle numPorts (OutPortId _) = angleToDimVec $ 0.0
portDefaultAngle numPorts (InPortId (Port.Arg portNum)) = angleToDimVec angle where
    angle = delta * (fromIntegral portNum) + delta / 2.0 + pi / 2.0
    delta = pi / (fromIntegral numPorts)
portDefaultAngle numPorts (InPortId (Port.Self)) = angleToDimVec 0.0

-- defaultAngles :: Command Global.State (Map AnyPortRef (Vector2 Double))
-- defaultAngles = do
--     nodes <- use $ Global.graph . Graph.nodes
--
--     let angles = calculateAngles <$> nodes where
--             calculateAngles node = portAngle <$> (zip [1..] $ Map.keys $ node ^. Node.ports) where
--                 portAngle (portIx, portId) = (PortRef.toAnyPortRef nodeId portId, portDefaultAngle portNum portId portIx) where
--                 nodeId = node ^. Node.nodeId
--                 portNum = length $ node ^. Node.ports
--
--     return $ Map.fromList $ concat $ angles

portTypes :: Command Global.State (Map AnyPortRef ValueType)
portTypes = do
    nodes <- use $ Global.graph . Graph.nodes

    return $ Map.fromList $ concat $ getPortsValueType <$> nodes where
            getPortsValueType node = portVT <$> (Map.toList $ node ^. Node.ports) where
                portVT (portId, port) = (PortRef.toAnyPortRef nodeId portId, port ^. Port.valueType) where
                nodeId = node ^. Node.nodeId

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

portRefToWidgetId :: AnyPortRef -> Command UIRegistry.State (Maybe WidgetId)
portRefToWidgetId portRef = do
    files <- allPorts
    let matching = find (\file -> (file ^. widget . PortModel.portRef) == portRef) files
    return (view objectId <$> matching)

focusNode :: WidgetId -> Command UIRegistry.State ()
focusNode id = do
    nodes <- allNodes
    let sortedNodes = sortBy (comparing $ negate . (view $ widget . Model.zPos)) nodes
        sortedIds   = (view objectId) <$> sortedNodes
        newOrder    = id : (delete id sortedIds)
    forM_ (zip newOrder [1..]) $ \(id, ix) -> do
        let newZPos = negate $ (fromIntegral ix) / 100.0
        UICmd.update id $ Model.zPos .~ newZPos

----

updateNodeMeta :: NodeId -> NodeMeta -> Command Global.State ()
updateNodeMeta nodeId meta = do
    Global.graph . Graph.nodesMap . ix nodeId . Node.nodeMeta .= meta

    inRegistry $ do
        widgetId <- nodeIdToWidgetId nodeId

        forM_ widgetId $ flip UICmd.move (fromTuple $  meta ^. NodeMeta.position)

    updateConnections


renameNode :: NodeId -> Text -> Command Global.State ()
renameNode nodeId name = do
    Global.graph . Graph.nodesMap . ix nodeId . Node.name .= name

    inRegistry $ do
        widgetId <- nodeIdToWidgetId nodeId

        forM_ widgetId $ flip UICmd.update_ $ Model.name .~ name


