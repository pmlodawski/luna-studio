module Reactive.Commands.Graph
    ( portRefToWidgetId
    , updateConnections
    , batchConnectNodes
    , connectionIdToWidgetId
    , updateConnNodes
    , allNodes
    , nodeIdToWidgetId
    , colorPort
    , portDefaultAngle
    , focusNode
    , localConnectNodes
    ) where


import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle

import           Data.Map.Lazy                 (Map(..))
import qualified Data.Map.Lazy                 as Map
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

import qualified Reactive.Commands.Batch       as BatchCmd

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
import           Empire.API.Data.TypeRep       (TypeRep (..))
import           Empire.API.Data.Port          (PortId(..))
import qualified Empire.API.Data.Port          as Port
import qualified Empire.API.Data.ValueType     as ValueType

hashMany :: [TypeRep] -> Int
hashMany as = sum $ zipWith (*) powers (tpRepToColor <$> as) where
    powers = (37 ^) <$> [0..]

ensureRange n = (n `mod` 8) + 1

tpRepToColor :: TypeRep -> Int
tpRepToColor (TCons tn as) = ensureRange $ case tn of
     "Int"        -> 0
     "Bool"       -> 1
     "Double"     -> 2
     "String"     -> 3
     "List"       -> 5 + hashMany as
     _            -> hash tn + hashMany as
tpRepToColor (TLam as out) = ensureRange . hashMany $ out : as
tpRepToColor (TVar n) = 9
tpRepToColor _ = 0

vtToColor (TypeIdent t) = tpRepToColor t
vtToColor _ = 0

colorPort port = vtToColor $ port ^. Port.valueType


updateConnNodes :: [NodeId] -> Command Global.State ()
updateConnNodes nodeIds = pureCommand $ \state -> let
    noConns nid   = not $ hasConnections nid (state ^. Global.graph)
    changeFun     = nodeIds & filter noConns
                            & map   (Map.adjust id)
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

getConnectionLine :: Map NodeId (Vector2 Double) -> Map AnyPortRef (Double, Int) -> Map AnyPortRef ValueType -> OutPortRef  -> InPortRef -> (Vector2 Double, Vector2 Double, Bool, Int)
getConnectionLine nodePos portAngles portTypes srcPortRef dstPortRef = (srcWs, dstWs, visible, color) where
    srcNWs@(Vector2 xSrcN ySrcN) = nodePos Map.! (srcPortRef ^. PortRef.srcNodeId)
    dstNWs@(Vector2 xDstN yDstN) = nodePos Map.! (dstPortRef ^. PortRef.dstNodeId)
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
    missingPortPos               = (-1, 0)
    missingPortColor             = 13


batchConnectNodes :: OutPortRef -> InPortRef -> Command Global.State ()
batchConnectNodes src dst = BatchCmd.connectNodes src dst

withArrow :: Getter InPortRef Bool
withArrow = to $ \ref -> case ref of
    InPortRef _ Port.Self -> False
    _                     -> True

localConnectNodes :: OutPortRef -> InPortRef -> Command Global.State ()
localConnectNodes src dst = do
    prevConn <- preuse $ Global.graph . Graph.connectionsMap . ix dst
    connectionId <- zoom Global.graph $ Graph.addConnection src dst
    let newConnection = not $ isJust prevConn
    when newConnection $ zoom Global.uiRegistry $ UICmd.register_ sceneGraphId (ConnectionModel.Connection connectionId True def def (dst ^. withArrow) def) def

portRefToAngleMap :: Command UIRegistry.State (Map AnyPortRef (Double, Int))
portRefToAngleMap = do
    ports <- allPorts
    return $ Map.fromList $ (\file -> (file ^. widget . PortModel.portRef, (file ^. widget . PortModel.angle, file ^. widget . PortModel.portCount))) <$> ports

nodePositionMap :: Command UIRegistry.State (Map NodeId (Vector2 Double))
nodePositionMap = do
    nodes <- allNodes
    return $ Map.fromList $ (\file -> (file ^. widget . Model.nodeId, file ^. widget . widgetPosition)) <$> nodes

angleToDimVec :: Double -> Vector2 Double
angleToDimVec angle = (/ 10.0) <$> Vector2 (cos angle) (-sin angle)

portDefaultAngle :: Int -> PortId -> Vector2 Double
portDefaultAngle numPorts (OutPortId _) = angleToDimVec $ 0.0
portDefaultAngle numPorts (InPortId (Port.Arg portNum)) = angleToDimVec angle where
    angle = delta * (fromIntegral portNum) + delta / 2.0 + pi / 2.0
    delta = pi / (fromIntegral numPorts)
portDefaultAngle numPorts (InPortId (Port.Self)) = angleToDimVec 0.0

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
