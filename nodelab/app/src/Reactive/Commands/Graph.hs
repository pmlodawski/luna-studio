module Reactive.Commands.Graph
    ( portRefToWidgetId
    , updateConnections
    , connectionIdToWidgetId
    , allNodes
    , nodeIdToWidgetId
    , focusNode
    ) where


import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle
import           Data.Map.Lazy                 (Map(..))
import qualified Data.Map.Lazy                 as Map
import           Data.Ord                      (comparing)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Node            as Model
import qualified Object.Widget.Connection      as ConnectionModel
import qualified Object.Widget.Port            as PortModel

import           Reactive.State.Graph
import qualified Reactive.State.Graph          as Graph
import qualified Reactive.State.UIRegistry     as UIRegistry
import qualified Reactive.State.Global         as Global
import qualified Reactive.State.Graph         as Graph
import           Reactive.State.Global         (State, inRegistry)
import           Reactive.Commands.Command     (Command)
import qualified Reactive.Commands.UIRegistry  as UICmd
import           Reactive.Commands.Node.Ports.Colors  (vtToColor)
import           UI.Instances                  ()

import           Empire.API.Data.Node          (NodeId)
import qualified Empire.API.Data.Node          as Node
import           Empire.API.Data.PortRef       (AnyPortRef(..), InPortRef(..), OutPortRef(..))
import qualified Empire.API.Data.PortRef       as PortRef
import           Empire.API.Data.Connection    (ConnectionId)
import qualified Empire.API.Data.Connection    as Connection
import           Empire.API.Data.ValueType     (ValueType (..))
import qualified Empire.API.Data.Port          as Port

allNodes :: Command Global.State [WidgetFile Model.Node]
allNodes = do
    widgetIds <- use $ Global.graph . Graph.nodeWidgets
    mayWidgets <- mapM (\id -> inRegistry $ UIRegistry.lookupTypedM id) widgetIds
    return $ catMaybes mayWidgets

allPorts :: Command Global.State [WidgetFile PortModel.Port]
allPorts = do
    widgetIds <- use $ Global.graph . Graph.portWidgets
    mayWidgets <- mapM (\id -> inRegistry $ UIRegistry.lookupTypedM id) widgetIds
    return $ catMaybes mayWidgets

allConnections :: Command Global.State [WidgetFile ConnectionModel.Connection]
allConnections = do
    widgetIds <- use $ Global.graph . Graph.connectionWidgets
    mayWidgets <- mapM (\id -> inRegistry $ UIRegistry.lookupTypedM id) widgetIds
    return $ catMaybes mayWidgets

nodeIdToWidgetId :: NodeId -> Command Global.State (Maybe WidgetId)
nodeIdToWidgetId nodeId = preuse $ Global.graph . Graph.nodeWidgetsMap . ix nodeId

connectionIdToWidgetId :: ConnectionId -> Command Global.State (Maybe WidgetId)
connectionIdToWidgetId connectionId = preuse $ Global.graph . Graph.connectionWidgetsMap . ix connectionId

portRefToWidgetId :: AnyPortRef -> Command Global.State (Maybe WidgetId)
portRefToWidgetId portRef = preuse $ Global.graph . Graph.portWidgetsMap . ix portRef

focusNode :: WidgetId -> Command Global.State ()
focusNode id = do
    nodes <- allNodes
    let sortedNodes = sortBy (comparing $ negate . (view $ widget . Model.zPos)) nodes
        sortedIds   = (view objectId) <$> sortedNodes
        newOrder    = id : (delete id sortedIds)
    inRegistry $ forM_ (zip newOrder [1..]) $ \(id, ix) -> do
        let newZPos = negate $ (fromIntegral ix) / 100.0
        UICmd.update id $ Model.zPos .~ newZPos

-- TODO: Refactor & optimize
updateConnections :: Command Global.State ()
updateConnections = do
    allConnections <- allConnections
    nodePositions  <- nodePositionMap
    portAngles     <- portRefToAngleMap
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

portRefToAngleMap :: Command Global.State (Map AnyPortRef (Double, Int))
portRefToAngleMap = do
    ports <- allPorts
    return $ Map.fromList $ (\file -> (file ^. widget . PortModel.portRef, (file ^. widget . PortModel.angle, file ^. widget . PortModel.portCount))) <$> ports

nodePositionMap :: Command Global.State (Map NodeId (Vector2 Double))
nodePositionMap = do
    nodes <- allNodes
    return $ Map.fromList $ (\file -> (file ^. widget . Model.nodeId, file ^. widget . widgetPosition)) <$> nodes

portTypes :: Command Global.State (Map AnyPortRef ValueType)
portTypes = do
    nodes <- use $ Global.graph . Graph.nodes

    return $ Map.fromList $ concat $ getPortsValueType <$> nodes where
            getPortsValueType node = portVT <$> (Map.toList $ node ^. Node.ports) where
                portVT (portId, port) = (PortRef.toAnyPortRef nodeId portId, port ^. Port.valueType) where
                nodeId = node ^. Node.nodeId

