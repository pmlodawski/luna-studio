module Reactive.Commands.Graph
    ( portRefToWidgetId
    , updateConnections
    , updateConnection
    , updateConnectionsForNodes
    , connectionIdToWidgetId
    , allNodes
    , nodeIdToWidgetId
    , focusNode
    ) where


import qualified Data.Map.Lazy                       as Map
import qualified Data.Set                            as Set
import           Data.Ord                            (comparing)
import           Utils.Angle
import           Utils.PreludePlus
import           Utils.Vector
import           Control.Monad.Trans.Maybe (runMaybeT)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Connection            as ConnectionModel
import qualified Object.Widget.Node                  as Model
import qualified Object.Widget.Port                  as PortModel

import           Reactive.Commands.Command           (Command)
import           Reactive.Commands.Node.Ports.Colors (vtToColor)
import qualified Reactive.Commands.UIRegistry        as UICmd
import           Reactive.State.Global               (inRegistry)
import qualified Reactive.State.Global               as Global
import qualified Reactive.State.Graph                as Graph
import qualified Reactive.State.UIRegistry           as UIRegistry
import           UI.Instances                        ()

import           Empire.API.Data.Connection          (ConnectionId)
import qualified Empire.API.Data.Connection          as Connection
import           Empire.API.Data.Node                (NodeId)
import qualified Empire.API.Data.Node                as Node
import qualified Empire.API.Data.Port                as Port
import           Empire.API.Data.PortRef             (AnyPortRef (..), InPortRef (..))
import qualified Empire.API.Data.PortRef             as PortRef

allNodes :: Command Global.State [WidgetFile Model.Node]
allNodes = do
    widgetIds <- use $ Global.graph . Graph.nodeWidgets
    mayWidgets <- mapM (\id -> inRegistry $ UIRegistry.lookupTypedM id) widgetIds
    return $ catMaybes mayWidgets

-- allPorts :: Command Global.State [WidgetFile PortModel.Port]
-- allPorts = do
--     widgetIds <- use $ Global.graph . Graph.portWidgets
--     mayWidgets <- mapM (\id -> inRegistry $ UIRegistry.lookupTypedM id) widgetIds
--     return $ catMaybes mayWidgets

getPort :: AnyPortRef -> Command Global.State (Maybe PortModel.Port)
getPort portRef = runMaybeT $ do
    (Just widgetId) <- preuse $ Global.graph . Graph.portWidgetsMap . ix portRef
    lift $ inRegistry $ UICmd.lookup widgetId

getGraphPort :: AnyPortRef -> Command Global.State (Maybe Port.Port)
getGraphPort portRef = preuse $ Global.graph . Graph.nodesMap . ix (portRef ^. PortRef.nodeId) . Node.ports . ix (portRef ^. PortRef.portId)

getNode :: NodeId -> Command Global.State (Maybe Model.Node)
getNode nodeId = runMaybeT $ do
    (Just widgetId)   <- preuse $ Global.graph . Graph.nodeWidgetsMap . ix nodeId
    lift $ inRegistry $ UICmd.lookup widgetId

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

updateConnections :: Command Global.State ()
updateConnections = do
    connectionIds <- uses (Global.graph . Graph.connectionsMap) Map.keys
    mapM_ updateConnection connectionIds

updateConnectionsForNodes :: [NodeId] -> Command Global.State ()
updateConnectionsForNodes nodes = do
    connections <- uses (Global.graph . Graph.connectionsMap) Map.toList
    let nodes' = Set.fromList nodes
        connectionsToUpdate = [id | (id, conn) <- connections, (Set.member (conn ^. Connection.src . PortRef.srcNodeId) nodes' || Set.member (conn ^. Connection.dst  . PortRef.dstNodeId) nodes') ]
    mapM_ updateConnection connectionsToUpdate

updateConnection :: ConnectionId -> Command Global.State ()
updateConnection connectionId = do
    (Just connection) <- preuse $ Global.graph . Graph.connectionsMap       . ix connectionId
    (Just widgetId  ) <- preuse $ Global.graph . Graph.connectionWidgetsMap . ix connectionId

    (Just srcPort   ) <- getPort $ OutPortRef' $ connection ^. Connection.src
    (Just srcNode   ) <- getNode $ connection ^. Connection.src . PortRef.srcNodeId
    (Just srcGraphPort) <- getGraphPort $ OutPortRef' $ connection ^. Connection.src

    (Just dstPort   ) <- getPort $ InPortRef' $ connection ^. Connection.dst
    (Just dstNode   ) <- getNode $ connection ^. Connection.dst . PortRef.dstNodeId
    let dstRadius      = portRadius $ connection ^. Connection.dst
        srcNodePos     = srcNode ^. widgetPosition
        dstNodePos     = dstNode ^. widgetPosition

    let srcPortAngle   = srcPort ^. PortModel.angle
        srcPortCount   = srcPort ^. PortModel.portCount
        dstPortAngle   = dstPort ^. PortModel.angle
        dstPortCount   = dstPort ^. PortModel.portCount

    let srcPortAngle'  = boundedAngle srcPortAngle srcPortCount srcNodePos dstNodePos
        dstPortAngle'  = boundedAngle dstPortAngle dstPortCount dstNodePos srcNodePos

    let posSrc         = moveByAngle srcNodePos normalPortRadius srcPortAngle'
        posDst         = moveByAngle dstNodePos dstRadius        dstPortAngle'

    let visible        = lengthSquared (dstNodePos - srcNodePos) > 100
        color          = vtToColor $ srcGraphPort ^. Port.valueType

    void $ inRegistry $ UICmd.update widgetId $ (ConnectionModel.from    .~ posSrc)
                                              . (ConnectionModel.to      .~ posDst)
                                              . (ConnectionModel.visible .~ visible)
                                              . (ConnectionModel.color   .~ color)

moveByAngle :: Vector2 Double -> Double -> Angle -> Vector2 Double
moveByAngle (Vector2 x y) radius angle = Vector2 (x + radius * cos angle) (y + radius * sin angle)

normalPortRadius = 22.0

portRadius (InPortRef _ Port.Self) = 0.0
portRadius _ = normalPortRadius
