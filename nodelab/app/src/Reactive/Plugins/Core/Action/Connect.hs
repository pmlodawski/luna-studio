module Reactive.Plugins.Core.Action.Connect where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle
import qualified Utils.Nodes    as NodeUtils
import           Debug.Trace

import qualified JS.NodeGraph   as UI

import           Object.Object
import           Object.Port
import           Object.Node
import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Connection      as UIConnection

import           Event.Keyboard                hiding      (Event)
import qualified Event.Keyboard                as Keyboard
import           Event.Mouse                   hiding      (Event, widget)
import qualified Event.Mouse                   as Mouse
import           Event.Event

import           Reactive.Plugins.Core.Action
import           Reactive.Commands.Graph
import           Reactive.Commands.Command     (Command, performIO, execCommand)
import qualified Reactive.Commands.UIRegistry  as UICmd
import qualified Reactive.State.Connect        as Connect
import           Reactive.State.Connect        (Connecting(..), DragHistory(..))
import qualified Reactive.State.Graph          as Graph
import qualified Reactive.State.UIRegistry     as UIRegistry
import qualified Reactive.State.Camera         as Camera
import qualified Reactive.State.Global         as Global
import           Reactive.State.Global         (State)
import qualified Reactive.State.UnderCursor    as UnderCursor

import qualified BatchConnector.Commands       as BatchCmd

import           Control.Monad.State          hiding (State)


toAction :: Event -> Maybe (Command State ())
toAction (Mouse event@(Mouse.Event Mouse.Pressed  _   Mouse.LeftButton _ _)) = Just $ startDrag event
toAction (Mouse event@(Mouse.Event Mouse.Moved    pos Mouse.LeftButton _ _)) = Just $ handleMove pos
toAction (Mouse event@(Mouse.Event Mouse.Released _   Mouse.LeftButton _ _)) = Just stopDrag
toAction _                                                                   = Nothing

showCurrentConnection :: Vector2 Double -> Vector2 Double -> Command (UIRegistry.State a) ()
showCurrentConnection from to = UICmd.update UIRegistry.currentConnectionId $ (UIConnection.currentFrom    .~ from)
                                                                            . (UIConnection.currentTo      .~ to  )
                                                                            . (UIConnection.currentVisible .~ True)

hideCurrentConnection :: Command (UIRegistry.State a) ()
hideCurrentConnection = UICmd.update UIRegistry.currentConnectionId $ UIConnection.currentVisible .~ False

startDrag :: Mouse.Event -> Command State ()
startDrag event@(Mouse.Event _ coord _ _ _) = do
    sourceRefMay <- gets UnderCursor.getPortRefUnderCursor
    forM_ sourceRefMay $ \sourceRef -> do
        graph  <- use Global.graph
        camera <- use $ Global.camera . Camera.camera
        let source = Graph.getPort graph sourceRef
        Global.connect . Connect.connecting ?= (Connecting sourceRef source Nothing (DragHistory coord coord))
        -- zoom Global.uiRegistry $ showCurrentConnection coord coord
        -- connectingMay <- use $ Global.connect . Connect.connecting
        -- let angle = calculateAngle camera graph connectingMay

        -- Global.graph . Graph.nodesMap %= updateSourcePortInNodes angle sourceRef

handleMove :: Vector2 Int -> Command State ()
handleMove coord = do
    connectingMay <- use $ Global.connect . Connect.connecting
    forM_ connectingMay $ \connecting@(Connecting sourceRef source _ (DragHistory start current)) -> do
        graph  <- use Global.graph
        camera <- use $ Global.camera . Camera.camera
        let newHistory = DragHistory start coord
        Global.connect . Connect.connecting ?= Connecting sourceRef source Nothing newHistory
        let angle = calculateAngle camera graph connectingMay
        -- Global.graph . Graph.nodesMap %= updateSourcePortInNodes angle sourceRef
        let cursorWs = Camera.screenToWorkspace camera current
        nodesMap <- use $ Global.graph . Graph.nodesMap

        start'   <- zoom Global.camera $ Camera.screenToWorkspaceM start
        current' <- zoom Global.camera $ Camera.screenToWorkspaceM current

        zoom Global.uiRegistry $ showCurrentConnection start' current'
        -- performIO $ do
        --
        --     displayDragLine nodesMap angle cursorWs connecting

stopDrag :: Command State ()
stopDrag = do
    connectingMay <- use $ Global.connect . Connect.connecting
    forM_ connectingMay $ \connecting@(Connecting sourceRef source _ (DragHistory start current)) -> do
        graph <- use Global.graph
        Global.connect . Connect.connecting .= Nothing
        nodesMap <- use $ Global.graph . Graph.nodesMap

        destinationRefMay <- gets UnderCursor.getPortRefUnderCursor
        forM_ destinationRefMay $ \destinationRef -> do
            let srcDstMay = NodeUtils.getSrcDstMay sourceRef destinationRef
            forM_ srcDstMay $ \(src, dst) -> do
                connectNodes src dst

    zoom Global.uiRegistry hideCurrentConnection
    updatePortAngles
    updateConnections

calculateAngle camera oldGraph (Just (Connecting sourceRef source destinationMay (DragHistory startPos currentPos))) = calcAngle destinPoint sourcePoint where
    sourcePoint = NodeUtils.getNodePos (Graph.getNodesMap oldGraph) $ sourceRef ^. refPortNodeId
    destinPoint = Camera.screenToWorkspace camera currentPos
calculateAngle _ _ Nothing = 0.0

