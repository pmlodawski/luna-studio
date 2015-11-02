module Reactive.Plugins.Core.Action.Drag where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle

import qualified JS.NodeGraph   as UI

import           Object.Object
import           Object.Node
import           Object.UITypes

import           Event.Keyboard hiding        (Event)
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding        (Event)
import qualified Event.Mouse    as Mouse
import           Event.Event

import           Reactive.Plugins.Core.Action
import           Reactive.Commands.Graph
import           Reactive.Commands.Command    (Command, performIO)
import qualified Reactive.State.Drag          as Drag
import           Reactive.State.Drag          (DragHistory(..))
import qualified Reactive.State.Graph         as Graph
import qualified Reactive.State.Selection     as Selection
import qualified Reactive.State.Camera        as Camera
import qualified Reactive.State.Global        as Global
import           Reactive.State.Global        (State)
import qualified Reactive.State.UnderCursor   as UnderCursor

import qualified Data.IntMap.Lazy             as IntMap
import qualified BatchConnector.Commands      as BatchCmd
import           Batch.Workspace              (Workspace)

import           Control.Monad.State          hiding (State)


toAction :: Event -> Maybe (Command State ())
toAction (Mouse event@(Mouse.Event Mouse.Pressed  _   Mouse.LeftButton _ _)) = Just $ startDrag event
toAction (Mouse event@(Mouse.Event Mouse.Moved    pos Mouse.LeftButton _ _)) = Just $ handleMove pos
toAction (Mouse event@(Mouse.Event Mouse.Released _   Mouse.LeftButton _ _)) = Just stopDrag
toAction _                                                                   = Nothing

shouldStartDrag :: Mouse.Event -> Command State Bool
shouldStartDrag (Mouse.Event Mouse.Pressed _ Mouse.LeftButton (KeyMods False False False False) (Just _)) = do
    nodesUnderCursor <- gets UnderCursor.getNodesUnderCursor
    return . not . null $ nodesUnderCursor
shouldStartDrag _ = return False

startDrag :: Mouse.Event -> Command State ()
startDrag event@(Mouse.Event _ coord _ _ _) = do
    shouldDrag <- shouldStartDrag event
    when shouldDrag $ do
        Global.drag . Drag.history ?= (DragHistory coord coord coord)

handleMove :: Vector2 Int -> Command State ()
handleMove coord = do
    dragHistory <- use $ Global.drag . Drag.history
    forM_ dragHistory $ \(DragHistory start previous current) -> do
        Global.drag . Drag.history ?= DragHistory start current coord
        moveNodes $ coord - current

moveNodes :: Vector2 Int -> Command State ()
moveNodes delta = do
    selection <- use $ Global.selection . Selection.nodeIds
    camFactor <- use $ Global.camera . Camera.camera . Camera.factor
    Global.graph . Graph.nodesMap %= changeNodesCoords camFactor delta selection
    updatePortAngles
    updateConnections
    nodesMap  <- use $ Global.graph . Graph.nodesMap
    let selNodes = IntMap.filter (\node -> node ^. nodeId `elem` selection) nodesMap
    performIO $ do
        moveNodesUI selNodes

stopDrag :: Command State ()
stopDrag = do
    Global.drag . Drag.history .= Nothing
    nodesMap  <- use $ Global.graph . Graph.nodesMap
    selection <- use $ Global.selection . Selection.nodeIds
    workspace <- use $ Global.workspace
    let selNodes = IntMap.filter (\node -> node ^. nodeId `elem` selection) nodesMap
    performIO $ updateNodesBatch workspace selNodes

changeNodesCoords :: Double -> Vector2 Int -> [NodeId] -> NodesMap -> NodesMap
changeNodesCoords factor delta selection = fmap $ \node -> if elem (node ^. nodeId) selection then node & nodePos +~ deltaWs else node where
    deltaWs = deltaToWs factor delta

deltaToWs :: Double -> Vector2 Int -> Vector2 Double
deltaToWs factor delta = (/ factor) . fromIntegral <$> delta

updateNodesBatch :: Workspace -> NodesMap -> IO ()
updateNodesBatch workspace nodesMap = BatchCmd.updateNodes workspace $ IntMap.elems nodesMap
