{-# LANGUAGE TupleSections #-}
module Reactive.Plugins.Core.Action.Drag
    ( toAction
    ) where

import           Utils.PreludePlus
import           Utils.Vector

import           Object.Widget                     (WidgetFile, objectId, parent, widget, widgetPosition)

import           Control.Monad.State               hiding (State)
import           Control.Monad.Trans.Maybe

import           Event.Event
import           Event.Keyboard                    hiding (Event)
import qualified Event.Mouse                       as Mouse

import qualified Reactive.Commands.Batch           as BatchCmd
import           Reactive.Commands.Command         (Command)
import           Reactive.Commands.Graph           (allNodes, updateConnectionsForNodes)
import           Reactive.Commands.Graph.Selection (selectedNodes)
import qualified Reactive.Commands.UIRegistry      as UICmd
import qualified Reactive.State.Camera             as Camera
import           Reactive.State.Drag               (DragHistory (..))
import qualified Reactive.State.Drag               as Drag
import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph
import qualified Reactive.State.UIRegistry         as UIRegistry

import           Object.Widget.Label               (Label)
import qualified Object.Widget.Node                as Model

import qualified UI.Handlers.Node                  as Node

import qualified Empire.API.Data.Node              as Node


toAction :: Event -> Maybe (Command State ())
toAction (Mouse _ event@(Mouse.Event Mouse.Pressed  pos   Mouse.LeftButton (KeyMods False False False False) (Just _))) = Just $ startDrag pos
toAction (Mouse _ event@(Mouse.Event Mouse.Moved    pos Mouse.LeftButton _ _)) = Just $ handleMove pos
toAction (Mouse _ event@(Mouse.Event Mouse.Released _   Mouse.LeftButton _ _)) = Just stopDrag
toAction _                                                                     = Nothing


isNodeUnderCursor :: Command UIRegistry.State Bool
isNodeUnderCursor = runMaybeT act >>= (return . isJust) where
    act = do
        (Just id) <- lift $ use UIRegistry.widgetOver
        (Just w ) <- lift $ (UIRegistry.lookupTypedM id :: Command UIRegistry.State (Maybe (WidgetFile Model.Node)))
        return w

isNodeLabelUnderCursor :: Command UIRegistry.State Bool
isNodeLabelUnderCursor = runMaybeT act >>= (\x -> return $ fromMaybe False x) where
    act = do
        (Just id) <- lift $ use UIRegistry.widgetOver
        (Just w)  <- lift $ (UIRegistry.lookupTypedM id :: Command UIRegistry.State (Maybe (WidgetFile Label)))
        (Just p)  <- return $ w ^. parent
        (Just n)  <- lift $ (UIRegistry.lookupTypedM p :: Command UIRegistry.State (Maybe (WidgetFile Model.Node)))
        exId <- lift $ Node.expressionId p
        return $ id == exId

startDrag :: Vector2 Int -> Command State ()
startDrag coord = do
    shouldDrag  <- zoom Global.uiRegistry isNodeUnderCursor
    shouldDrag' <- zoom Global.uiRegistry isNodeLabelUnderCursor
    when (shouldDrag || shouldDrag') $
        Global.drag . Drag.history ?= (DragHistory coord coord coord)

handleMove :: Vector2 Int -> Command State ()
handleMove coord = do
    dragHistory <- use $ Global.drag . Drag.history
    withJust dragHistory $ \(DragHistory start previous current) -> do
        Global.drag . Drag.history ?= DragHistory start current coord
        moveNodes $ coord - current

moveNodes :: Vector2 Int -> Command State ()
moveNodes delta = do
    widgets <- selectedNodes
    delta'  <- scaledDelta delta
    let selectedIds = (^. objectId) <$> widgets
    forM_ selectedIds $ \id -> zoom Global.uiRegistry $ UICmd.moveBy id delta'
    updateConnectionsForNodes $ (view $ widget . Model.nodeId) <$> widgets

stopDrag :: Command State ()
stopDrag = do
    dragHistory <- use $ Global.drag . Drag.history

    withJust dragHistory $ \(DragHistory start current _) -> do
        Global.drag . Drag.history .= Nothing
        when (start /= current) $ do
            widgets <- allNodes

            let selected = filter (^. widget . Model.isSelected) widgets
                nodesToUpdate = (\w -> (w ^. widget . Model.nodeId, w ^. widget . widgetPosition)) <$> selected

            updates <- forM nodesToUpdate $ \(id, pos) -> do
                Global.graph . Graph.nodesMap . ix id . Node.position .= toTuple pos
                newMeta <- preuse $ Global.graph . Graph.nodesMap . ix id . Node.nodeMeta
                return $ (id, ) <$> newMeta
            BatchCmd.updateNodeMeta $ catMaybes updates
            updateConnectionsForNodes $ fst <$> nodesToUpdate

scaledDelta :: Vector2 Int -> Command State (Vector2 Double)
scaledDelta delta = do
    factor <- use $ Global.camera . Camera.camera . Camera.factor
    return $ (/ factor) . fromIntegral <$> delta
