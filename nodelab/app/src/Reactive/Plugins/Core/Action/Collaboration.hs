module Reactive.Plugins.Core.Action.Collaboration
    ( toAction
    ) where


import           Utils.PreludePlus

import qualified Batch.Workspace                             as Workspace
import qualified Data.DateTime as DT
import qualified Data.Map.Lazy as Map

import           Empire.API.Data.GraphLocation               (GraphLocation)
import qualified Empire.API.Graph.Collaboration              as Collaboration

import           Event.Batch                                 (Event (..))
import qualified Event.Event                                 as Event

import           Reactive.Commands.Batch                     (collaborativeTouch)
import           Reactive.Commands.Command                   (Command)
import           Reactive.Commands.Graph                     (nodeIdToWidgetId)
import           Reactive.Commands.Graph.Selection (selectedNodes)
import           Reactive.State.Global                       (State, inRegistry)
import qualified Reactive.State.Global                       as Global
import qualified Reactive.State.Graph                        as GraphST

import           Object.Widget                               (widget)
import qualified Object.Widget.Node                          as NodeModel
import           Reactive.Commands.UIRegistry                as UICmd


isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)

isCurrentLocationAndGraphLoaded :: GraphLocation -> Command State Bool
isCurrentLocationAndGraphLoaded location = do
    icl <- isCurrentLocation location
    igl <- use $ Global.workspace . Workspace.isGraphLoaded
    return $ icl && igl

toAction :: Event.Event -> Maybe (Command State ())
toAction (Event.Batch ev) = Just $ case ev of
    CollaborationUpdate update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. Collaboration.location)
        let clientId = update ^. Collaboration.clientId
        myClientId   <- use Global.clientId
        currentTime  <- use Global.lastEventTimestamp
        when (shouldProcess && clientId /= myClientId) $
            case update ^. Collaboration.event of
                Collaboration.Touch nodeIds -> do
                    forM_ nodeIds $ \nodeId -> do
                        nodeWidget <- nodeIdToWidgetId nodeId
                        inRegistry $ withJust nodeWidget $ \nodeWidget ->
                            UICmd.update_ nodeWidget $ NodeModel.collaboratingClients . at clientId ?~ currentTime
                Collaboration.CancelTouch nodeIds -> do
                    forM_ nodeIds $ \nodeId -> do
                        nodeWidget <- nodeIdToWidgetId nodeId
                        inRegistry $ withJust nodeWidget $ \nodeWidget ->
                            UICmd.update_ nodeWidget $ NodeModel.collaboratingClients . at clientId .~ Nothing

                _ -> return ()

    _ -> return ()
toAction Event.Tick = Just $ do
    widgetIds <- use $ Global.graph . GraphST.nodeWidgets
    currentTime  <- use Global.lastEventTimestamp
    forM_ widgetIds $ \nodeWidget -> do
        inRegistry $ UICmd.update_ nodeWidget $ NodeModel.collaboratingClients %~ Map.filter (\ts -> (DT.diffSeconds currentTime ts) < 30)
    when ((DT.toSeconds currentTime) `mod` 15 == 0) $ do
        selected <- selectedNodes
        let nodeIds = (view $ widget . NodeModel.nodeId) <$> selected
        collaborativeTouch nodeIds

toAction _ = Nothing
