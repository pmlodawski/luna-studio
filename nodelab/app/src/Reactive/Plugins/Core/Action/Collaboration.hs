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
import           Reactive.Commands.Graph.Selection           (selectedNodes)
import           Reactive.State.Global                       (State, inRegistry)
import qualified Reactive.State.Global                       as Global
import qualified Reactive.State.Graph                        as GraphST

import           Object.Widget                               (widget)
import qualified Object.Widget.Node                          as NodeModel
import           Reactive.Commands.UIRegistry                as UICmd


refreshTime, modifyTime :: Integer
refreshTime = 10
modifyTime  =  5

isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)

isCurrentLocationAndGraphLoaded :: GraphLocation -> Command State Bool
isCurrentLocationAndGraphLoaded location = do
    icl <- isCurrentLocation location
    igl <- use $ Global.workspace . Workspace.isGraphLoaded
    return $ icl && igl

touchCurrentlySelected :: Command State ()
touchCurrentlySelected = do
    selected <- selectedNodes
    let nodeIds = (view $ widget . NodeModel.nodeId) <$> selected
    collaborativeTouch nodeIds

expireTouchedNodes :: Command State ()
expireTouchedNodes = do
    widgetIds <- use $ Global.graph . GraphST.nodeWidgets
    currentTime  <- use Global.lastEventTimestamp
    forM_ widgetIds $ \nodeWidget -> do
        inRegistry $ UICmd.update_ nodeWidget $ (NodeModel.collaboration . NodeModel.touch  %~ Map.filter (\ts -> (DT.diffSeconds ts currentTime) > 0))
                                              . (NodeModel.collaboration . NodeModel.modify %~ Map.filter (\ts -> (DT.diffSeconds ts currentTime) > 0))


everyNSeconds :: Integer -> Command State () -> Command State ()
everyNSeconds interval action = do
    currentTime  <- use Global.lastEventTimestamp
    when ((DT.toSeconds currentTime) `mod` interval == 0) action

toAction :: Event.Event -> Maybe (Command State ())
toAction (Event.Batch ev) = Just $ case ev of
    CollaborationUpdate update -> do
        shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. Collaboration.location)
        let clientId = update ^. Collaboration.clientId
            touchNodes nodeIds setter = forM_ nodeIds $ \nodeId -> do
                    nodeWidget <- nodeIdToWidgetId nodeId
                    inRegistry $ withJust nodeWidget $ \nodeWidget -> UICmd.update_ nodeWidget  setter
        myClientId   <- use Global.clientId
        currentTime  <- use Global.lastEventTimestamp
        when (shouldProcess && clientId /= myClientId) $
            case update ^. Collaboration.event of
                Collaboration.Touch       nodeIds -> touchNodes nodeIds $ NodeModel.collaboration . NodeModel.touch  . at clientId ?~ (DT.addSeconds (2 * refreshTime) currentTime)
                Collaboration.Modify      nodeIds -> touchNodes nodeIds $ NodeModel.collaboration . NodeModel.modify . at clientId ?~ (DT.addSeconds modifyTime currentTime)
                Collaboration.CancelTouch nodeIds -> touchNodes nodeIds $ NodeModel.collaboration . NodeModel.touch  . at clientId .~ Nothing
                _ -> return ()

    _ -> return ()

toAction Event.Tick = Just $ do
    expireTouchedNodes
    everyNSeconds refreshTime touchCurrentlySelected

toAction _ = Nothing
