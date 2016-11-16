module Reactive.Commands.Graph.Selection
     ( selectedNodes
     , focusSelectedNode
     , selectAll
     , selectNodes
     , trySelectFromState
     , unselectAll
     ) where

import           Control.Monad.State          (modify)
import qualified Data.Set                     as Set
import           Utils.PreludePlus

import           Empire.API.Data.Node         (NodeId)
import qualified Empire.API.Data.Node         as Node

import           Object.Widget                (WidgetFile (..), objectId, widget)
import qualified Object.Widget.Node           as NodeModel

import           Reactive.Commands.Batch      (cancelCollaborativeTouch, collaborativeTouch)
import           Reactive.Commands.Command    (Command)
import           Reactive.Commands.Graph      (allNodes, nodeIdToWidgetId)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (State, graph, inRegistry, nodesToSelectIds)
import           Reactive.State.Graph         (getNodes)
import qualified Reactive.State.UIRegistry    as UIRegistry


unselectAll :: Command State ()
unselectAll = do
    widgets <- allNodes
    nodesToCancelTouch <- inRegistry $ forM widgets $ \wf -> do
        let widgetId = wf ^. objectId
        if (wf ^. widget . NodeModel.isSelected) then do
                UICmd.update_ widgetId $ NodeModel.isSelected .~ False
                return $ Just $ wf ^. widget . NodeModel.nodeId
        else return Nothing

    cancelCollaborativeTouch $ catMaybes nodesToCancelTouch

selectAll :: Command State ()
selectAll = do
    widgets <- allNodes
    selectNodes $ (view $ widget . NodeModel.nodeId) <$> widgets

selectNodes :: [NodeId] -> Command State ()
selectNodes nodeIds = do
    widgetIds <- fmap catMaybes $ mapM nodeIdToWidgetId nodeIds
    inRegistry $ forM_ widgetIds $ (flip UICmd.update) (NodeModel.isSelected .~ True)
    focusSelectedNode
    collaborativeTouch nodeIds

selectedNodes :: Command State [WidgetFile NodeModel.Node]
selectedNodes = do
    widgets <- allNodes
    return $ filter (^. widget . NodeModel.isSelected) widgets

trySelectFromState :: Command State ()
trySelectFromState = do
    graph'    <- use graph
    toSelect  <- use nodesToSelectIds
    let allNodeIds    = Set.fromList $ map (^. Node.nodeId) $ getNodes graph'
        shouldProceed = Set.isSubsetOf toSelect allNodeIds && not (Set.null toSelect)
    when shouldProceed $ do
        unselectAll
        selectNodes $ Set.toList toSelect
        modify (& nodesToSelectIds .~ Set.empty)

focusSelectedNode :: Command State ()
focusSelectedNode = do
    widgets <- selectedNodes
    inRegistry $ UIRegistry.focusedWidget .= (view objectId <$> widgets ^? ix 0)
