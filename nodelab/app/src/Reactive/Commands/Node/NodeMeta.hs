module Reactive.Commands.Node.NodeMeta
    ( updateNodesMeta
    , modifyNodeMeta
    ) where

import           Utils.PreludePlus
import           Utils.Vector

import           Empire.API.Data.Node         (NodeId)
import qualified Empire.API.Data.Node         as Node
import           Empire.API.Data.NodeMeta     (NodeMeta (..))
import qualified Empire.API.Data.NodeMeta     as NodeMeta

import qualified Object.Widget.Node           as NodeModel

import qualified Reactive.Commands.Batch      as BatchCmd
import           Reactive.Commands.Command    (Command)
import           Reactive.Commands.Graph      (nodeIdToWidgetId, updateConnectionsForNodes)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.Graph         as Graph

updateNodeMeta' :: NodeId -> NodeMeta -> Command Global.State ()
updateNodeMeta' nodeId meta = do
    Global.graph . Graph.nodesMap . ix nodeId . Node.nodeMeta .= meta
    widgetId' <- nodeIdToWidgetId nodeId
    inRegistry $ do
        withJust widgetId' $ \widgetId -> do
            UICmd.update widgetId $ NodeModel.visualizationsEnabled .~ meta ^. NodeMeta.displayResult
            UICmd.move   widgetId $ fromTuple $  meta ^. NodeMeta.position


updateNodeMeta :: NodeId -> NodeMeta -> Command Global.State ()
updateNodeMeta nodeId meta = do
    updateNodeMeta' nodeId meta
    updateConnectionsForNodes [nodeId]

updateNodesMeta :: [(NodeId, NodeMeta)] -> Command Global.State ()
updateNodesMeta updates = do
    mapM (uncurry updateNodeMeta') updates
    updateConnectionsForNodes $ fst <$> updates

modifyNodeMeta :: NodeId -> (NodeMeta -> NodeMeta) -> Command Global.State ()
modifyNodeMeta nid setter = do
    oldMeta <- preuse $ Global.graph . Graph.nodesMap . ix nid . Node.nodeMeta
    withJust oldMeta $ \oldMeta -> do
        let newMeta = setter oldMeta
        BatchCmd.updateNodeMeta [(nid, newMeta)]
