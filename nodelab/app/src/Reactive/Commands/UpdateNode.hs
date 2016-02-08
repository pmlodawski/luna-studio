module Reactive.Commands.UpdateNode where

import           Utils.PreludePlus
import           Data.Text.Lazy    (Text)

import qualified Object.Widget.Node as NodeModel
import qualified JS.NodeGraph       as UI

import           Reactive.Commands.Command    (Command, performIO)
import           Reactive.Commands.Graph      (nodeIdToWidgetId)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Graph      as Graph

import qualified BatchConnector.Monadic.Commands as BatchCmd
import           Empire.API.Data.Node (NodeId)
import qualified Empire.API.Data.Node as Node


-- updateNode :: NodeId -> Text -> Command State ()
-- updateNode nodeId expr = do
--     Global.graph . Graph.nodesMap . ix nodeId . Node.expression .= expr
--     nodeMay <- preuse $ Global.graph . Graph.nodesMap . ix nodeId
--     case nodeMay of
--         Just node -> do
--             widgetId <- zoom Global.uiRegistry $ nodeIdToWidgetId nodeId
--             forM_ widgetId $ \widgetId -> do
--                 zoom Global.uiRegistry $ UICmd.update widgetId (NodeModel.expression .~ expr)
--             zoom Global.workspace $ BatchCmd.updateNode node
--             zoom Global.workspace BatchCmd.runMain
--         Nothing   -> return ()
