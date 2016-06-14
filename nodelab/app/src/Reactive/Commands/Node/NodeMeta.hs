module Reactive.Commands.Node.NodeMeta
    ( updateNodeMeta
    ) where

import           Utils.PreludePlus
import           Utils.Vector

import           Empire.API.Data.Node         (NodeId)
import qualified Empire.API.Data.Node         as Node
import           Empire.API.Data.NodeMeta     (NodeMeta (..))
import qualified Empire.API.Data.NodeMeta     as NodeMeta

import qualified Object.Widget.Node           as Model

import           Reactive.Commands.Command    (Command)
import           Reactive.Commands.Graph      (nodeIdToWidgetId, updateConnectionsForNodes)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.Graph         as Graph

updateNodeMeta :: NodeId -> NodeMeta -> Command Global.State ()
updateNodeMeta nodeId meta = do
    Global.graph . Graph.nodesMap . ix nodeId . Node.nodeMeta .= meta
    widgetId <- nodeIdToWidgetId nodeId
    inRegistry $ do
        withJust widgetId $ \widgetId -> do
            UICmd.move   widgetId $ fromTuple $  meta ^. NodeMeta.position
    updateConnectionsForNodes [nodeId]

