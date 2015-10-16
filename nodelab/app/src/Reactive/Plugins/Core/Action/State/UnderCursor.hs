module Reactive.Plugins.Core.Action.State.UnderCursor where


import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Port
import           Object.Node
import qualified Object.Widget.Node as WNode
import qualified Object.Widget.Port as WPort

import           Reactive.Plugins.Core.Action.State.Global
import qualified Reactive.Plugins.Core.Action.State.Graph      as Graph
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry

import           Object.Widget
import           Utils.CtxDynamic



data UnderCursor = UnderCursor { _nodesUnderCursor   :: NodeCollection
                               , _port               :: Maybe PortRef
                               } deriving (Eq, Show)


makeLenses ''UnderCursor


instance PrettyPrinter UnderCursor where
    display (UnderCursor nodes port)
        = "n(" <> display nodes
        <> " " <> display port
        <> ")"

getNodesUnderCursor :: State -> NodeCollection
getNodesUnderCursor state = maybeToList node where
    registry = state ^. uiRegistry
    nodes    = state ^. graph . Graph.nodes
    node     = do
        widgetId    <- registry ^. UIRegistry.widgetOver
        maybeWidget <- UIRegistry.lookup widgetId registry
        widget      <- (fromCtxDynamic (maybeWidget ^. widget)) :: Maybe WNode.Node
        let nid = widget ^. WNode.nodeId
        find (\n -> (n ^. nodeId) == nid) nodes


getPortRefUnderCursor :: State -> Maybe PortRef
getPortRefUnderCursor state = do
    let registry = state ^. uiRegistry
    widgetId    <- registry ^. UIRegistry.widgetOver
    maybeWidget <- UIRegistry.lookup widgetId registry
    widget      <- (fromCtxDynamic (maybeWidget ^. widget)) :: Maybe WPort.Port
    return $ widget ^. WPort.portRef

underCursor :: State -> UnderCursor
underCursor state = UnderCursor (getNodesUnderCursor state) Nothing
