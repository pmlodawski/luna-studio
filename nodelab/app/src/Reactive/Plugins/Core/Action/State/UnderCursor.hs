module Reactive.Plugins.Core.Action.State.UnderCursor where


import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Port
import           Object.Node
import qualified Object.Widget.Node as WNode

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
-- getNodesUnderCursor state = getNodesAt (state ^. mousePos) (toCamera state) (Graph.getNodes $ state ^. graph)
-- --
getNodesUnderCursor state = maybeToList node where
    registry = state ^. uiRegistry
    nodes    = Graph.getNodes $ state ^. graph
    node     = do
        widgetId    <- registry ^. UIRegistry.widgetOver
        maybeWidget <- UIRegistry.lookup widgetId registry
        widget      <- (fromCtxDynamic maybeWidget) :: Maybe WNode.Node
        let nid = widget ^. WNode.nodeId
        find (\n -> (n ^. nodeId) == nid) nodes


getPortRefUnderCursor :: State -> Maybe PortRef
getPortRefUnderCursor state = getPortRef (state ^. mousePos) (toCamera state) (Graph.getNodes $ state ^. graph)


underCursor :: State -> UnderCursor
underCursor state = UnderCursor (getNodesUnderCursor state) Nothing -- (getPortRefUnderCursor state)
