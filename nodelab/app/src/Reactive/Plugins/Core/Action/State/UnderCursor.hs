module Reactive.Plugins.Core.Action.State.UnderCursor where


import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Port
import           Object.Node

import           Reactive.Plugins.Core.Action.State.Global
import qualified Reactive.Plugins.Core.Action.State.Graph      as Graph
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry

import           Object.Widget



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
getNodesUnderCursor state = getNodesAt (state ^. mousePos) (toCamera state) (Graph.getNodes $ state ^. graph)
--
-- getNodesUnderCursor state = maybeToList node where
--     node = do
--         widgetId <- state ^. uiRegistry . UIRegistry.widgetOver
--         if (widgetId `div` 65536) == 1 then find (\x -> (x ^. nodeId) == (widgetId - 65536)) (state ^. nodes)
--                                        else Nothing


getPortRefUnderCursor :: State -> Maybe PortRef
getPortRefUnderCursor state = getPortRef (state ^. mousePos) (toCamera state) (Graph.getNodes $ state ^. graph)


underCursor :: State -> UnderCursor
underCursor state = UnderCursor (getNodesUnderCursor state) Nothing -- (getPortRefUnderCursor state)
