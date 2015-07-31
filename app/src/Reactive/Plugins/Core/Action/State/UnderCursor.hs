module Reactive.Plugins.Core.Action.State.UnderCursor where


import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Port
import           Object.Node

import           Reactive.Plugins.Core.Action.State.Global

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
getNodesUnderCursor state = getNodesAt (state ^. mousePos) (toCamera state) (state ^. nodes)


getPortRefUnderCursor :: State -> Maybe PortRef
getPortRefUnderCursor state = getPortRef (state ^. mousePos) (toCamera state) (state ^. nodes)


underCursor :: State -> UnderCursor
underCursor state = UnderCursor (getNodesUnderCursor state) Nothing -- (getPortRefUnderCursor state)
