module Reactive.Plugins.Core.Action.Backend.Runner where

import           Utils.PreludePlus

import           Object.Object
import           Object.Node
import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Node       as UINode
import qualified Object.Widget.Slider     as UISlider
import qualified Object.Widget            as Widget
import           Object.Widget            (WidgetFile)
import           Object.Widget.Helpers

import           Event.Event
import qualified Event.Batch             as Batch
import           BatchConnector.Commands as BatchCmd
import           Batch.Workspace
import           Batch.Value
import           JS.NodeGraph            (setComputedValue)

import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.State.Graph as Graph
import           Reactive.Plugins.Core.Action.State.Graph hiding (State)
import qualified Reactive.Plugins.Core.Action.State.UIRegistry     as UIRegistry
import           Reactive.Plugins.Core.Action.State.Global as Global

data Action = RequestRerun
            | UpdateValue Int Value
            deriving (Show, Eq)

instance PrettyPrinter Action where
    display = show

newtype Reaction = PerformIO (IO ())

instance PrettyPrinter Reaction where
    display _ = "runner(PerfomIO)"

toAction :: Event Node -> Maybe Action
toAction (Batch (Batch.NodeAdded _))              = Just RequestRerun
toAction (Batch  Batch.NodesConnected)            = Just RequestRerun
toAction (Batch  Batch.NodesDisconnected)         = Just RequestRerun
toAction (Batch  Batch.NodeRemoved)               = Just RequestRerun
toAction (Batch  Batch.NodeModified)              = Just RequestRerun
toAction (Batch (Batch.ValueUpdate nodeId value)) = Just $ UpdateValue nodeId value
toAction _                                        = Nothing

instance ActionStateUpdater Action where
    execSt RequestRerun state               = ActionUI (PerformIO BatchCmd.runMain) state
    execSt (UpdateValue nodeId value) state = ActionUI io newState where
        uiRegistry = state ^. Global.uiRegistry
        (newState, sio)   = setFixedValues nodeId value state
        nodeMay    = nodeIdToWidgetId uiRegistry nodeId
        lookupSlider :: WidgetId   -> Maybe (WidgetFile Global.State (UISlider.Slider Double))
        lookupSlider widgetId       = UIRegistry.lookupTyped widgetId uiRegistry
        io = PerformIO $ do
                setComputedValue nodeId (display value)


setFixedValues :: NodeId -> Value -> Global.State -> (Global.State, IO ())
setFixedValues nodeId value state = (newState, io) where
    ports = getDestinationPorts nodeId $ state ^. graph
    newState = setFixedValue 0.5 (head ports) state
    io = return ()

-- f :: PortRef -> (UINode.Node, Int)

getDestinationPorts :: NodeId -> Graph.State -> [PortRef]
getDestinationPorts nodeId graph = fmap (^. destination) $ connectionsStartingWithNode nodeId graph


setFixedValue :: Double -> PortRef -> Global.State -> Global.State
setFixedValue val portRef state = state


instance ActionUIUpdater Reaction where
    updateUI (WithState (PerformIO act) st) = act
