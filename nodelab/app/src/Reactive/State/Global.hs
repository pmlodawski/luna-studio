module Reactive.State.Global where


import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object


import           Batch.Workspace
import qualified Reactive.State.Camera            as Camera
import qualified Reactive.State.Graph             as Graph
import qualified Reactive.State.Selection         as Selection
import qualified Reactive.State.MultiSelection    as MultiSelection
import qualified Reactive.State.Drag              as Drag
import qualified Reactive.State.Connect           as Connect
import qualified Reactive.State.NodeSearcher      as NodeSearcher
import qualified Reactive.State.UIRegistry        as UIRegistry
import qualified Reactive.State.ConnectionPen     as ConnectionPen

import qualified Reactive.State.Sandbox           as Sandbox

data State = State { _iteration      :: Integer
                   , _mousePos       :: Vector2 Int
                   , _graph          :: Graph.State
                   , _camera         :: Camera.State
                   , _selection      :: Selection.State
                   , _multiSelection :: MultiSelection.State
                   , _drag           :: Drag.State
                   , _connect        :: Connect.State
                   , _nodeSearcher   :: NodeSearcher.State
                   , _uiRegistry     :: UIRegistry.State State
                   , _sandbox        :: Sandbox.State
                   , _connectionPen  :: ConnectionPen.State
                   , _workspace      :: Workspace
                   } deriving (Eq, Show)

makeLenses ''State

initialState :: Workspace -> State
initialState workspace = State def def def def def def def def def def def def workspace

instance PrettyPrinter State where
    display (State iteration mousePos graph camera selection multiSelection drag connect nodeSearcher uiRegistry sandbox pen workspace)
        = "gS(" <> display iteration
         <> " " <> display mousePos
         <> " " <> display graph
         <> " " <> display camera
         <> " " <> display selection
         <> " " <> display multiSelection
         <> " " <> display drag
         <> " " <> display connect
         <> " " <> display nodeSearcher
         <> " " <> display uiRegistry
         <> " " <> display sandbox
         <> " " <> display pen
         <> " " <> display workspace
         <> ")"

genNodeId :: State -> NodeId
genNodeId state = Graph.genNodeId $ state ^. graph
