module Reactive.State.Global where


import           Utils.PreludePlus
import           Utils.Vector

import           Data.Aeson                    (ToJSON)
import           Reactive.Commands.Command     (Command, performIO)

import           Batch.Workspace
import qualified Event.Event                   as Event
import qualified Reactive.State.Camera         as Camera
import qualified Reactive.State.Connect        as Connect
import qualified Reactive.State.ConnectionPen  as ConnectionPen
import qualified Reactive.State.Drag           as Drag
import qualified Reactive.State.Graph          as Graph
import qualified Reactive.State.MultiSelection as MultiSelection
import qualified Reactive.State.UIElements     as UIElements
import qualified Reactive.State.UIRegistry     as UIRegistry

foreign import javascript unsafe "{}" defJsState :: Event.JSState

data State = State { _mousePos          :: Vector2 Int
                   , _graph             :: Graph.State
                   , _camera            :: Camera.State
                   , _multiSelection    :: MultiSelection.State
                   , _drag              :: Drag.State
                   , _connect           :: Connect.State
                   , _uiRegistry        :: UIRegistry.State
                   , _connectionPen     :: ConnectionPen.State
                   , _workspace         :: Workspace
                   , _uiElements        :: UIElements.State
                   , _lastEvent         :: Maybe Event.Event
                   , _eventNum          :: Int
                   , _jsState           :: Event.JSState
                   } deriving (Eq, Show, Generic)

instance ToJSON State

makeLenses ''State

initialState :: State
initialState = State (Vector2 200 200) def def def def def def def def def def def defJsState

inRegistry :: Command UIRegistry.State a -> Command State a
inRegistry = zoom uiRegistry
