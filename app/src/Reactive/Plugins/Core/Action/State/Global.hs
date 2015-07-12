module Reactive.Plugins.Core.Action.State.Global where


import           Data.Monoid
import           Data.Default
import           Control.Lens

import           Object.Object
import qualified Object.Node    as Node     ( position )
import           Object.Node    hiding      ( position )
import           Utils.Vector
import           Utils.PrettyPrinter

import qualified Reactive.Plugins.Core.Action.State.Camera            as Camera
import qualified Reactive.Plugins.Core.Action.State.AddRemove         as AddRemove
import qualified Reactive.Plugins.Core.Action.State.Selection         as Selection
import qualified Reactive.Plugins.Core.Action.State.Drag              as Drag
import qualified Reactive.Plugins.Core.Action.State.NodeSearcher      as NodeSearcher


data State = State { _iteration    :: Integer
                   , _mousePos     :: Vector2 Int
                   , _camera       :: Camera.State
                   , _nodes        :: NodeCollection
                   , _addRemove    :: AddRemove.State
                   , _selection    :: Selection.State
                   , _drag         :: Drag.State
                   , _nodeSearcher :: NodeSearcher.State
                   } deriving (Eq, Show)

makeLenses ''State

instance Default State where
    def = State def (Vector2 400 200) def def def def def def

instance PrettyPrinter State where
    display (State iteration mousePos camera nodes addRemove selection drag nodeSearcher) =
                                                     "gS( " <> display iteration
                                                     <> " " <> display mousePos
                                                     <> " " <> display camera
                                                     <> " " <> display nodes
                                                     <> " " <> display addRemove
                                                     <> " " <> display selection
                                                     <> " " <> display drag
                                                     <> " " <> display nodeSearcher
                                                     <> " )"

instance Monoid State where
    mempty = def
    a `mappend` b = if a ^. iteration > b ^.iteration then a else b
