module Reactive.Plugins.Core.Action.State.Global where


import           Data.Monoid
import           Data.Default
import           Control.Lens

import           Object.Object
import qualified Object.Node    as Node     ( position )
import           Object.Node    hiding      ( position )
import           Utils.PrettyPrinter

import qualified Reactive.Plugins.Core.Action.State.AddRemove as AddRemove
import qualified Reactive.Plugins.Core.Action.State.Selection as Selection
import qualified Reactive.Plugins.Core.Action.State.Drag      as Drag


-- data CommonState = CommonState { _nodes :: NodeCollection } deriving (Eq, Show)

data State = State { _iteration :: Integer
                   , _mousePos  :: Point
                   , _nodes     :: NodeCollection
                   , _addRemove :: AddRemove.State
                   , _selection :: Selection.State
                   , _drag      :: Drag.State
                   } deriving (Eq, Show)

-- makeLenses ''CommonState
makeLenses ''State

instance Default State where
    def = State def def def def def def

instance PrettyPrinter State where
    display (State iteration mousePos nodes addRemove selection drag) =
                                                     "gS( " <> display iteration
                                                     <> " " <> display mousePos
                                                     <> " " <> display nodes
                                                     <> " " <> display addRemove
                                                     <> " " <> display selection
                                                     <> " " <> display drag
                                                     <> " )"

instance Monoid State where
    mempty = def
    a `mappend` b = if a ^. iteration > b ^.iteration then a else b
