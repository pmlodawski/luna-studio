module Reactive.Plugins.Core.Action.State.Global where


import           Data.Monoid
import           Data.Default
import           Control.Lens

import           Object.Object
import qualified Object.Node    as Node     ( position )
import           Object.Node    hiding      ( position )
import           Utils.PrettyPrinter

import qualified Reactive.Plugins.Core.Action.State.Selection as Selection
import qualified Reactive.Plugins.Core.Action.State.Drag      as Drag


-- data CommonState = CommonState { _nodes :: NodeCollection } deriving (Eq, Show)

data State = State { _nodes     :: NodeCollection
                   , _selection :: Selection.State
                   , _drag      :: Drag.State
                   } deriving (Eq, Show)

-- makeLenses ''CommonState
makeLenses ''State

instance Default State where
    def = State def def def

instance PrettyPrinter State where
    display (State nodes selection drag) = "gS( " <> display nodes <> " " <> display selection <> " " <> display drag <> " )"
