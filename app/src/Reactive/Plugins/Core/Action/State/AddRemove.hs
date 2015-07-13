module Reactive.Plugins.Core.Action.State.AddRemove where


import           Control.Lens
import           Data.Default
import           Data.Monoid

import           Object.Object
import           Object.Node
import           Utils.PrettyPrinter

data State = State { _toRemoveIds  :: NodeIdCollection
                   } deriving (Eq, Show)


makeLenses ''State

instance Default State where
    def = State def

instance PrettyPrinter State where
    display (State toRemoveIds) = "arS(" <> display toRemoveIds <> ")"
