module Reactive.Plugins.Core.Action.State.AddRemove where


import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object

data State = State { _toRemoveIds  :: NodeIdCollection
                   } deriving (Eq, Show)


makeLenses ''State

instance Default State where
    def = State def

instance PrettyPrinter State where
    display (State toRemoveIds) = "arS(" <> display toRemoveIds <> ")"
