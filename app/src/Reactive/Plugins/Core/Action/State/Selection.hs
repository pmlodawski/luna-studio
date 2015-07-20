module Reactive.Plugins.Core.Action.State.Selection where


import           Utils.PreludePlus

import           Object.Object


data State = State { _nodeIds :: NodeIdCollection
                   } deriving (Eq, Show)

makeLenses ''State

instance Default State where
    def = State def

instance PrettyPrinter State where
    display (State nodeIds) = "sS(" <> display nodeIds <> ")"
