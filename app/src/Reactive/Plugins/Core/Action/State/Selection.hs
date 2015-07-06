module Reactive.Plugins.Core.Action.State.Selection where


import           Control.Lens
import           Data.Default
import           Data.Monoid

import           Object.Node
import           Utils.PrettyPrinter


data State = State { _nodeIds :: NodeIdCollection
                   } deriving (Eq, Show)

makeLenses ''State

instance Default State where
    def = State def

instance PrettyPrinter State where
    display (State nodeIds) = "sS( " <> display nodeIds <> " )"
