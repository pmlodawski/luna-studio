module Reactive.Plugins.Core.Action.State.Camera where


import           Control.Lens
import           Data.Default
import           Data.Monoid

import           Object.Object
import           Utils.PrettyPrinter


data State = State { _camPan    :: Point
                   , _camFactor :: Double
                   } deriving (Eq, Show)

makeLenses ''State

instance Default State where
    def = State def def

instance PrettyPrinter State where
    display (State camPan camFactor) = "cS( " <> show camPan <> " " <> show camFactor <> " )"
