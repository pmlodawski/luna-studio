module Reactive.Plugins.Core.Action.State.Camera where


import           Control.Lens
import           Data.Default
import           Data.Monoid

import           Object.Object
import           Utils.PrettyPrinter


data State = State { _camPanX     :: Double
                   , _camPanY     :: Double
                   , _camFactor   :: Double
                   , _halfScreenX :: Double
                   , _halfScreenY :: Double
                   } deriving (Eq, Show)

makeLenses ''State


instance Default State where
    def = State 0.0 0.0 1.0 800.0 450.0

instance PrettyPrinter State where
    display (State camPanX camPanY camFactor halfScreenX halfScreenY) =
                                                "cS( " <> display camPanX
                                                <> " " <> display camPanY
                                                <> " " <> display camFactor
                                                <> " " <> display halfScreenX
                                                <> " " <> display halfScreenY
                                                <> " )"
