module Reactive.Plugins.Core.Action.State.Camera where


import           Control.Lens
import           Data.Default
import           Data.Monoid

import           Object.Object
import           Utils.PrettyPrinter


data DragHistory = DragHistory { _dragStartPos    :: Vector2 Int
                               , _dragPreviousPos :: Vector2 Int
                               , _dragCurrentPos  :: Vector2 Int
                               } deriving (Eq, Show)


data State = State { _camPanX     :: Double
                   , _camPanY     :: Double
                   , _camFactor   :: Double
                   , _history     :: Maybe DragHistory
                   } deriving (Eq, Show)

makeLenses ''State
makeLenses ''DragHistory


instance Default State where
    def = State 0.0 0.0 1.0 def

instance PrettyPrinter State where
    display (State camPanX camPanY camFactor history) =
                                                "cS( " <> display camPanX
                                                <> " " <> display camPanY
                                                <> " " <> display camFactor
                                                <> " " <> display history
                                                <> " )"

instance PrettyPrinter DragHistory where
    display (DragHistory start prev curr) = display start <> " " <> display prev <> " " <> display curr
