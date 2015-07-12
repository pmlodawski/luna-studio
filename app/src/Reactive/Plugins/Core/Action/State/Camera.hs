module Reactive.Plugins.Core.Action.State.Camera where


import           Control.Lens
import           Data.Default
import           Data.Monoid

import           Object.Object
import           Utils.Vector
import           Utils.PrettyPrinter


data DragHistory = DragHistory { _dragStartPos    :: Vector2 Int
                               , _dragPreviousPos :: Vector2 Int
                               , _dragCurrentPos  :: Vector2 Int
                               } deriving (Eq, Show)


data Camera = Camera { _screenSize :: Vector2 Int
                     , _pan        :: Vector2 Double
                     , _factor     :: Double
                     } deriving (Eq, Show)

data State = State { _camera   :: Camera
                   , _history  :: Maybe DragHistory
                   } deriving (Eq, Show)

makeLenses ''State
makeLenses ''Camera
makeLenses ''DragHistory


instance Default Camera where
    def = Camera def def 1.0

instance PrettyPrinter Camera where
    display (Camera screenSize pan factor) = "( " <> display screenSize <>
                                             " "  <> display factor <>
                                             " "  <> display factor <>
                                             " )"

instance Default State where
    def = State def def

instance PrettyPrinter State where
    display (State camera history) = "cS( " <> display camera <>
                                     " "    <> display history <>
                                     " )"

instance PrettyPrinter DragHistory where
    display (DragHistory start prev curr) = display start <> " " <> display prev <> " " <> display curr
