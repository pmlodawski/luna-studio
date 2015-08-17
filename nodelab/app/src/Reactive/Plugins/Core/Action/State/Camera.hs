module Reactive.Plugins.Core.Action.State.Camera where


import           Utils.PreludePlus
import           Utils.Vector


data DragHistory = DragHistory { _fixedPointPosScreen    :: Vector2 Int
                               , _fixedPointPosWorkspace :: Vector2 Double
                               , _dragPreviousPos        :: Vector2 Int
                               , _dragCurrentPos         :: Vector2 Int
                               } deriving (Eq, Show)



data Camera = Camera { _pan        :: Vector2 Double
                     , _factor     :: Double
                     } deriving (Eq, Show)

data State = State { _camera   :: Camera
                   , _history  :: Maybe DragHistory
                   } deriving (Eq, Show)

makeLenses ''State
makeLenses ''Camera
makeLenses ''DragHistory


instance Default Camera where
    def = Camera def 1.0

instance PrettyPrinter Camera where
    display (Camera pan factor) = "(" <> display pan <>
                                  " " <> display factor <>
                                  ")"

instance Default State where
    def = State def def

instance PrettyPrinter State where
    display (State camera history) = "cS(" <> display camera <>
                                     " "   <> display history <>
                                     " )"

instance PrettyPrinter DragHistory where
    display (DragHistory fixedS fixedW prev curr) = display fixedS <> " " <> display fixedW <> " " <> display prev <> " " <> display curr
