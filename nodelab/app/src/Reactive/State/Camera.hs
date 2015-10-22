module Reactive.State.Camera where


import           Utils.PreludePlus
import           Utils.Vector


data DragHistory = DragHistory { _fixedPointPosScreen    :: Vector2 Int
                               , _fixedPointPosWorkspace :: Vector2 Double
                               , _dragPreviousPos        :: Vector2 Int
                               , _dragCurrentPos         :: Vector2 Int
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
    def = Camera (Vector2 400 200) def 1.0

instance PrettyPrinter Camera where
    display (Camera screenSize pan factor) = "(" <> display screenSize <>
                                             " " <> display pan <>
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

glToWorkspace :: Camera -> Vector2 Double -> Vector2 Double
glToWorkspace (Camera _ pan factor) (Vector2 xGl yGl) = Vector2
    (xGl / factor + pan ^. x)
    (yGl / factor + pan ^. y)

screenToGl :: Vector2 Int -> Vector2 Int -> Vector2 Double
screenToGl (Vector2 screenSizeX screenSizeY) (Vector2 x y) = Vector2
    ( fromIntegral x - (fromIntegral screenSizeX) / 2.0)
    ( fromIntegral y - (fromIntegral screenSizeY) / 2.0)

screenToWorkspace :: Camera -> Vector2 Int -> Vector2 Double
screenToWorkspace camera pos =
    glToWorkspace camera $ screenToGl (camera ^. screenSize) pos
