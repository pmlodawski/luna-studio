module Reactive.State.Camera where


import           Utils.PreludePlus
import           Utils.Vector
import           Reactive.Commands.Command (Command)

data DragHistory =  PanDragHistory  { _panPreviousPos         :: Vector2 Int }
                 | ZoomDragHistory  { _zoomPreviousPos        :: Vector2 Int
                                    , _zoomFPScreen           :: Vector2 Int
                                    , _zoomFPWorkspace        :: Vector2 Double }
                 deriving (Eq, Show)

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
    display (PanDragHistory prev)  = "pan: " <> display prev
    display (ZoomDragHistory prev fps fpw) = "zoom: " <> display prev <> " fps: " <> display fps <> " fpw: " <> display fpw

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

screenToWorkspaceM :: Vector2 Int -> Command State (Vector2 Double)
screenToWorkspaceM pos = do
    camera  <- use camera
    return $ screenToWorkspace camera pos
