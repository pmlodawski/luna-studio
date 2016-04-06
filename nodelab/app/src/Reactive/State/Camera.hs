module Reactive.State.Camera where


import Utils.PreludePlus
import Utils.Vector
import Reactive.Commands.Command (Command)
import Data.Aeson (ToJSON)

data DragHistory =  PanDragHistory  { _panPreviousPos         :: Vector2 Int }
                 | ZoomDragHistory  { _zoomPreviousPos        :: Vector2 Int
                                    , _zoomFPScreen           :: Vector2 Int
                                    , _zoomFPWorkspace        :: Vector2 Double }
                 deriving (Eq, Show, Generic)

data Camera = Camera { _screenSize :: Vector2 Int
                     , _windowSize :: Vector2 Int
                     , _pan        :: Vector2 Double
                     , _factor     :: Double
                     } deriving (Eq, Show, Generic)

data State = State { _camera   :: Camera
                   , _history  :: Maybe DragHistory
                   } deriving (Eq, Show, Generic)

makeLenses ''State
makeLenses ''Camera
makeLenses ''DragHistory

instance ToJSON State
instance ToJSON Camera
instance ToJSON DragHistory

instance Default Camera where
    def = Camera (Vector2 400 200) (Vector2 400 200) def 1.0

instance Default State where
    def = State def def

glToWorkspace :: Camera -> Vector2 Double -> Vector2 Double
glToWorkspace (Camera _ _ pan factor) (Vector2 xGl yGl) = Vector2
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
