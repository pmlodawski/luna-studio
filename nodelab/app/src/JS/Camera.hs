module JS.Camera where

import           Utils.PreludePlus
import           Utils.Vector

import           JS.Bindings


-- -1      -  +1      NormalizedGl (Cartesian)
-- -scr/2  -  +scr/2  Gl           (Cartesian)
-- -x      -  +x      Workspace    (Cartesian possibly panned and zoomed)
--  0      -   scr    Screen

data Camera = Camera { _screenSize :: Vector2 Int
                     , _pan        :: Vector2 Double
                     , _factor     :: Double
                     } deriving (Eq, Show)

makeLenses ''Camera


instance PrettyPrinter Camera where
    display (Camera screenSize pan factor) = "( " <> display screenSize <>
                                             " "  <> display pan <>
                                             " "  <> display factor <>
                                             " )"

instance Default Camera where
    def = Camera def def 1.0

screenToGl :: Vector2 Int -> Vector2 Int -> Vector2 Double
screenToGl (Vector2 screenSizeX screenSizeY) (Vector2 x y) = Vector2
    ( fromIntegral x - (fromIntegral screenSizeX) / 2.0)
    ( fromIntegral y - (fromIntegral screenSizeY) / 2.0)


screenToNormalizedGl :: Vector2 Int -> Vector2 Int -> Vector2 Double
screenToNormalizedGl (Vector2 screenSizeX screenSizeY) (Vector2 x y) = Vector2
    ( (fromIntegral x / fromIntegral screenSizeX) * 2.0 - 1.0)
    (-(fromIntegral y / fromIntegral screenSizeY) * 2.0 + 1.0)


glToWorkspace :: Camera -> Vector2 Double -> Vector2 Double
glToWorkspace (Camera _ pan factor) (Vector2 xGl yGl) = Vector2
    (xGl / factor + pan ^. x)
    (yGl / factor + pan ^. y)


screenToWorkspace :: Camera -> Vector2 Int -> Vector2 Double
screenToWorkspace camera pos =
    glToWorkspace camera $ screenToGl (camera ^. screenSize) pos


workspaceToScreen :: Camera -> Vector2 Double -> Vector2 Int
workspaceToScreen (Camera (Vector2 screenSizeX screenSizeY) pan factor) (Vector2 xWs yWs) = Vector2
    (round (( xWs - pan ^. x) * factor + (fromIntegral screenSizeX) / 2.0))
    (round (( yWs - pan ^. y) * factor + (fromIntegral screenSizeY) / 2.0))


workspaceToGl :: Camera -> Vector2 Double -> Vector2 Double
workspaceToGl camera pos =
    screenToGl (camera ^. screenSize) $ workspaceToScreen camera pos
