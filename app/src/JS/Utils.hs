module JS.Utils where

import Control.Lens

import JS.Bindings
import Object.Object

-- -1      -  +1      NormalizedGl (Cartesian)
-- -scr/2  -  +scr/2  Gl           (Cartesian)
-- -x      -  +x      Workspace    (Cartesian possibly panned and zoomed)
--  0      -   scr    Screen


screenToGl :: Point -> Point -> (Double, Double)
screenToGl (Point screenSizeX screenSizeY) (Point x y) =
    ( fromIntegral x - (fromIntegral $ screenSizeX) / 2.0,
     -fromIntegral y + (fromIntegral $ screenSizeY) / 2.0)


screenToNormalizedGl :: Point -> Point -> (Double, Double)
screenToNormalizedGl (Point screenSizeX screenSizeY) (Point x y) =
    ( (fromIntegral x / fromIntegral screenSizeX) * 2.0 - 1.0,
     -(fromIntegral y / fromIntegral screenSizeY) * 2.0 + 1.0)


glToWorkspace :: Double -> (Double, Double) -> (Double, Double) -> (Double, Double)
glToWorkspace camFactor (camPanX, camPanY) (x, y) =
    (x / camFactor + camPanX,
     y / camFactor + camPanY)


screenToWorkspace :: Point -> Double -> (Double, Double) -> Point -> (Double, Double)
screenToWorkspace screenSize camFactor camPan pos =
    glToWorkspace camFactor camPan $ screenToGl screenSize pos


workspaceToScreen :: Double -> (Double, Double) -> (Double, Double) -> (Double, Double)
workspaceToScreen camFactor (camPanX, camPanY) (x, y) =
    (x / camFactor + camPanX,
     y / camFactor + camPanY)

