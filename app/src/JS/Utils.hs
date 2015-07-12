module JS.Utils where

import Control.Lens

import JS.Bindings
import Object.Object

-- -1      -  +1      NormalizedGl (Cartesian)
-- -scr/2  -  +scr/2  Gl           (Cartesian)
-- -x      -  +x      Workspace    (Cartesian possibly panned and zoomed)
--  0      -   scr    Screen


screenToGl :: Vector2 Int -> Vector2 Int -> Vector2 Double
screenToGl (Vector2 screenSizeX screenSizeY) (Vector2 x y) = Vector2
    ( fromIntegral x - (fromIntegral $ screenSizeX) / 2.0)
    (-fromIntegral y + (fromIntegral $ screenSizeY) / 2.0)


screenToNormalizedGl :: Vector2 Int -> Vector2 Int -> Vector2 Double
screenToNormalizedGl (Vector2 screenSizeX screenSizeY) (Vector2 x y) = Vector2
    ( (fromIntegral x / fromIntegral screenSizeX) * 2.0 - 1.0)
    (-(fromIntegral y / fromIntegral screenSizeY) * 2.0 + 1.0)


glToWorkspace :: Double -> Vector2 Double -> Vector2 Double -> Vector2 Double
glToWorkspace camFactor (Vector2 camPanX camPanY) (Vector2 x y) = Vector2
    (x / camFactor + camPanX)
    (y / camFactor + camPanY)


screenToWorkspace :: Vector2 Int -> Double -> Vector2 Double -> Vector2 Int -> Vector2 Double
screenToWorkspace screenSize camFactor camPan pos =
    glToWorkspace camFactor camPan $ screenToGl screenSize pos


workspaceToScreen :: Double -> Vector2 Double -> Vector2 Double -> Vector2 Double
workspaceToScreen camFactor (Vector2 camPanX camPanY) (Vector2 x y) = Vector2
    (x / camFactor + camPanX)
    (y / camFactor + camPanY)

