{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

module JS.Camera where

import           Utils.PreludePlus
import           Utils.Vector
import           GHCJS.Foreign

foreign import javascript unsafe "common.camera.updateProjectionMatrix()"
    updateProjectionMatrix :: IO ()

foreign import javascript unsafe "common.cameraHUD.updateProjectionMatrix()"
    updateHUDProjectionMatrix :: IO ()

foreign import javascript unsafe "app.updateScreenSize($1, $2)"
    updateScreenSize :: Int -> Int -> IO ()

foreign import javascript unsafe "app.updateHtmCanvasPanPos($1, $2, $3)"
    updateHtmCanvasPanPos :: Double -> Double -> Double -> IO ()

foreign import javascript unsafe "app.updateCamera($1, $2, $3, $4, $5)"
    updateCamera :: Double -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript unsafe "app.updateCameraHUD($1, $2, $3, $4)"
    updateCameraHUD :: Double -> Double -> Double -> Double -> IO ()

