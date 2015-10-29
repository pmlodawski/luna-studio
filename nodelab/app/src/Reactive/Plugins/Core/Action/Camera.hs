{-# LANGUAGE NamedFieldPuns #-}

module Reactive.Plugins.Core.Action.Camera where

import           Utils.PreludePlus
import           Utils.Vector

import           JS.Camera

import           Object.Node
import           Event.Event (Event(Keyboard, Mouse))
import           Event.Keyboard (KeyMods(..), ctrl)
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    (MouseButton(..))
import qualified Event.Mouse    as Mouse
import           Reactive.Plugins.Core.Action
import           Reactive.State.Camera    (DragHistory(..))
import qualified Reactive.State.Camera    as Camera
import qualified Reactive.State.Graph     as Graph
import qualified Reactive.State.Global    as Global

import Reactive.Commands.Command (Command, ioCommand, execCommand, performIO)


toAction :: Event -> Maybe (Command Global.State ())
toAction evt = (>> syncCameraM) <$> toAction' evt

toAction' :: Event -> Maybe (Command Global.State ())
toAction' (Mouse (Mouse.Event evt pos RightButton  _ _)) = Just $ zoomDrag evt pos
toAction' (Mouse (Mouse.Event evt pos MiddleButton _ _)) = Just $ panDrag  evt pos

toAction' (Mouse (Mouse.Event (Mouse.Wheel delta) pos _ KeyMods {_ctrl = False} _)) = Just $ panCamera delta
toAction' (Mouse (Mouse.Event (Mouse.Wheel delta) pos _ KeyMods {_ctrl = True} _))  = Just $ wheelZoom pos delta

toAction' (Keyboard (Keyboard.Event Keyboard.Press char mods)) = case char of
    '='   -> Just $ zoomIn
    '+'   -> Just $ zoomIn
    '-'   -> Just $ zoomOut
    '0'   -> Just $ if mods ^. ctrl then resetZoom
                                    else autoZoom
    _     -> Nothing

toAction' (Keyboard (Keyboard.Event Keyboard.Down char KeyMods { _ctrl = True })) = case char of
    '\37' -> Just panLeft
    '\39' -> Just panRight
    '\38' -> Just panUp
    '\40' -> Just panDown
    _     -> Nothing
toAction' _ = Nothing

minCamFactor   =   0.2
maxCamFactor   =   8.0
dragZoomSpeed  = 512.0
wheelZoomSpeed =  64.0
panStep        =  50.0
zoomFactorStep =   1.1

restrictCamFactor = min maxCamFactor . max minCamFactor

panCamera :: Vector2 Double -> Command Global.State ()
panCamera delta = do
    camFactor <- use $ Global.camera . Camera.camera . Camera.factor
    Global.camera . Camera.camera . Camera.pan += ((/ camFactor) <$> delta)

panLeft  = panCamera $ Vector2 (-panStep)         0
panRight = panCamera $ Vector2   panStep          0
panUp    = panCamera $ Vector2        0   (-panStep)
panDown  = panCamera $ Vector2        0     panStep

setZoom :: Double -> Command Global.State ()
setZoom newFactor = Global.camera . Camera.camera . Camera.factor .= (restrictCamFactor newFactor)

resetZoom :: Command Global.State ()
resetZoom = Global.camera . Camera.camera . Camera.factor .= 1.0

autoZoom :: Command Global.State ()
autoZoom = do
    nodes             <- use $ Global.graph  . Graph.nodes
    screenSize'       <- use $ Global.camera . Camera.camera . Camera.screenSize

    let padding        = Vector2 80.0 80.0
        screenSize     = fromIntegral <$> screenSize'
        minXY          = -padding + (Vector2 (minimum $ (^. nodePos . x) <$> nodes) (minimum $ (^. nodePos . y) <$> nodes))
        maxXY          =  padding + (Vector2 (maximum $ (^. nodePos . x) <$> nodes) (maximum $ (^. nodePos . y) <$> nodes))
        spanXY         = maxXY - minXY
        zoomFactorXY   = Vector2 (screenSize ^. x / spanXY ^. x) (screenSize ^. y / spanXY ^. y)
        zoomFactor     = min (zoomFactorXY ^. x) (zoomFactorXY ^. y)
        zoomPan        = minXY + ((/2.0) <$> spanXY)

    setZoom zoomFactor
    Global.camera . Camera.camera . Camera.pan    .= zoomPan

zoomIn :: Command Global.State ()
zoomIn = do
    factor <- use $ Global.camera . Camera.camera . Camera.factor
    setZoom $ factor * zoomFactorStep

zoomOut :: Command Global.State ()
zoomOut = do
    factor <- use $ Global.camera . Camera.camera . Camera.factor
    setZoom $ factor / zoomFactorStep

wheelZoom :: Vector2 Int -> Vector2 Double -> Command Global.State ()
wheelZoom pos delta = do
    camera         <- use $ Global.camera . Camera.camera
    let delta'      = (- delta ^. x - delta ^. y) / wheelZoomSpeed
        workspace   = Camera.screenToWorkspace camera pos
    fixedPointZoom pos workspace delta'

fixedPointZoom :: Vector2 Int -> Vector2 Double -> Double -> Command Global.State ()
fixedPointZoom fpScreen fpWorkspace delta = do
    oldFactor           <- use $ Global.camera . Camera.camera . Camera.factor

    let newFactor        = oldFactor * (1.0 + delta)
    setZoom newFactor

    oldCamera           <- use $ Global.camera . Camera.camera
    let nonPannedCamera  = oldCamera & Camera.factor .~ (restrictCamFactor newFactor)
                                     & Camera.pan    .~ Vector2 0.0 0.0
        newWorkspace     = Camera.screenToWorkspace nonPannedCamera fpScreen
        newPan           = -newWorkspace + fpWorkspace

    Global.camera . Camera.camera . Camera.pan .= newPan

panDrag :: Mouse.Type -> Vector2 Int -> Command Global.State ()
panDrag Mouse.Pressed pos = do
    Global.camera . Camera.history ?= PanDragHistory pos

panDrag Mouse.Moved   pos = do
    history <- use $ Global.camera . Camera.history
    case history of
        Just (PanDragHistory prev) -> do
            Global.camera . Camera.history ?= PanDragHistory pos
            panCamera $ fromIntegral <$> prev - pos
        _                          -> return ()

panDrag Mouse.Released _ = do
    Global.camera . Camera.history .= Nothing

panDrag _ _ = return ()

zoomDrag :: Mouse.Type -> Vector2 Int -> Command Global.State ()
zoomDrag Mouse.Pressed screenPos = do
    camera <- use $ Global.camera . Camera.camera
    let workspacePos = Camera.screenToWorkspace camera screenPos
    Global.camera . Camera.history ?= ZoomDragHistory screenPos screenPos workspacePos

zoomDrag Mouse.Moved   pos = do
    history <- use $ Global.camera . Camera.history
    case history of
        Just (ZoomDragHistory prev fpScreen fpWorkspace) -> do
            Global.camera . Camera.history ?= ZoomDragHistory pos fpScreen fpWorkspace
            let deltaV = fromIntegral <$> (prev - pos)
                delta  = (-deltaV ^. x + deltaV ^. y) / dragZoomSpeed
            fixedPointZoom fpScreen fpWorkspace delta
        _                           -> return ()

zoomDrag Mouse.Released _ = do
    Global.camera . Camera.history .= Nothing

zoomDrag _ _ = return ()

syncCamera :: Global.State -> IO ()
syncCamera state = do
    let cPan         = state ^. Global.camera . Camera.camera . Camera.pan
        cFactor      = state ^. Global.camera . Camera.camera . Camera.factor
        screenSize   = state ^. Global.camera . Camera.camera . Camera.screenSize
        hScreen      = (/ 2.0) . fromIntegral <$> screenSize
        camLeft      = appX cameraLeft
        camRight     = appX cameraRight
        camTop       = appY cameraTop
        camBottom    = appY cameraBottom
        hX           = appX htmlX
        hY           = appY htmlY
        appX      f  = f cFactor (cPan ^. x) (hScreen ^. x)
        appY      f  = f cFactor (cPan ^. y) (hScreen ^. y)
    updateCamera cFactor camLeft camRight camTop camBottom
    updateCameraHUD 0.0 (fromIntegral $ screenSize ^. x) 0.0 (fromIntegral $ screenSize ^. y)
    updateHtmCanvasPanPos hX hY cFactor
    updateProjectionMatrix
    updateHUDProjectionMatrix

syncCameraM :: Command Global.State ()
syncCameraM = ioCommand syncCamera


cameraLeft, cameraRight, cameraTop, cameraBottom, htmlX, htmlY :: Double -> Double -> Double -> Double
cameraLeft   camFactor camPanX halfScreenX = -halfScreenX / camFactor + camPanX
cameraRight  camFactor camPanX halfScreenX =  halfScreenX / camFactor + camPanX
cameraTop    camFactor camPanY halfScreenY = -halfScreenY / camFactor + camPanY
cameraBottom camFactor camPanY halfScreenY =  halfScreenY / camFactor + camPanY
htmlX        camFactor camPanX halfScreenX =  halfScreenX - camPanX * camFactor
htmlY        camFactor camPanY halfScreenY =  halfScreenY - camPanY * camFactor

