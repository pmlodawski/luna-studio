module Reactive.Plugins.Core.Action.Camera where

import           Utils.PreludePlus
import           Utils.Vector

import           JS.Bindings
import qualified JS.NodeGraph   as UI
import qualified JS.Camera      as Camera

import           Object.Node
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event )
import qualified Event.Mouse    as Mouse
import           Object.UITypes
import           Event.Event
import           Event.WithObjects
import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.Camera
import qualified Reactive.Plugins.Core.Action.State.Graph     as Graph
import qualified Reactive.Plugins.Core.Action.State.Global    as Global


data DragType = StartDrag
              | Dragging
              | StopDrag
              deriving (Eq, Show)

data MouseActionType = Zoom | Pan deriving (Eq, Show)

data KeyActionType = ResetZoom
                   | AutoZoom
                   | ZoomIn
                   | ZoomOut
                   | PanLeft
                   | PanRight
                   | PanUp
                   | PanDown
                   deriving (Eq, Show)

data Action = KeyAction   { _keyActionType :: KeyActionType }
            | MouseAction { _actionType    :: MouseActionType
                          , _dragType      :: DragType
                          , _zoomPos       :: Vector2 Int
                          }
            deriving (Eq, Show)


makeLenses ''Action


instance PrettyPrinter DragType where
    display = show

instance PrettyPrinter MouseActionType where
    display = show

instance PrettyPrinter KeyActionType where
    display = show

instance PrettyPrinter Action where
    display (KeyAction tpe)           = "cA(Key "   <> display tpe <> ")"
    display (MouseAction act tpe pos) = "cA(Mouse " <> display act <> " " <> display tpe <> " " <> display pos <> ")"


toAction :: Event Node -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos button keyMods _)) = case button of
    RightButton        -> case tpe of
        Mouse.Pressed  -> case keyMods of
           (KeyMods False False False False) -> Just (MouseAction Zoom StartDrag pos)
           _                                 -> Nothing
        Mouse.Released -> Just (MouseAction Zoom StopDrag pos)
        Mouse.Moved    -> Just (MouseAction Zoom Dragging pos)
        _              -> Nothing
    MiddleButton       -> case tpe of
        Mouse.Pressed  -> case keyMods of
           (KeyMods False False False False) -> Just (MouseAction Pan StartDrag pos)
           _                                 -> Nothing
        Mouse.Released -> Just (MouseAction Pan StopDrag pos)
        Mouse.Moved    -> Just (MouseAction Pan Dragging pos)
        _              -> Nothing
    _                  -> Nothing
toAction (Keyboard (Keyboard.Event Keyboard.Press char)) = case char of
    '='   -> Just $ KeyAction ZoomIn
    '+'   -> Just $ KeyAction ZoomIn
    '-'   -> Just $ KeyAction ZoomOut
    'z'   -> Just $ KeyAction ResetZoom
    '0'   -> Just $ KeyAction AutoZoom
    _     -> Nothing
toAction (Keyboard (Keyboard.Event Keyboard.Down char)) = case char of
    '\37' -> Just $ KeyAction PanLeft
    '\39' -> Just $ KeyAction PanRight
    '\38' -> Just $ KeyAction PanUp
    '\40' -> Just $ KeyAction PanDown
    _     -> Nothing
toAction _ = Nothing

minCamFactor   =   0.2
maxCamFactor   =   8.0
dragZoomSpeed  = 512.0
panStep        =  10.0
zoomFactorStep =   1.1

restrictCamFactor = min maxCamFactor . max minCamFactor

instance ActionStateUpdater Action where
    execSt newActionCandidate oldState =
        case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI  NoAction newState
        where
        newState                       = oldState &  Global.iteration                +~ 1
                                                  &  Global.camera . camera . pan    .~ newCamPan
                                                  &  Global.camera . camera . factor .~ newCamFactor
                                                  &  Global.camera . history         .~ newDrag
        oldCam                         = oldState ^. Global.camera
        oldCamPan                      = oldCam ^. camera . pan
        oldCamFactor                   = oldCam ^. camera . factor
        oldDrag                        = oldCam ^. history
        newAction                      = Just newActionCandidate
        newCamPan                      = case newActionCandidate of
            MouseAction Zoom _ _      -> zoomPan
            MouseAction Pan  _ _      -> oldCamPan + dragPan
            KeyAction keyAct          -> Vector2 newCamPanX newCamPanY where
                newCamPanX            = case keyAct of
                    PanLeft           -> oldCamPan ^. x - panStep / oldCamFactor
                    PanRight          -> oldCamPan ^. x + panStep / oldCamFactor
                    AutoZoom          -> autoZoomPan ^. x
                    _                 -> oldCamPan ^. x
                newCamPanY             = case keyAct of
                    PanUp             -> oldCamPan ^. y - panStep / oldCamFactor
                    PanDown           -> oldCamPan ^. y + panStep / oldCamFactor
                    AutoZoom          -> autoZoomPan ^. y
                    _                 -> oldCamPan ^. y
        newCamFactor                   = case newActionCandidate of
            KeyAction ResetZoom       -> 1.0
            KeyAction ZoomIn          -> max minCamFactor $ oldCamFactor * zoomFactorStep
            KeyAction ZoomOut         -> min maxCamFactor $ oldCamFactor / zoomFactorStep
            KeyAction AutoZoom        -> restrictCamFactor autoZoomFactor
            MouseAction Zoom _ _      -> restrictCamFactor newCamFactorCandidate
            _                         -> oldCamFactor
        newDrag                        = case newActionCandidate of
            MouseAction act tpe point -> case tpe of
                StartDrag             -> Just $ DragHistory point (Camera.screenToWorkspace camera point) point point where
                    camera = Global.toCamera oldState
                Dragging              -> case oldDrag of
                    Just oldDragState -> Just $ oldDragState & dragPreviousPos .~ (oldDragState ^. dragCurrentPos)
                                                             & dragCurrentPos  .~ point
                    Nothing           -> Nothing
                StopDrag              -> Nothing
            _                         -> Nothing
        (autoZoomPan, autoZoomFactor)  = (autoZoomPan, autoZoomFactor) where
            nodes          = oldState ^. Global.graph . Graph.nodeList
            screenSize     = fromIntegral <$> oldState ^. Global.screenSize
            minXY          = -padding + (Vector2 (minimum $ (^. nodePos . x) <$> nodes) (minimum $ (^. nodePos . y) <$> nodes))
            maxXY          =  padding + (Vector2 (maximum $ (^. nodePos . x) <$> nodes) (maximum $ (^. nodePos . y) <$> nodes))
            spanXY         = maxXY - minXY
            zoomFactorXY   = Vector2 (screenSize ^. x / spanXY ^. x) (screenSize ^. y / spanXY ^. y)
            autoZoomFactor = min (zoomFactorXY ^. x) (zoomFactorXY ^. y)
            autoZoomPan    = minXY + ((/2.0) <$> spanXY)
            padding        = Vector2 80.0 80.0
        (zoomPan, newCamFactorCandidate)  = case newDrag of
                Just drag                -> (zoomPan, newCamFactorCandidate) where
                    camFactorDelta        = (delta ^. x + delta ^. y) / dragZoomSpeed
                    newCamFactorCandidate = oldCamFactor * (1.0 + camFactorDelta)
                    delta                 = fromIntegral <$> (negateSnd $ drag ^. dragCurrentPos - drag ^. dragPreviousPos)
                    camera                = Global.toCamera oldState
                    oldScreen             = drag ^. fixedPointPosScreen
                    oldWorkspace          = drag ^. fixedPointPosWorkspace
                    newWorkspace          = Camera.screenToWorkspace nonPannedCamera oldScreen
                    nonPannedCamera       = camera & Camera.factor .~ (restrictCamFactor newCamFactorCandidate)
                                                   & Camera.pan    .~ Vector2 0.0 0.0
                    zoomPan               = -newWorkspace + oldWorkspace
                Nothing                  -> (oldCamPan, oldCamFactor)
        dragPan                           = case newDrag of
                Just drag                -> prevWorkspace - currWorkspace where
                    camera                = Global.toCamera oldState
                    currWorkspace         = Camera.screenToWorkspace camera $ drag ^. dragCurrentPos
                    prevWorkspace         = Camera.screenToWorkspace camera $ drag ^. dragPreviousPos
                Nothing                  -> Vector2 0.0 0.0


instance ActionUIUpdater Action where
    updateUI (WithState _ state) = syncCamera state


syncCamera :: Global.State -> IO ()
syncCamera state = do
    let cPan         = state ^. Global.camera . camera . pan
        cFactor      = state ^. Global.camera . camera . factor
        screenSize   = state ^. Global.screenSize
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


cameraLeft, cameraRight, cameraTop, cameraBottom, htmlX, htmlY :: Double -> Double -> Double -> Double
cameraLeft   camFactor camPanX halfScreenX = -halfScreenX / camFactor + camPanX
cameraRight  camFactor camPanX halfScreenX =  halfScreenX / camFactor + camPanX
cameraTop    camFactor camPanY halfScreenY = -halfScreenY / camFactor + camPanY
cameraBottom camFactor camPanY halfScreenY =  halfScreenY / camFactor + camPanY
htmlX        camFactor camPanX halfScreenX =  halfScreenX - camPanX * camFactor
htmlY        camFactor camPanY halfScreenY =  halfScreenY - camPanY * camFactor

