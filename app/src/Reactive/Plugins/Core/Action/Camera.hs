module Reactive.Plugins.Core.Action.Camera where

import           Prelude       hiding       ( mapM_, forM_ )
import           Data.Foldable              ( mapM_, forM_ )
import           Control.Lens
import           Control.Applicative
import           Data.Default
import           Data.Maybe
import           Data.List
import           Data.Char
import           Data.Monoid
import           Data.Function
import           System.Mem

import           JS.Bindings
import           JS.Appjs
import qualified JS.Utils       as Utils

import           Object.Object
import qualified Object.Node    as Node     ( position )
import           Object.Node    hiding      ( position )
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event )
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Event.WithObjects
import           Utils.Vector
import           Utils.Wrapper
import           Utils.PrettyPrinter
import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.Camera
import qualified Reactive.Plugins.Core.Action.State.Global    as Global


data DragType = StartDrag
              | Dragging
              | StopDrag
              deriving (Eq, Show)

data MouseActionType = Zoom | Pan deriving (Eq, Show)

data KeyActionType = ResetZoom
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
    display (KeyAction tpe)           = "cA( Key "   <> display tpe <> ")"
    display (MouseAction act tpe pos) = "cA( Mouse " <> display act <> " " <> display tpe <> " " <> display pos <> " )"


toAction :: Event Node -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos button keyMods)) = case button of
    3                  -> case tpe of
        Mouse.Pressed  -> case keyMods of
           (KeyMods False False False False) -> Just (MouseAction Zoom StartDrag pos)
           _                                 -> Nothing
        Mouse.Released -> Just (MouseAction Zoom StopDrag pos)
        Mouse.Moved    -> Just (MouseAction Zoom Dragging pos)
    2                  -> case tpe of
        Mouse.Pressed  -> case keyMods of
           (KeyMods False False False False) -> Just (MouseAction Pan StartDrag pos)
           _                                 -> Nothing
        Mouse.Released -> Just (MouseAction Pan StopDrag pos)
        Mouse.Moved    -> Just (MouseAction Pan Dragging pos)
    _                  -> Nothing
toAction (Keyboard (Keyboard.Event Keyboard.Press char)) = case char of
    '='   -> Just $ KeyAction ZoomIn
    '+'   -> Just $ KeyAction ZoomIn
    '-'   -> Just $ KeyAction ZoomOut
    'z'   -> Just $ KeyAction ResetZoom
    _     -> Nothing
toAction (Keyboard (Keyboard.Event Keyboard.Down char)) = case char of
    '\37' -> Just $ KeyAction PanLeft
    '\39' -> Just $ KeyAction PanRight
    '\38' -> Just $ KeyAction PanUp
    '\40' -> Just $ KeyAction PanDown
    _     -> Nothing
toAction _ = Nothing

minCamFactor = 0.14
maxCamFactor = 8.0


-- workspace node positions -> not changing while zooming



instance ActionStateUpdater Action where
    execSt newActionCandidate oldState =
        case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI NoAction newState
        where
        newState                       = oldState &  Global.iteration +~ 1
                                                  &  Global.camera . camera . pan    .~ newCamPan
                                                  &  Global.camera . camera . factor .~ newCamFactor
                                                  &  Global.camera . history  .~ newUpdDrag
        oldCam                         = oldState ^. Global.camera
        oldCamPan                      = oldCam ^. camera . pan
        oldCamFactor                   = oldCam ^. camera . factor
        oldDrag                        = oldCam ^. history
        newAction                      = Just newActionCandidate
        newCamPan                      = case newActionCandidate of
            MouseAction Zoom _ _      -> oldCamPan + zoomPan
            MouseAction Pan  _ _      -> oldCamPan + dragPan
            KeyAction keyAct          -> Vector2 newCamPanX newCamPanY where
                newCamPanX            = case keyAct of
                    PanLeft           -> oldCamPan ^. x - 10.0 / oldCamFactor
                    PanRight          -> oldCamPan ^. x + 10.0 / oldCamFactor
                    _                 -> oldCamPan ^. x
                newCamPanY             = case keyAct of
                    PanUp             -> oldCamPan ^. y + 10.0 / oldCamFactor
                    PanDown           -> oldCamPan ^. y - 10.0 / oldCamFactor
                    _                 -> oldCamPan ^. y
            _                         -> oldCamPan
        newCamFactor                   = case newActionCandidate of
            KeyAction ResetZoom       -> 1.0
            KeyAction ZoomIn          -> max minCamFactor $ oldCamFactor / 1.1
            KeyAction ZoomOut         -> min maxCamFactor $ oldCamFactor * 1.1
            MouseAction Zoom _ _      -> min maxCamFactor . max minCamFactor $ oldCamFactor * (1.0 + camDragFactorDelta)
            _                         -> oldCamFactor
        newDrag                        = case newActionCandidate of
            MouseAction act tpe point -> case tpe of
                StartDrag             -> Just $ DragHistory point point point
                Dragging              -> case oldDrag of
                    Just oldDragState -> Just $ DragHistory startPos prevPos point where
                        startPos       = oldDragState ^. dragStartPos
                        prevPos        = oldDragState ^. dragCurrentPos
                    Nothing           -> Nothing
                StopDrag              -> Nothing
            _                         -> Nothing
        -- TODO: 1) name the identifiers below appropriately
        --       2) fix zoomPan for mouse zooming
        --       3) why it works properly even with screenSize = (0, 0)   (uninitialized initial value)
        (camDragFactorDelta, zoomPan, newUpdDrag)
                                       = case newDrag of
                Just drag             -> (camDragFactorDelta, zoomPan, newUpdDrag) where
                    camDragFactorDelta = (fromIntegral $ delta ^. x + delta ^. y) / 512.0
                    delta              = Vector2 ( drag ^. dragCurrentPos . x - drag ^. dragPreviousPos . x)
                                                 (-drag ^. dragCurrentPos . y + drag ^. dragPreviousPos . y)
                    zoomPan            = Vector2 0.0 0.0 -- Vector2 (fromIntegral $ drag ^. dragStartPos . x) * camDragFactorDelta (fromIntegral $ drag ^. dragStartPos . y) * camDragFactorDelta
                    newUpdDrag         = newDrag -- Just $ drag & Camera.dragStartPos +~ zoomPan
                Nothing               -> (0.0, Vector2 0.0 0.0, newDrag)
        dragPan                        = case newDrag of
                Just drag             -> prevWorkspace - currWorkspace where
                    camera             = Utils.Camera screenSize oldCamPan oldCamFactor
                    currWorkspace      = Utils.screenToWorkspace camera $ drag ^. dragCurrentPos
                    prevWorkspace      = Utils.screenToWorkspace camera $ drag ^. dragPreviousPos
                    screenSize         = oldState ^. Global.screenSize
                Nothing               -> Vector2 0.0 0.0


instance ActionUIUpdater Action where
    updateUI (WithState action state) = do
        let cPan         = state ^. Global.camera . camera . pan
            cFactor      = state ^. Global.camera . camera . factor
            screenSize   = state ^. Global.screenSize
            hScreen      = (/ 2.0) <$> vector2FromIntegral screenSize
            camLeft      = appX cameraLeft
            camRight     = appX cameraRight
            camTop       = appY cameraTop
            camBottom    = appY cameraBottom
            hX           = appX htmlX
            hY           = appY htmlY
            appX      f  = f cFactor (cPan ^. x) (hScreen ^. x)
            appY      f  = f cFactor (cPan ^. y) (hScreen ^. y)
        updateCamera cFactor (cPan ^. x) (cPan ^. y) camLeft camRight camTop camBottom
        updateHtmCanvasPanPos hX hY cFactor
        updateProjectionMatrix


cameraLeft, cameraRight, cameraTop, cameraBottom, htmlX, htmlY :: Double -> Double -> Double -> Double
cameraLeft   camFactor camPanX halfScreenX = -halfScreenX / camFactor + camPanX
cameraRight  camFactor camPanX halfScreenX =  halfScreenX / camFactor + camPanX
cameraTop    camFactor camPanY halfScreenY =  halfScreenY / camFactor + camPanY
cameraBottom camFactor camPanY halfScreenY = -halfScreenY / camFactor + camPanY
htmlX        camFactor camPanX halfScreenX =  halfScreenX - camPanX * camFactor
htmlY        camFactor camPanY halfScreenY =  halfScreenY + camPanY * camFactor

