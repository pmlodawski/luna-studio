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

import           Object.Object
import qualified Object.Node    as Node     ( position )
import           Object.Node    hiding      ( position )
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.Mouse    hiding      ( Event )
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Event.WithObjects
import           Utils.Wrapper
import           Utils.PrettyPrinter
import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.Camera    as Camera
import qualified Reactive.Plugins.Core.Action.State.Global    as Global


data DragType = StartDrag
              | Dragging
              | StopDrag
              deriving (Eq, Show)

data MouseActionType = Zoom | Pan deriving (Eq, Show)

data Action = ZoomIn
            | ZoomOut
            | PanLeft
            | PanRight
            | PanUp
            | PanDown
            | MouseAction { _actionType :: MouseActionType
                          , _dragType   :: DragType
                          , _zoomPos    :: Point
                          }
            deriving (Eq, Show)


makeLenses ''Action


instance PrettyPrinter DragType where
    display = show

instance PrettyPrinter MouseActionType where
    display = show

instance PrettyPrinter Action where
    display ZoomIn                    = "cA( ZoomIn )"
    display ZoomOut                   = "cA( ZoomOut )"
    display PanLeft                   = "cA( PanLeft )"
    display PanRight                  = "cA( PanRight )"
    display PanUp                     = "cA( PanUp )"
    display PanDown                   = "cA( PanDown )"
    display (MouseAction act tpe pos) = "cA( Mouse " <> display act <> " " <> display tpe <> " " <> display pos <> " )"


toAction :: Event Node -> Maybe Action
toAction (Mouse (WithObjects (Mouse.Event tpe pos button keyMods) objects)) = case button of
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
    '='   -> Just ZoomIn
    '+'   -> Just ZoomIn
    '-'   -> Just ZoomOut
    _     -> Nothing
toAction (Keyboard (Keyboard.Event Keyboard.Down char)) = case char of
    '\37' -> Just PanLeft
    '\39' -> Just PanRight
    '\38' -> Just PanUp
    '\40' -> Just PanDown
    _     -> Nothing
toAction _ = Nothing


instance ActionStateUpdater Action where
    execSt newActionCandidate oldState =
        case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI NoAction newState
        where
        newState                       = oldState &  Global.iteration +~ 1
                                                  &  Global.camera . Camera.camPanX   .~ newCamPanX
                                                  &  Global.camera . Camera.camPanY   .~ newCamPanY
                                                  &  Global.camera . Camera.camFactor .~ newCamFactor
                                                  &  Global.camera . Camera.history   .~ newUpdDrag
        oldCamPanX                     = oldState ^. Global.camera . Camera.camPanX
        oldCamPanY                     = oldState ^. Global.camera . Camera.camPanY
        oldCamFactor                   = oldState ^. Global.camera . Camera.camFactor
        oldDrag                        = oldState ^. Global.camera . Camera.history
        newAction                      = Just newActionCandidate
        newCamPanX                     = case newActionCandidate of
            PanLeft                   -> oldCamPanX - 10.0 / oldCamFactor
            PanRight                  -> oldCamPanX + 10.0 / oldCamFactor
            MouseAction Zoom _ _      -> oldCamPanX + deltaPanX
            _                         -> oldCamPanX
        newCamPanY                     = case newActionCandidate of
            PanUp                     -> oldCamPanY + 10.0 / oldCamFactor
            PanDown                   -> oldCamPanY - 10.0 / oldCamFactor
            MouseAction Zoom _ _      -> oldCamPanY + deltaPanY
            _                         -> oldCamPanY
        newCamFactor                   = case newActionCandidate of
            ZoomIn                    -> max 0.2 $ oldCamFactor / 1.1
            ZoomOut                   -> min 2.0 $ oldCamFactor * 1.1
            MouseAction Zoom _ _      -> min 2.0 . max 0.2 $ oldCamFactor * (1.0 + camDragFactorDelta)
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
        (camDragFactorDelta, deltaPanX, deltaPanY, newUpdDrag)
                                       = case newDrag of
                Just drag             -> (camDragFactorDelta, deltaPanX, deltaPanY, newUpdDrag) where
                    camDragFactorDelta = (fromIntegral $ deltaX + deltaY) / 512.0
                    deltaX             =  drag ^. dragCurrentPos . x - drag ^. dragPreviousPos . x
                    deltaY             = -drag ^. dragCurrentPos . y + drag ^. dragPreviousPos . y
                    deltaPanX          = 0 -- (fromIntegral $ drag ^. dragStartPos . x) * camDragFactorDelta
                    deltaPanY          = 0 -- (fromIntegral $ drag ^. dragStartPos . y) * camDragFactorDelta
                    deltaPan           = Point (round deltaPanX) (round deltaPanY)
                    newUpdDrag         = newDrag -- Just $ drag & Camera.dragStartPos +~ deltaPan
                Nothing               -> (0.0, 0.0, 0.0, newDrag)
        (mousePanX, mousePanY)         = case newDrag of
                Just drag             -> (mousePanX, mousePanY) where
                    mousePanX          = 0
                    mousePanY          = 0
                Nothing               -> (0.0, 0.0)


instance ActionUIUpdater Action where
    updateUI (WithState action state) = do
        screenWidth  <- innerWidth
        screenHeight <- innerHeight
        let
            hScreenX    = (fromIntegral screenWidth)  / 2.0
            hScreenY    = (fromIntegral screenHeight) / 2.0
            camLeft     = appX cameraLeft
            camRight    = appX cameraRight
            camTop      = appY cameraTop
            camBottom   = appY cameraBottom
            hX          = appX htmlX
            hY          = appY htmlY
            appX      f = f cFactor cPanX hScreenX
            appY      f = f cFactor cPanY hScreenY
        updateCamera cFactor cPanX cPanY camLeft camRight camTop camBottom
        updateHtmCanvasPanPos hX hY cFactor
        updateProjectionMatrix
        where
            -- hScreenX    = state ^. Global.camera . Camera.halfScreenX
            -- hScreenY    = state ^. Global.camera . Camera.halfScreenY
            cPanX       = state ^. Global.camera . Camera.camPanX
            cPanY       = state ^. Global.camera . Camera.camPanY
            cFactor     = state ^. Global.camera . Camera.camFactor


cameraLeft, cameraRight, cameraTop, cameraBottom, htmlX, htmlY :: Double -> Double -> Double -> Double
cameraLeft   camFactor camPanX halfScreenX = -halfScreenX / camFactor + camPanX
cameraRight  camFactor camPanX halfScreenX =  halfScreenX / camFactor + camPanX
cameraTop    camFactor camPanY halfScreenY =  halfScreenY / camFactor + camPanY
cameraBottom camFactor camPanY halfScreenY = -halfScreenY / camFactor + camPanY
htmlX        camFactor camPanX halfScreenX =  halfScreenX - camPanX * camFactor
htmlY        camFactor camPanY halfScreenY =  halfScreenY + camPanY * camFactor

