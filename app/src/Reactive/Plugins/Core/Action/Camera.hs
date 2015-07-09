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


data Action = ZoomIn
            | ZoomOut
            | MoveLeft
            | MoveRight
            | MoveUp
            | MoveDown
            deriving (Eq, Show)


makeLenses ''Action

instance PrettyPrinter Action where
    display ZoomIn    = "cA( ZoomIn )"
    display ZoomOut   = "cA( ZoomOut )"
    display MoveLeft  = "cA( MoveLeft )"
    display MoveRight = "cA( MoveRight )"
    display MoveUp    = "cA( MoveUp )"
    display MoveDown  = "cA( MoveDown )"


toAction :: Event Node -> Maybe Action
toAction (Mouse (WithObjects mouseEvent objects)) = Nothing
toAction (Keyboard (Keyboard.Event Keyboard.Press char)) = case char of
    '='   -> Just ZoomIn
    '+'   -> Just ZoomIn
    '-'   -> Just ZoomOut
    _     -> Nothing
toAction (Keyboard (Keyboard.Event Keyboard.Down char)) = case char of
    '\37' -> Just MoveLeft
    '\39' -> Just MoveRight
    '\38' -> Just MoveUp
    '\40' -> Just MoveDown
    _     -> Nothing
toAction _ = Nothing


instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI NoAction newState
        where
        newState                = oldState & Global.iteration +~ 1
                                           & Global.camera . Camera.camPanX   .~ newCamPanX
                                           & Global.camera . Camera.camPanY   .~ newCamPanY
                                           & Global.camera . Camera.camFactor .~ newCamFactor
        oldCamPanX              = oldState ^. Global.camera . Camera.camPanX
        oldCamPanY              = oldState ^. Global.camera . Camera.camPanY
        oldCamFactor            = oldState ^. Global.camera . Camera.camFactor
        newAction               = Just newActionCandidate
        newCamPanX              = case newActionCandidate of
            MoveLeft           -> oldCamPanX - 10.0 / oldCamFactor
            MoveRight          -> oldCamPanX + 10.0 / oldCamFactor
            _                  -> oldCamPanX
        newCamPanY              = case newActionCandidate of
            MoveUp             -> oldCamPanY + 10.0 / oldCamFactor
            MoveDown           -> oldCamPanY - 10.0 / oldCamFactor
            _                  -> oldCamPanY
        newCamFactor            = case newActionCandidate of
            ZoomIn             -> max 0.2 $ oldCamFactor / 1.1
            ZoomOut            -> min 2.0 $ oldCamFactor * 1.1
            _                  -> oldCamFactor




instance ActionUIUpdater Action where
    updateUI (WithState action state) = do
        updateCam state
        return ()
        -- case action of
        --     ZoomIn      -> putStrLn $ display action
        --     ZoomOut     -> putStrLn $ display action
        --     MoveLeft    -> putStrLn $ display action
        --     MoveRight   -> putStrLn $ display action
        --     MoveUp      -> putStrLn $ display action
        --     MoveDown    -> putStrLn $ display action



updateCam :: Global.State -> IO ()
updateCam state = do
    screenWidth  <- innerWidth
    screenHeight <- innerHeight
    let
        hScreenX    = (fromIntegral screenWidth)  / 2.0
        hScreenY    = (fromIntegral screenHeight) / 2.0
        camLeft     = cameraLeft   hScreenX cFactor cPanX
        camRight    = cameraRight  hScreenX cFactor cPanX
        camTop      = cameraTop    hScreenY cFactor cPanY
        camBottom   = cameraBottom hScreenY cFactor cPanY
        hX          = htmlX        hScreenX cFactor cPanX
        hY          = htmlY        hScreenY cFactor cPanY
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
cameraLeft   halfScreenX camFactor camPanX = -halfScreenX / camFactor + camPanX
cameraRight  halfScreenX camFactor camPanX =  halfScreenX / camFactor + camPanX
cameraTop    halfScreenY camFactor camPanY =  halfScreenY / camFactor + camPanY
cameraBottom halfScreenY camFactor camPanY = -halfScreenY / camFactor + camPanY
htmlX        halfScreenX camFactor camPanX =  halfScreenX - camPanX * camFactor
htmlY        halfScreenY camFactor camPanY =  halfScreenY + camPanY * camFactor

