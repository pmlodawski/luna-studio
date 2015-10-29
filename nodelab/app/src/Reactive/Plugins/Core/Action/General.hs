module Reactive.Plugins.Core.Action.General where

import           Utils.PreludePlus
import           Utils.Vector

import           JS.Bindings
import           JS.Camera
import           Object.Object
import           Object.Node
import           Event.Mouse    hiding      (Event)
import qualified Event.Mouse    as Mouse
import qualified Event.Window   as Window
import           Event.Event
import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.Camera         as Camera
import qualified Reactive.State.Camera   as Camera
import qualified Reactive.State.Global   as Global

import Reactive.Commands.Command (Command, ioCommand, execCommand, performIO)

toAction :: Event -> Maybe (Command Global.State ())
toAction (Mouse (Mouse.Event Mouse.Moved pos _ _ _))         = Just $ updateMousePos pos
toAction (Window (Window.Event Window.Resized width height)) = Just $ zoom Global.camera $ updateWindowSize (Vector2 width height)

updateWindowSize :: Vector2 Int -> Command Camera.State ()
updateWindowSize size = do
    Camera.camera . Camera.screenSize .= size
    Camera.syncCamera
    performIO $ updateScreenSize (size ^. x) (size ^. y)

updateMousePos :: Vector2 Int -> Command Global.State ()
updateMousePos pos = do
    Global.mousePos .= pos
    camera <- use $ Global.camera . Camera.camera
    let Vector2 x y = Camera.screenToWorkspace camera pos
    performIO $ updateMouse x y
