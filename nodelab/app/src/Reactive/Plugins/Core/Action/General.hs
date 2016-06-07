module Reactive.Plugins.Core.Action.General where

import           Utils.PreludePlus
import           Utils.Vector

import           Event.Event
import           Event.Mouse                             hiding (Event, WithObjects)
import qualified Event.Mouse                             as Mouse
import qualified Event.Window                            as Window
import           JS.Camera
import qualified Reactive.Commands.Camera                as Camera
import qualified Reactive.Plugins.Core.Action.TextEditor as TextEditor
import qualified Reactive.Plugins.Core.Action.UILayout   as UILayout
import qualified Reactive.State.Camera                   as Camera
import qualified Reactive.State.Global                   as Global

import           Reactive.Commands.Command               (Command, execCommand, ioCommand, performIO)

toAction :: Event -> Maybe (Command Global.State ())
toAction (Mouse _ (Mouse.Event Mouse.Moved pos _ _ _))       = Just $ updateMousePos pos
toAction (Window (Window.Resized width height)) = Just $ updateWindowSize (Vector2 width height)
toAction _                                                   = Nothing

updateWindowSize :: Vector2 Int -> Command Global.State ()
updateWindowSize size = do
    textEditorWidth <- TextEditor.relayout size
    zoom Global.camera $ do
        let canvasWidth = size ^. x - textEditorWidth
        Camera.camera . Camera.windowSize .= size
        Camera.camera . Camera.screenSize .= Vector2 canvasWidth (size ^. y)
        Camera.syncCamera
        performIO $ updateScreenSize canvasWidth (size ^. y)
    UILayout.relayout

updateMousePos :: Vector2 Int -> Command Global.State ()
updateMousePos pos = Global.mousePos .= pos
