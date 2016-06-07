module Reactive.Plugins.Core.Action.General
    ( toAction
    ) where

import           Utils.PreludePlus
import           Utils.Vector

import           Event.Event
import           Event.Mouse                             hiding (Event, WithObjects)
import qualified Event.Mouse                             as Mouse
import qualified Event.Window                            as Window
import           JS.Camera
import qualified Reactive.Commands.Camera                as Camera
import qualified Reactive.Commands.UILayout              as UILayout
import qualified Reactive.State.Camera                   as Camera
import qualified Reactive.State.Global                   as Global

import           Reactive.Commands.Command               (Command, execCommand, ioCommand, performIO)

toAction :: Event -> Maybe (Command Global.State ())
toAction (Mouse _ (Mouse.Event Mouse.Moved pos _ _ _)) = Just $ updateMousePos pos
toAction (Window (Window.Resized width height))        = Just $ Camera.updateWindowSize (Vector2 width height)
toAction _                                             = Nothing

updateMousePos :: Vector2 Int -> Command Global.State ()
updateMousePos pos = Global.mousePos .= pos
