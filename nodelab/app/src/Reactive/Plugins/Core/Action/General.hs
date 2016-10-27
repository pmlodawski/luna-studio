module Reactive.Plugins.Core.Action.General
    ( toAction
    ) where

import           Utils.PreludePlus
import           Utils.Vector

import           Event.Event
import qualified Event.Mouse                  as Mouse
import qualified Event.Window                 as Window
import           Object.Widget                (widgetSize)
import qualified Reactive.Commands.Camera     as Camera
import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.UIElements    as UIElements



toAction :: Event -> Maybe (Command Global.State ())
toAction (Mouse _ (Mouse.Event Mouse.Moved pos _ _ _)) = Just $ updateMousePos pos
toAction (Window (Window.Resized width height))        = Just $ do
    moveInputsEdge  width height
    moveOutputsEdge width height
    Camera.updateWindowSize (Vector2 width height)
toAction _                                             = Nothing

updateMousePos :: Vector2 Int -> Command Global.State ()
updateMousePos pos = Global.mousePos .= pos

moveInputsEdge :: Int -> Int -> Command Global.State ()
moveInputsEdge _ height = do
    inputsEdge <- use $ Global.uiElements . UIElements.inputsEdge
    inputsSize <- inRegistry $ UICmd.get' inputsEdge widgetSize
    inRegistry $ UICmd.moveY inputsEdge $ fromIntegral height / 2.0 - inputsSize ^. x / 2

moveOutputsEdge :: Int -> Int -> Command Global.State ()
moveOutputsEdge width height = do
    outputsEdge <- use $ Global.uiElements . UIElements.outputsEdge
    outputsSize <- inRegistry $ UICmd.get' outputsEdge widgetSize
    inRegistry $ UICmd.moveX outputsEdge $ fromIntegral width - outputsSize ^. x
    inRegistry $ UICmd.moveY outputsEdge $ fromIntegral height / 2.0 - outputsSize ^. y / 2
