{-# LANGUAGE NamedFieldPuns #-}

module Reactive.Plugins.Core.Action.Camera where

import           Utils.PreludePlus
import           Utils.Vector

import           Event.Event               (Event (Keyboard, Mouse))
import           Event.Keyboard            (KeyMods (..), ctrl)
import qualified Event.Keyboard            as Keyboard
import           Event.Mouse               (MouseButton (..))
import qualified Event.Mouse               as Mouse
import qualified JS.Camera                 as JS
import           Reactive.State.Camera     (DragHistory (..))
import qualified Reactive.State.Camera     as Camera
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Graph      as Graph

import           Empire.API.Data.Node      (Node)
import qualified Empire.API.Data.Node      as Node
import           Reactive.Commands.Camera  (autoZoom, panCamera, panDown, panDrag, panLeft, panRight, panUp, resetZoom,
                                            syncCamera, wheelZoom, zoomDrag, zoomIn, zoomOut)
import           Reactive.Commands.Command (Command, execCommand, ioCommand, performIO)


toAction :: Event -> Maybe (Command Global.State ())
toAction (Keyboard _ (Keyboard.Event Keyboard.Press '0' KeyMods {_ctrl = True})) = Just $ autoZoom >> (zoom Global.camera syncCamera)
toAction evt = (zoom Global.camera) <$> (>> syncCamera) <$> toAction' evt

toAction' :: Event -> Maybe (Command Camera.State ())
toAction' (Mouse _ (Mouse.Event evt pos RightButton  _ _)) = Just $ zoomDrag evt pos
toAction' (Mouse _ (Mouse.Event evt pos MiddleButton _ _)) = Just $ panDrag  evt pos

toAction' (Mouse _ (Mouse.Event (Mouse.Wheel delta) pos _ KeyMods {_ctrl = False} _)) = Just $ panCamera delta
toAction' (Mouse _ (Mouse.Event (Mouse.Wheel delta) pos _ KeyMods {_ctrl = True} _))  = Just $ wheelZoom pos delta

toAction' (Keyboard _ (Keyboard.Event Keyboard.Press char _)) = case char of
    '='   -> Just $ zoomIn
    '+'   -> Just $ zoomIn
    '-'   -> Just $ zoomOut
    '0'   -> Just $ resetZoom
    _     -> Nothing

toAction' (Keyboard _ (Keyboard.Event Keyboard.Down char KeyMods { _ctrl = True })) = case char of
    '\37' -> Just panLeft
    '\39' -> Just panRight
    '\38' -> Just panUp
    '\40' -> Just panDown
    _     -> Nothing
toAction' _ = Nothing
