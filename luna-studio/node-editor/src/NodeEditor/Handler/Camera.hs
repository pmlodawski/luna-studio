module NodeEditor.Handler.Camera
    ( handle
    ) where

import           Common.Action.Command          (Command)
import           Common.Prelude
import           NodeEditor.Action.Camera       (centerGraph, panDown, panLeft, panRight, panUp, resetCamera, resetPan,
                                                 resetZoom, zoomIn, zoomOut)
import           NodeEditor.Event.Event         (Event (Shortcut))
import qualified NodeEditor.Event.Shortcut      as Shortcut
import           NodeEditor.State.Global        (State)


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event command _))         = Just $ handleCommand command
handle _                                             = Nothing


-- TODO consider using state and below approach
-- init = do
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--


handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.CenterGraph -> centerGraph
    Shortcut.PanDown     -> panDown
    Shortcut.PanLeft     -> panLeft
    Shortcut.PanRight    -> panRight
    Shortcut.PanUp       -> panUp
    Shortcut.ResetCamera -> resetCamera
    Shortcut.ResetPan    -> resetPan >> centerGraph
    Shortcut.ResetZoom   -> resetZoom
    Shortcut.ZoomIn      -> zoomIn
    Shortcut.ZoomOut     -> zoomOut
    _                    -> return ()
