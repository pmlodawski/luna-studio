module Object.Widget.CompositeWidget where

import Utils.PreludePlus
import Utils.Vector

import Object.UITypes
import Object.Widget
import Reactive.State.UIRegistry (State)
import Reactive.Commands.Command (Command)

class DisplayObjectClass a => CompositeWidget a where
    createWidget :: WidgetId -> a                   -> Command State ()
    updateWidget :: WidgetId -> a -> a              -> Command State ()
    -- resizeWidget :: WidgetId -> a -> Vector2 Double -> Command State ()
