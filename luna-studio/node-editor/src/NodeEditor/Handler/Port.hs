module NodeEditor.Handler.Port where

import           Common.Action.Command       (Command)
import           Common.Prelude
import           LunaStudio.Data.PortRef     (AnyPortRef (InPortRef', OutPortRef'))
import           NodeEditor.Action.Basic     (setPortDefault)
import           NodeEditor.Action.Port      (acceptEditTextPortControl, editTextPortControl, handleClick, handleMouseDown,
                                              handleMouseEnter, handleMouseLeave, startMoveSlider)
import           NodeEditor.Event.Event      (Event (UI, View))
import qualified NodeEditor.Event.View       as View
import           NodeEditor.Event.View       (BaseEvent (MouseEvent), ViewEvent (ViewEvent), mousePosition)
import           NodeEditor.Event.UI         (UIEvent (PortEvent))
import qualified NodeEditor.React.Event.Port as Port
import           NodeEditor.State.Action     (Action (continue))
import           NodeEditor.State.Global     (State)
import qualified NodeEditor.State.Mouse      as State


handle :: Event -> Maybe (Command State ())
handle (UI (PortEvent (Port.MouseDown           evt portRef)))            = Just $ State.mousePosition evt >>= flip handleMouseDown portRef
handle (UI (PortEvent (Port.Click               evt portRef)))            = Just $ State.mousePosition evt >>= flip handleClick     portRef
handle (UI (PortEvent (Port.MouseEnter              portRef)))            = Just $ handleMouseEnter portRef
handle (UI (PortEvent (Port.MouseLeave              portRef)))            = Just $ handleMouseLeave portRef
handle (UI (PortEvent (Port.EditTextPortControlBlur _)))                  = Just $ continue acceptEditTextPortControl
handle (UI (PortEvent (Port.EditTextPortControl     portRef val)))        = Just $ editTextPortControl portRef val
handle (UI (PortEvent (Port.PortSetPortDefault      portRef portDef)))    = Just $ setPortDefault portRef portDef
handle (UI (PortEvent (Port.PortInitSlider      _   portRef sliderInit))) = Just $ startMoveSlider portRef sliderInit
handle (View (ViewEvent path portRef evt@(MouseEvent { View.type_ = type_ }))) = do
    let isInPort  = last path == "InPort"
        isOutPort = last path == "OutPort"
        anyPortRef = if isInPort
            then InPortRef'  $ convert portRef
            else OutPortRef' $ convert portRef
    if isInPort || isOutPort then case type_ of
        "click"     -> Just $ handleClick     (mousePosition evt) anyPortRef
        "mousedown" -> Just $ handleMouseDown (mousePosition evt) anyPortRef
        _ -> Nothing
    else Nothing
handle _ = Nothing
