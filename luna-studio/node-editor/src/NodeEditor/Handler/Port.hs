module NodeEditor.Handler.Port where

import           Common.Action.Command       (Command)
import           Common.Prelude
import           NodeEditor.Action.Basic     (setPortDefault)
import           NodeEditor.Action.Port      (handleClick, handleMouseDown)
import           NodeEditor.Event.Event      (Event (View))
import           NodeEditor.Event.View       (BaseEvent (Mouse, PortControl), ViewEvent, mousePosition)
import qualified NodeEditor.Event.View       as View
import           NodeEditor.State.Global     (State)


handle :: Event -> Maybe (Command State ())
handle (View evt) = handleViewEvent evt
handle _          = Nothing

handleViewEvent :: ViewEvent -> Maybe (Command State ())
handleViewEvent evt = case evt ^. View.base of
    Mouse e -> do
        let path = evt ^. View.path
            isPort = "InPort" `elem` path || "OutPort" `elem` path || "NewPort" `elem` path
            anyPortRef = View.getAnyPortRef evt
        if isPort then case e ^. View.type_ of
            "click"     -> Just
                $ handleClick     (mousePosition $ evt ^. View.base) anyPortRef
            "mousedown" -> Just
                $ handleMouseDown (mousePosition $ evt ^. View.base) anyPortRef
            _ -> Nothing
        else Nothing
    PortControl e -> do
        let inPortRef = View.getInPortRef evt
            content = e ^. View.content
        Just $ setPortDefault inPortRef $ convert content
    _ -> Nothing
