module NodeEditor.Handler.Connect
    ( handle
    ) where

import           Common.Action.Command             (Command)
import           Common.Prelude
import           LunaStudio.Data.PortRef           (AnyPortRef (InPortRef', OutPortRef'))
import           NodeEditor.Action.Connect         (cancelSnapToPort, handleConnectionMouseDown, handleMouseUp, handleMove,
                                                    handlePortMouseUp, snapToPort)
import           NodeEditor.Event.Event            (Event (View))
import           NodeEditor.Event.View             (BaseEvent (Disconnect, Mouse), ViewEvent, base)
import qualified NodeEditor.Event.View             as View
import           NodeEditor.React.Event.Connection (ModifiedEnd (Destination, Source))
import           NodeEditor.State.Action           (Action (continue, end), Connect)
import           NodeEditor.State.Global           (State)

handle :: Event -> Maybe (Command State ())
handle (View evt) = handleViewEvent evt
handle _          = Nothing


handleViewEvent :: ViewEvent -> Maybe (Command State ())
handleViewEvent evt = case evt ^. base of
    Disconnect e -> Just $ do
        let inPortRef = View.getInPortRef evt
            connectionEnd = if e ^. View.src then Source else Destination
        handleConnectionMouseDown def inPortRef connectionEnd
    Mouse e -> do
        let path = evt ^. View.path
            isPort = "InPort" `elem` path || "OutPort" `elem` path || "NewPort" `elem` path
            anyPortRef = View.getAnyPortRef evt
        if isPort then case e ^. View.type_ of
            "mouseup"     -> Just . continue $ handlePortMouseUp anyPortRef
            "mouseenter"  -> Just . continue $ snapToPort anyPortRef
            "mouseleave"  -> Just . continue $ cancelSnapToPort anyPortRef
            _ -> Nothing
        else if path == ["NodeEditor"] then case e ^. View.type_ of
            "mouseup" -> Just . continue
                . handleMouseUp . View.mousePosition $ evt ^. base
            "click"   -> Just $ continue (end :: Connect -> Command State ())
            _         -> Nothing
        else Nothing
    _ -> Nothing
