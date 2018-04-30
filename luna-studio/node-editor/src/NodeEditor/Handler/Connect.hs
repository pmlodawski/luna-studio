module NodeEditor.Handler.Connect
    ( handle
    ) where

import           Common.Action.Command             (Command)
import           Common.Prelude
import           LunaStudio.Data.PortRef           (AnyPortRef (InPortRef', OutPortRef'))
import           NodeEditor.Action.Connect         (cancelSnapToPort, handleConnectionMouseDown, handleMouseUp, handleMove,
                                                    handlePortMouseUp, snapToPort)
import           NodeEditor.Event.Event            (Event (UI, View))
import           NodeEditor.Event.UI               (UIEvent (AppEvent, ConnectionEvent, PortEvent, SidebarEvent))
import qualified NodeEditor.Event.View             as View
import           NodeEditor.Event.View             (BaseEvent (Disconnect, MouseEvent), ViewEvent (ViewEvent))
import qualified NodeEditor.React.Event.App        as App
import qualified NodeEditor.React.Event.Connection as Connection
import qualified NodeEditor.React.Event.Port       as Port
import qualified NodeEditor.React.Event.Sidebar    as Sidebar
import           NodeEditor.State.Action           (Action (continue, end), Connect)
import           NodeEditor.State.Global           (State)
import           NodeEditor.State.Mouse            (mousePosition, workspacePosition)
import           NodeEditor.React.Event.Connection (ModifiedEnd (Source, Destination))

handle :: Event -> Maybe (Command State ())
handle (UI (ConnectionEvent (Connection.MouseDown evt connId end'))) = Just $ do
    mousePos <- mousePosition evt
    handleConnectionMouseDown mousePos connId end'
handle (UI (AppEvent        (App.MouseMove        evt _          ))) = Just $ continue . handleMove =<< workspacePosition evt
handle (UI (SidebarEvent    (Sidebar.MouseMove    evt _ _        ))) = Just $ continue . handleMove =<< workspacePosition evt
handle (UI (AppEvent        (App.MouseUp          evt            ))) = Just $ continue . handleMouseUp =<< mousePosition evt
handle (UI (AppEvent         App.Click                            )) = Just $ continue   (end :: Connect -> Command State ())
handle (UI (PortEvent       (Port.MouseUp             portRef    ))) = Just $ continue $ handlePortMouseUp portRef
handle (UI (PortEvent       (Port.MouseEnter          portRef    ))) = Just $ continue $ snapToPort portRef
handle (UI (PortEvent       (Port.MouseLeave          portRef    ))) = Just $ continue $ cancelSnapToPort portRef
handle (View (ViewEvent path portRef evt)) = case evt of
    Disconnect src -> Just $ do
        let connectionEnd = if src then Source else Destination
        handleConnectionMouseDown def (convert portRef) connectionEnd
    MouseEvent { View.type_ = type_ } -> do
        let isInPort  = last path == "InPort"
            isOutPort = last path == "OutPort"
            anyPortRef = if isInPort
                then InPortRef'  $ convert portRef
                else OutPortRef' $ convert portRef
        if isInPort || isOutPort then case type_ of
            "mouseup"     -> Just $ continue $ handlePortMouseUp anyPortRef
            "mouseenter"  -> Just $ continue $ snapToPort anyPortRef
            "mouseleave"  -> Just $ continue $ cancelSnapToPort anyPortRef
            _ -> Nothing
        else if path == ["NodeEditor"] then case type_ of
            "mouseup" -> Just $ continue $ handleMouseUp $ View.mousePosition evt
            "click"   -> Just $ continue   (end :: Connect -> Command State ())
            _         -> Nothing
        else Nothing
    _ -> Nothing
handle _ = Nothing
