module NodeEditor.Handler.ConnectionPen
    ( handle
    ) where

import           Common.Action.Command           (Command)
import           Common.Prelude
import           Data.Timestamp                  (Timestamp)
import           NodeEditor.Action.ConnectionPen (connectMove, disconnectMove, startConnecting, startDisconnecting, stopConnecting,
                                                  stopDisconnecting)
import           NodeEditor.Event.Event          (Event, Event (UI))
import qualified NodeEditor.Event.Mouse          as Mouse
import           NodeEditor.Event.UI             (UIEvent (AppEvent, SidebarEvent))
import qualified NodeEditor.React.Event.App      as App
import qualified NodeEditor.React.Event.Sidebar  as Sidebar
import           NodeEditor.State.Action         (Action (continue))
import           NodeEditor.State.Global         (State)
import           NodeEditor.State.Mouse          (workspacePosition)
import           React.Flux                      (MouseEvent)


handle :: Event -> Maybe (Command State ())
handle (UI (AppEvent (App.MouseDown evt timestamp)))           = Just $ handleMouseDown evt timestamp
handle (UI (AppEvent (App.MouseMove evt timestamp)))           = Just $ do
    pos <- workspacePosition evt
    continue (connectMove pos timestamp) >> continue (disconnectMove pos timestamp)
handle (UI (SidebarEvent (Sidebar.MouseMove evt _ timestamp))) = Just $ do
    pos <- workspacePosition evt
    continue (connectMove pos timestamp) >> continue (disconnectMove pos timestamp)
handle (UI (AppEvent (App.MouseUp   _)))                       = Just $ do
    continue stopConnecting >> continue stopDisconnecting
handle _                                                       = Nothing

handleMouseDown :: MouseEvent -> Timestamp -> Command State ()
handleMouseDown evt timestamp
    | Mouse.withCtrl      evt Mouse.leftButton  = workspacePosition evt >>= flip startConnecting    timestamp
    | Mouse.withCtrlShift evt Mouse.leftButton  = workspacePosition evt >>= flip startDisconnecting timestamp
    | Mouse.withCtrl      evt Mouse.rightButton = workspacePosition evt >>= flip startDisconnecting timestamp
    | otherwise                                 = return ()
