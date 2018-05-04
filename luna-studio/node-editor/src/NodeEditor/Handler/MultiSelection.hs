module NodeEditor.Handler.MultiSelection
    ( handle
    ) where

import           Common.Action.Command            (Command)
import           Common.Prelude
import           NodeEditor.Action.MultiSelection (startMultiSelection, stopMultiSelection, updateMultiSelection)
import           NodeEditor.Event.Event           (Event (UI))
import qualified NodeEditor.Event.Mouse           as Mouse
import           NodeEditor.Event.UI              (UIEvent (AppEvent, SidebarEvent))
import qualified NodeEditor.React.Event.App       as App
import qualified NodeEditor.React.Event.Sidebar   as Sidebar
import           NodeEditor.State.Action          (Action (continue))
import           NodeEditor.State.Global          (State)
import           NodeEditor.State.Mouse           (workspacePosition)


handle :: Event -> Maybe (Command State ())
handle (UI (AppEvent     (App.MouseDown evt _)))       = Just $ do
    let shouldProceed = Mouse.withoutMods evt Mouse.leftButton
    when shouldProceed $
        startMultiSelection =<< workspacePosition evt
handle (UI (AppEvent     (App.MouseMove evt _)))       = Just $
    continue . updateMultiSelection =<< workspacePosition evt
handle (UI (SidebarEvent (Sidebar.MouseMove evt _ _))) = Just $
    continue . updateMultiSelection =<< workspacePosition evt
handle (UI (AppEvent     (App.MouseUp   _)))           = Just $ continue   stopMultiSelection
handle _                                               = Nothing
