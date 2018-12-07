module NodeEditor.Handler.Breadcrumbs where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           NodeEditor.Action.Basic            (enterBreadcrumbs, exitBreadcrumb)
import           NodeEditor.Event.Event             (Event (Shortcut, UI, View))
import           NodeEditor.Event.Shortcut          (ShortcutEvent, shortcut)
import qualified NodeEditor.Event.Shortcut          as Shortcut
import           NodeEditor.Event.View              (BaseEvent (Navigate), ViewEvent, base)
import qualified NodeEditor.Event.View              as View
import           NodeEditor.State.Global            (State)


handle :: Event -> Maybe (Command State ())
handle (View     evt) = handleViewEvent     evt
handle (Shortcut evt) = handleShortcutEvent evt
handle _              = Nothing


handleViewEvent :: ViewEvent -> Maybe (Command State ())
handleViewEvent evt = case evt ^. base of
    Navigate e -> Just . enterBreadcrumbs $ e ^. View.to
    _          -> Nothing

handleShortcutEvent :: ShortcutEvent -> Maybe (Command State ())
handleShortcutEvent evt = case evt ^. shortcut of
    Shortcut.ExitGraph -> Just exitBreadcrumb
    _                  -> Nothing
