module NodeEditor.Handler.Breadcrumbs where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           NodeEditor.Action.Basic            (enterBreadcrumbs, exitBreadcrumb)
import           NodeEditor.Event.Event             (Event (Shortcut, UI, View))
import qualified NodeEditor.Event.Shortcut          as Shortcut
import           NodeEditor.Event.UI                (UIEvent (BreadcrumbsEvent))
import           NodeEditor.Event.View              (BaseEvent (Navigate), ViewEvent(ViewEvent))
import qualified NodeEditor.React.Event.Breadcrumbs as Breadcrumbs
import           NodeEditor.State.Global            (State)


handle :: Event -> Maybe (Command State ())
handle (View (ViewEvent _  _ (Navigate bc)))          = Just $ enterBreadcrumbs bc
handle (UI (BreadcrumbsEvent (Breadcrumbs.Enter bc))) = Just $ enterBreadcrumbs bc
handle (UI (BreadcrumbsEvent  Breadcrumbs.Exit     )) = Just exitBreadcrumb
handle (Shortcut (Shortcut.Event command _))          = Just $ handleCommand command
handle _   = Nothing



handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.ExitGraph -> exitBreadcrumb
    _                  -> return ()
