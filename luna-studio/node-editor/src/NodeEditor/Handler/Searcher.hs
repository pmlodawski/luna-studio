module NodeEditor.Handler.Searcher where

import Common.Prelude

import qualified Data.Text                       as Text
import qualified NodeEditor.Action.Searcher      as Searcher
import qualified NodeEditor.Event.Shortcut       as Shortcut
import qualified NodeEditor.Event.View           as View
import qualified NodeEditor.React.Event.Searcher as Searcher

import Common.Action.Command              (Command)
import LunaStudio.Data.Position           (fromTuple)
import NodeEditor.Action.State.NodeEditor (whenGraphLoaded)
import NodeEditor.Event.Event             (Event (Shortcut, UI, View))
import NodeEditor.Event.UI                (UIEvent (SearcherEvent))
import NodeEditor.State.Action            (Action (continue))
import NodeEditor.State.Global            (State)
import Text.Read                          (readMaybe)
import NodeEditor.Event.View              (BaseEvent (EditNodeExpression, EditNodeName,
                                           SearcherAccept, SearcherEdit, SearcherScrollPrev,
                                           SearcherScrollNext, SearcherTabPressed),
                                           SearcherEditEvent (SearcherEditEvent),
                                           ViewEvent, base)



handle :: (Event -> IO ()) -> Event -> Maybe (Command State ())
handle scheduleEvent (UI (SearcherEvent evt))
    = handleSearcherEvent scheduleEvent evt
handle scheduleEvent (View evt) = handleViewEvent scheduleEvent evt
handle _ (Shortcut evt)         = handleShortcutEvent evt
handle _ _                      = Nothing

handleShortcutEvent :: Shortcut.ShortcutEvent -> Maybe (Command State ())
handleShortcutEvent evt = case evt ^. Shortcut.shortcut of
    Shortcut.SearcherEditExpression {}
        -> Just $ whenGraphLoaded Searcher.editSelectedNodeExpression
    Shortcut.SearcherOpen
        -> Just . whenGraphLoaded . Searcher.open
            $ fmap fromTuple $ readMaybe =<< evt ^. Shortcut.arg
    _ -> Nothing

handleSearcherEvent
    :: (Event -> IO ()) -> Searcher.Event -> Maybe (Command State ())
handleSearcherEvent scheduleEvent = \case
    Searcher.InputChanged input ss se
        -> Just . continue $ Searcher.updateInput input ss se
    Searcher.Accept
        -> Just . continue $ Searcher.accept scheduleEvent
    Searcher.AcceptInput
        -> Just . continue $ Searcher.withHint 0 (Searcher.accept scheduleEvent)
    Searcher.AcceptWithHint i
        -> Just . continue $ Searcher.withHint i Searcher.updateInputWithSelectedHint
    Searcher.HintShortcut i -> Just . continue $ Searcher.withHint i Searcher.updateInputWithSelectedHint
    Searcher.Continue       -> Just $ continue Searcher.handleTabPressed
    Searcher.ScrollPrev     -> Just $ continue Searcher.selectPreviousHint
    Searcher.ScrollNext     -> Just $ continue Searcher.selectNextHint
    _                       -> Nothing

    -- Searcher.KeyUp k                  -> when (Keys.withoutMods k Keys.backspace) $ continue Searcher.enableRollback
    -- Searcher.MoveLeft                 -> continue Searcher.tryRollback

handleViewEvent :: (Event -> IO ()) -> ViewEvent -> Maybe (Command State ())
handleViewEvent scheduleEvent evt = case evt ^. base of
    EditNodeName        {} -> Just $ Searcher.editName nl
    EditNodeExpression  {} -> Just $ Searcher.editExpression nl
    SearcherEdit (SearcherEditEvent ss se input) ->
           jc $ Searcher.updateInput input ss se
    SearcherAccept searcher -> jc   $ if Text.null $ searcher ^. View.acceptValue
        then Searcher.close
        else Searcher.accept scheduleEvent
    SearcherTabPressed  {} -> jc Searcher.handleTabPressed
    SearcherScrollPrev  {} -> jc Searcher.selectPreviousHint
    SearcherScrollNext  {} -> jc Searcher.selectNextHint
    _ -> Nothing
    where
        nl = View.getNodeLoc evt
        jc = Just . continue
