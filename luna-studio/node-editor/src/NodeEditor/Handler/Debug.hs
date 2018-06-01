{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.Handler.Debug where

import           Common.Prelude

import           Common.Action.Command              (Command)
import           NodeEditor.Event.Event             (Event (Shortcut))
import qualified NodeEditor.Event.Shortcut          as Shortcut
import           NodeEditor.State.Global            (State)
import NodeEditor.Action.State.NodeEditor (setDebugLayer)


handle :: Event -> Maybe (Command State ())
handle (Shortcut evt) = handleShortcutEvent evt
handle _              = Nothing

handleShortcutEvent :: Shortcut.ShortcutEvent -> Maybe (Command State ())
handleShortcutEvent evt = case evt ^. Shortcut.shortcut of
    Shortcut.EnableDebugLayer -> setDebugLayer . read <$> evt ^. Shortcut.arg
    _                         -> Nothing