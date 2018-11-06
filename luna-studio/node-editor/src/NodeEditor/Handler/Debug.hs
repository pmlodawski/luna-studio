{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.Handler.Debug where

import Common.Prelude

import qualified NodeEditor.Event.Shortcut as Shortcut

import Common.Action.Command              (Command)
import NodeEditor.Action.State.NodeEditor (setDebugLayer)
import NodeEditor.Event.Event             (Event (Shortcut))
import NodeEditor.State.Global            (State)


handle :: Event -> Maybe (Command State ())
handle (Shortcut evt) = handleShortcutEvent evt
handle _              = Nothing

handleShortcutEvent :: Shortcut.ShortcutEvent -> Maybe (Command State ())
handleShortcutEvent evt = case evt ^. Shortcut.shortcut of
    Shortcut.EnableDebugLayer -> setDebugLayer . read <$> evt ^. Shortcut.arg
    _                         -> Nothing
