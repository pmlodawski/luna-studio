module NodeEditor.Handler.InputField where

import           Common.Action.Command             (Command)
import           Common.Prelude
import           NodeEditor.Action.InputField      (accept, activateInputField, updateActiveInputField)
import qualified NodeEditor.Event.Atom             as Atom
import           NodeEditor.Event.Event            (Event (Atom, Shortcut, UI))
import qualified NodeEditor.Event.Shortcut         as Shortcut
import           NodeEditor.Event.UI               (UIEvent (InputFieldEvent))
import qualified NodeEditor.React.Event.InputField as Field
import           NodeEditor.React.Model.InputField (ActiveInputField (ActiveInputField))
import           NodeEditor.State.Action           (Action (continue))
import           NodeEditor.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event Shortcut.AcceptInput _))       = Just $ continue accept
handle (UI (InputFieldEvent (Field.ActivateInputField fid)))    = Just $ activateInputField fid
handle (Atom (Atom.InputFieldUpdate content cursors selection)) = Just $ print "UPDATE" >> (continue $ updateActiveInputField (ActiveInputField content cursors selection))
handle _                                                        = Nothing
