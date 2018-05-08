-- TODO[PM]: Finish implementation
module NodeEditor.Handler.Clipboard where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           NodeEditor.Action.Basic                    (removeSelectedNodes)
import           NodeEditor.Action.Batch                    (copy, paste)
import           NodeEditor.Action.State.NodeEditor         (getSelectedNodes)
import           NodeEditor.Event.Event                     (Event (Shortcut))
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import           NodeEditor.React.Model.Node.ExpressionNode (nodeLoc)
import           NodeEditor.State.Global                    (State)
import qualified NodeEditor.View.NodeEditor                 as NodeEditor


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event Shortcut.Paste (Just cbd))) = Just $ sendPasteRequest cbd
handle (Shortcut (Shortcut.Event Shortcut.Copy   _        )) = Just sendCopyRequest
handle (Shortcut (Shortcut.Event Shortcut.Cut    _        )) = Just cutSelectionToClipboard
handle _ = Nothing

sendPasteRequest :: String -> Command State ()
sendPasteRequest cbd = flip paste cbd =<< NodeEditor.getMousePosition

sendCopyRequest :: Command State ()
sendCopyRequest = copy =<< view nodeLoc `fmap2` getSelectedNodes

cutSelectionToClipboard :: Command State()
cutSelectionToClipboard = sendCopyRequest >> removeSelectedNodes
