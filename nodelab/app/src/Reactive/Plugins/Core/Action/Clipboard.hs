module Reactive.Plugins.Core.Action.Clipboard where

import           Data.Aeson                                  (encode, decode)
import           Data.ByteString.Lazy.Char8                  (pack, unpack)
import           Empire.API.Data.PortRef                     as PortRef
import           Event.Event                                 (Event(..))
import           Event.Keyboard                              (KeyMods (..))
import qualified Event.Keyboard                              as Keyboard
import qualified Event.Clipboard                             as Clipboard
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal                               (toJSVal)
import           GHCJS.Types                                 (JSString, JSVal)
import qualified Object.Widget                               as Widget
import qualified Object.Widget.Node                          as UINode
import           Reactive.Commands.Command                   (Command, performIO)
import           Reactive.Commands.Graph.Selection           (selectedNodes)
import           Reactive.State.Global                       (State)
import qualified Reactive.State.Global                       as Global
import           Reactive.State.Graph                        (separateSubgraph)
import           Utils.PreludePlus

foreign import javascript safe "clipboard.copy($1)" copyStringToClipboard :: JSString -> IO ()

toAction :: Event -> Maybe (Command State ())
-- TODO(LJK): Bindowanie na akcje Copy zamiast na skrót ctrl+c
toAction (Keyboard _ (Keyboard.Event Keyboard.Down 'C' (KeyMods False True False False))) = Just $ copySelectionToClipboard
toAction (Clipboard (Clipboard.Paste clipboardData)) = Just $ pasteFromClipboard clipboardData
toAction _ = Nothing

copySelectionToClipboard :: Command State ()
copySelectionToClipboard = do
  performIO $ putStrLn "Copy launched."
  nodeIds <- map (view $ Widget.widget . UINode.nodeId) <$> selectedNodes
  graph <- use Global.graph
  let subgraph = separateSubgraph nodeIds graph
  performIO $ copyStringToClipboard $ fromString $ unpack $ encode subgraph

pasteFromClipboard :: Text -> Command State ()
pasteFromClipboard clipboardData = do
  performIO $ putStrLn "Paste launched."
  performIO $ print clipboardData
