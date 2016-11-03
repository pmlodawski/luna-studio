module Reactive.Plugins.Core.Action.Clipboard where

import           Data.Aeson                                  (encode, decode)
import           Data.ByteString.Lazy.Char8                  (unpack)
import           Data.Text.Lazy                              (pack)
import           Data.Text.Lazy.Encoding                     (encodeUtf8)
import qualified Event.Clipboard                             as Clipboard
import           Event.Event                                 (Event(..))
import qualified Event.Keyboard                              as Keyboard
import           Event.Keyboard                              (KeyMods (..))
import           GHCJS.Types                                 (JSString)
import qualified Object.Widget                               as Widget
import qualified Object.Widget.Node                          as UINode
import           Reactive.Commands.Batch                     (removeNode)
import           Reactive.Commands.Command                   (Command, performIO)
import           Reactive.Commands.Graph.Selection           (selectedNodes)
import qualified Reactive.State.Global                       as Global
import           Reactive.State.Global                       (State)
import           Reactive.State.GraphSkeleton                as GraphSkeleton
import           Utils.PreludePlus

foreign import javascript safe "clipboard.copy($1)" copyStringToClipboard :: JSString -> IO ()

toAction :: Event -> Maybe (Command State ())
-- TODO(LJK): Bindowanie na akcje Copy zamiast na skrót ctrl+c
toAction (Keyboard _ (Keyboard.Event Keyboard.Down 'C' (KeyMods False True False False))) = Just $ copySelectionToClipboard
toAction (Keyboard _ (Keyboard.Event Keyboard.Down 'X' (KeyMods False True False False))) = Just $ cutSelectionToClipboard
toAction (Clipboard (Clipboard.Paste clipboardData)) = Just $ pasteFromClipboard clipboardData
toAction _ = Nothing

copySelectionToClipboard :: Command State ()
copySelectionToClipboard = do
  performIO $ putStrLn "Copy launched."
  nodeIds <- map (view $ Widget.widget . UINode.nodeId) <$> selectedNodes
  graph <- use Global.graph
  let subgraph = separateSubgraph nodeIds graph
  performIO $ copyStringToClipboard $ fromString $ unpack $ encode subgraph

cutSelectionToClipboard :: Command State()
cutSelectionToClipboard = do
  performIO $ putStrLn "Cut launched."
  copySelectionToClipboard
  nodeIds <- map (view $ Widget.widget . UINode.nodeId) <$> selectedNodes
  removeNode nodeIds


pasteFromClipboard :: Text -> Command State ()
pasteFromClipboard clipboardData = do
  performIO $ putStrLn "Paste launched."
  let skeleton = fromJust (decode $ encodeUtf8 clipboardData :: Maybe GraphSkeleton)
      nodes = view GraphSkeleton.nodesList skeleton
  performIO $ print $ head nodes
  graph <- use Global.graph
  performIO $ print clipboardData
