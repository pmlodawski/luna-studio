module Reactive.Plugins.Core.Action.Clipboard where

import           Data.Aeson                        (decode, encode)
import           Data.ByteString.Lazy.Char8        (unpack)
import           Data.Text.Lazy.Encoding           (encodeUtf8)
import           Empire.API.Data.Node              (Node)
import qualified Empire.API.Data.Node              as Node
import qualified Empire.API.Data.NodeMeta          as NodeMeta
import qualified Event.Clipboard                   as Clipboard
import           Event.Event                       (Event (..))
import           Event.Keyboard                    (KeyMods (..))
import qualified Event.Keyboard                    as Keyboard
import qualified Object.Widget                     as Widget
import qualified Object.Widget.Node                as UINode
import           Reactive.Commands.Batch           (addSubgraph)
import           Reactive.Commands.Command         (Command, performIO)
import           Reactive.Commands.Graph.Selection (selectedNodes)
import           Reactive.Commands.Node.Remove     (removeSelectedNodes)
import qualified Reactive.State.Camera             as Camera
import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global
import           Reactive.State.GraphSkeleton      as GraphSkeleton
import           Utils.PreludePlus
import           Utils.Vector                      (Vector2 (..))

foreign import javascript safe "clipboard.copy($1)" copyStringToClipboard :: JSString -> IO ()

toAction :: Event -> Maybe (Command State ())
-- TODO(LJK): Bind to event copy and cut instead of keyboard shortcut
toAction (Keyboard _ (Keyboard.Event Keyboard.Down 'C' (KeyMods False True False False))) = Just $ copySelectionToClipboard
toAction (Keyboard _ (Keyboard.Event Keyboard.Down 'X' (KeyMods False True False False))) = Just $ cutSelectionToClipboard
toAction (Clipboard (Clipboard.Paste clipboardData)) = Just $ pasteFromClipboard clipboardData
toAction _ = Nothing

copySelectionToClipboard :: Command State ()
copySelectionToClipboard = do
  nodeIds <- map (view $ Widget.widget . UINode.nodeId) <$> selectedNodes
  graph <- use Global.graph
  let subgraph = separateSubgraph nodeIds graph
  performIO $ copyStringToClipboard $ fromString $ unpack $ encode subgraph

cutSelectionToClipboard :: Command State()
cutSelectionToClipboard = copySelectionToClipboard >> removeSelectedNodes

pasteFromClipboard :: Text -> Command State ()
pasteFromClipboard clipboardData = do
  let skeleton = fromJust (decode $ encodeUtf8 clipboardData :: Maybe GraphSkeleton)
      nodes = view GraphSkeleton.nodesList skeleton
      connections = view GraphSkeleton.connectionsList skeleton
  mousePos <- use $ Global.mousePos
  (Vector2 mousePosX mousePosY) <- zoom Global.camera $ Camera.screenToWorkspaceM mousePos
  let shiftX = mousePosX - (minimum $ map (\node -> fst (node ^. (Node.nodeMeta . NodeMeta.position))) nodes)
      shiftY = mousePosY - (minimum $ map (\node -> snd (node ^. (Node.nodeMeta . NodeMeta.position))) nodes)
      shiftNodeX :: Node -> Node
      shiftNodeX node = node & (Node.nodeMeta . NodeMeta.position . _1) %~ (+shiftX)
      shiftNodeY :: Node -> Node
      shiftNodeY node = node & (Node.nodeMeta . NodeMeta.position . _2) %~ (+shiftY)
      shiftNode :: Node -> Node
      shiftNode node = shiftNodeY $ shiftNodeX node
      nodes' = map (shiftNode) nodes
  addSubgraph nodes' connections
