module Reactive.Commands.RegisterNode where

import           Control.Monad.State               hiding (State)
import           Data.Text.Lazy                    (Text)
import           Utils.PreludePlus
import           Utils.Vector                      (Vector2, toTuple)

import           Reactive.Commands.Command         (Command, performIO)
import qualified Reactive.State.Camera             as Camera
import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global

import qualified Object.Node                       as Node
import           Object.Widget                     (widget)
import qualified Object.Widget.Node                as UINode
import qualified Reactive.Commands.Batch           as BatchCmd
import           Reactive.Commands.Graph.Selection (selectedNodes)
import           Reactive.State.Global             (inRegistry)
import qualified Reactive.State.UIElements         as UIElements

import           Empire.API.Data.Node              (Node (..))
import qualified Empire.API.Data.Node              as Node
import           Empire.API.Data.NodeMeta          (NodeMeta (..))
import qualified Empire.API.Data.NodeMeta          as NodeMeta
import qualified JS.GoogleAnalytics                as GA

registerNode :: Text -> Command State ()
registerNode expr = do
    nodePos <- use $ Global.uiElements . UIElements.nsPos
    let nodeMeta = def & NodeMeta.position .~ (toTuple nodePos)
    selected   <- inRegistry selectedNodes
    let connectTo = case selected of
            []     -> Nothing
            [wf]   -> Just $ wf ^. widget . UINode.nodeId
            (_:_) -> Nothing
    BatchCmd.addNode expr nodeMeta connectTo
    GA.sendEvent $ GA.AddNode $ if isJust connectTo then GA.AutoConnect else GA.Simple
