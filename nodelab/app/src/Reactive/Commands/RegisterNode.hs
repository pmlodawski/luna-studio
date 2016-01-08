module Reactive.Commands.RegisterNode where

import           Utils.PreludePlus

import           Reactive.Commands.Command     (Command, performIO)
import           Reactive.Commands.PendingNode (renderPending)
import           Reactive.State.Global         (State)
import qualified Reactive.State.Global         as Global
import qualified Reactive.State.Camera         as Camera

import qualified Object.Node             as Node
import           Utils.Vector            (Vector2, toTuple)
import           Data.Text.Lazy          (Text)
import           Control.Monad.State     hiding (State)
import qualified BatchConnector.Commands as BatchCmd
import           Reactive.State.Graph (genNodeId)
import           Empire.API.Data.Node (Node(..))
import           Empire.API.Data.NodeMeta (NodeMeta(..))
import qualified Empire.API.Data.Node as Node

registerNode :: Text -> Command State ()
registerNode expr = do
    nodeId  <- zoom Global.graph $ gets genNodeId
    camera  <- use $ Global.camera . Camera.camera
    nodePos <- uses Global.mousePos $ Camera.screenToWorkspace camera
    let nodeMeta = NodeMeta $ toTuple nodePos
    workspace <- use Global.workspace
    performIO $ BatchCmd.addNode workspace expr nodeMeta 42
    -- renderPending node
