module Reactive.Commands.RegisterNode where

import           Utils.PreludePlus

import           Reactive.Commands.Command     (Command, performIO)
import           Reactive.Commands.PendingNode (renderPending)
import           Reactive.State.Global         (State)
import qualified Reactive.State.Global         as Global
import qualified Reactive.State.Camera         as Camera

import           Object.Node             (Node(..))
import qualified Object.Node             as Node
import           Utils.Vector            (Vector2)
import           Data.Text.Lazy          (Text)
import qualified Utils.MockHelper        as MockHelper
import           Control.Monad.State     hiding (State)
import qualified BatchConnector.Commands as BatchCmd
import           Reactive.State.Graph (genNodeId)

registerNode :: Text -> Command State ()
registerNode expr = do
    nodeId  <- zoom Global.graph $ gets genNodeId
    camera  <- use $ Global.camera . Camera.camera
    nodePos <- uses Global.mousePos $ Camera.screenToWorkspace camera
    let node = Node nodeId nodePos expr (Node.createPorts expr) (MockHelper.getNodeType expr)
    workspace <- use Global.workspace
    performIO $ BatchCmd.addNode workspace node
    renderPending node
