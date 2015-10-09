module Reactive.Plugins.Core.Action.Commands.RegisterNode where

import           Utils.PreludePlus

import           Reactive.Plugins.Core.Action.Commands.Command     (Command, performIO)
import           Reactive.Plugins.Core.Action.Commands.PendingNode (renderPending)
import           Reactive.Plugins.Core.Action.State.Global         (State)
import qualified Reactive.Plugins.Core.Action.State.Global         as Global

import qualified JS.Camera               as Camera
import           Object.Node             (Node(..))
import qualified Object.Node             as Node
import           Utils.Vector            (Vector2)
import           Data.Text.Lazy          (Text)
import qualified Utils.MockHelper        as MockHelper
import           Control.Monad.State     hiding (State)
import qualified BatchConnector.Commands as BatchCmd

registerNode :: Text -> Command State ()
registerNode expr = do
    nodeId  <- gets Global.genNodeId
    camera  <- gets Global.toCamera
    nodePos <- uses Global.mousePos $ Camera.screenToWorkspace camera
    let node = Node nodeId False nodePos expr (Node.createPorts expr) (MockHelper.getNodeType expr)
    workspace <- use Global.workspace
    performIO $ BatchCmd.addNode workspace node
    renderPending node
