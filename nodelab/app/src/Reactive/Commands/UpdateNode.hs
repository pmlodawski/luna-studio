module Reactive.Commands.UpdateNode where

import           Utils.PreludePlus
import           Data.Text.Lazy    (Text)

import           Object.Object     (NodeId)
import qualified Object.Node       as Node
import qualified JS.NodeGraph      as UI

import           Reactive.Commands.Command (Command, performIO)
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Graph      as Graph

import qualified BatchConnector.Monadic.Commands as BatchCmd

updateNode :: NodeId -> Text -> Command State ()
updateNode nodeId expr = do
    Global.graph . Graph.nodesMap . ix nodeId . Node.expression .= expr
    nodeMay <- preuse $ Global.graph . Graph.nodesMap . ix nodeId
    case nodeMay of
        Just node -> do
            performIO $ UI.updateLabel node
            zoom Global.workspace $ BatchCmd.updateNode node
            zoom Global.workspace BatchCmd.runMain
        Nothing   -> return ()
