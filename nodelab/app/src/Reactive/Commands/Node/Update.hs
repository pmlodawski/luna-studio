{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.Update
    ( updateNode
    , updateNodeValue
    , updateNodeProfilingData
    , updateExpression
    ) where

import           Utils.PreludePlus

import           Control.Monad.State                  (modify)

import qualified Object.Widget.Node                   as Model

import           Reactive.Commands.Command            (Command)
import           Reactive.Commands.Graph              (nodeIdToWidgetId, updateConnectionsForNodes)
import qualified Reactive.Commands.UIRegistry         as UICmd
import           Reactive.State.Global                (State, inRegistry)
import qualified Reactive.State.Global                as Global
import qualified Reactive.State.Graph                 as Graph

import           Empire.API.Data.Node                 (Node, NodeId)
import qualified Empire.API.Data.Node                 as Node
import qualified Empire.API.Graph.NodeResultUpdate    as NodeResult

import           Reactive.Commands.Node.Create        (addNode)
import           Reactive.Commands.Node.Ports         (displayPorts)
import           Reactive.Commands.Node.TextResult    (nodeValueToText)
import           Reactive.Commands.Node.Visualization (removeVisualization, visualizeError, visualizeNodeValueReprs)
import qualified Reactive.Commands.Batch              as BatchCmd


updateNode :: Node -> Command State ()
updateNode node = do
    let nodeId  = node ^. Node.nodeId
    inGraph <- preuse $ Global.graph . Graph.nodesMap . ix nodeId
    case inGraph of
        Just existingNode -> updateExistingNode node
        Nothing           -> addNode            node

updateExistingNode :: Node -> Command State ()
updateExistingNode node = do
    let nodeId  = node ^. Node.nodeId
    maybeWidgetId <- nodeIdToWidgetId nodeId
    zoom Global.graph $ modify (Graph.addNode node)
    withJust maybeWidgetId $ \widgetId -> do
        displayPorts widgetId node

        case node ^. Node.nodeType of
            Node.ExpressionNode expression -> do
                inRegistry $ UICmd.update_ widgetId $ Model.expression .~ expression
            _ -> return ()
        -- TODO: obsluzyc to ze moga zniknac polaczenia
    updateConnectionsForNodes [nodeId]

updateNodeValue :: NodeId -> NodeResult.NodeValue -> Command State ()
updateNodeValue id val = do
    widgetId <- nodeIdToWidgetId id
    inRegistry $ do
        withJust widgetId $ \widgetId -> do
            removeVisualization widgetId
            case val of
                NodeResult.Value name [] -> do
                    UICmd.update_ widgetId $ Model.value   .~ name
                    UICmd.update_ widgetId $ Model.isError .~ False
                NodeResult.Value name valueReprs -> do
                    UICmd.update_ widgetId $ Model.value   .~ name
                    UICmd.update_ widgetId $ Model.isError .~ False
                    visualizeNodeValueReprs widgetId valueReprs
                NodeResult.Error msg -> do
                    UICmd.update_ widgetId $ Model.value   .~ "Error!"
                    UICmd.update_ widgetId $ Model.isError .~ True
                    visualizeError widgetId msg

updateNodeProfilingData :: NodeId -> Integer -> Command State ()
updateNodeProfilingData id execTime = do
    widgetId <- nodeIdToWidgetId id
    inRegistry $ withJust widgetId $ flip UICmd.update_ $ Model.execTime ?~ execTime

updateExpression :: NodeId -> Text -> Command State ()
updateExpression nodeId expr = do
    -- BatchCmd.
    return ()
