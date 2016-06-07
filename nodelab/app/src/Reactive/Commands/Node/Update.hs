{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.Update
    ( updateNode
    , updateNodeValue
    , updateNodeProfilingData
    ) where

import           Utils.PreludePlus
import           Utils.Vector

import           Control.Monad.State               hiding (State)
import           Data.Hashable                     (hash)
import           Data.List                         (intercalate)
import           Data.List.Split                   (wordsBy)
import qualified Data.Map.Lazy                     as Map
import qualified Data.Text.Lazy                    as Text
import           GHC.Float                         (double2Float)

import           Object.UITypes                    (WidgetId)
import           Object.Widget                     (objectId, widget)
import qualified Object.Widget.Button              as Button
import qualified Object.Widget.DataFrame           as DataFrame
import qualified Object.Widget.Group               as Group
import qualified Object.Widget.Label               as Label
import           Object.Widget.LabeledTextBox      (LabeledTextBox (..))
import qualified Object.Widget.LabeledTextBox      as LabeledTextBox
import qualified Object.Widget.LongText            as LongText
import qualified Object.Widget.Node                as Model
import           Object.Widget.Number.Continuous   (ContinuousNumber (..))
import qualified Object.Widget.Number.Continuous   as ContinuousNumber
import           Object.Widget.Number.Discrete     (DiscreteNumber (..))
import qualified Object.Widget.Number.Discrete     as DiscreteNumber
import qualified Object.Widget.Plots.Image         as Image
import qualified Object.Widget.Plots.ScatterPlot   as ScatterPlot
import qualified Object.Widget.Port                as PortModel
import           Object.Widget.Toggle              (Toggle (..))
import qualified Object.Widget.Toggle              as Toggle
import qualified UI.Handlers.Button                as Button
import           UI.Handlers.Generic               (onValueChanged)
import qualified UI.Handlers.LabeledTextBox        as LabeledTextBox
import qualified UI.Handlers.Node                  as Node
import qualified UI.Handlers.Number.Continuous     as ContinuousNumber
import qualified UI.Handlers.Number.Discrete       as DiscreteNumber
import qualified UI.Handlers.Toggle                as Toggle

import           Reactive.Commands.Command         (Command, performIO)
import           Reactive.Commands.EnterNode       (enterNode)
import           Reactive.Commands.Graph           (colorPort, focusNode, nodeIdToWidgetId, portDefaultAngle,
                                                    updateConnections, updateNodeMeta)
-- import           Reactive.Commands.PendingNode   (unrenderPending)
import           Reactive.Commands.RemoveNode      (removeSelectedNodes)
import           Reactive.Commands.Selection       (selectedNodes)
import qualified Reactive.Commands.UIRegistry      as UICmd
import           Reactive.State.Global             (State, inRegistry)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph
import           Reactive.State.UIRegistry         (addHandler, sceneGraphId)
import qualified Reactive.State.UIRegistry         as UIRegistry

import qualified Reactive.Commands.Batch           as BatchCmd
import qualified JS.NodeGraph                      as UI

import           Data.HMap.Lazy                    (HTMap)
import qualified Data.HMap.Lazy                    as HMap
import           Data.Map.Lazy                     (Map)
import qualified Data.Map.Lazy                     as Map
import           UI.Handlers.Generic               (ValueChangedHandler (..), triggerValueChanged)
import qualified UI.Handlers.Node                  as UINode
import           UI.Layout                         as Layout
import qualified UI.Registry                       as UIR
import qualified UI.Scene
import qualified UI.Widget                         as UIT

import qualified Style.Node                        as Style
import qualified Style.Types                       as Style

import qualified Empire.API.Data.Breadcrumb        as Breadcrumb
import           Empire.API.Data.DefaultValue      (Value (..))
import qualified Empire.API.Data.DefaultValue      as DefaultValue
import qualified Empire.API.Data.Error             as LunaError
import           Empire.API.Data.Node              (Node, NodeId)
import qualified Empire.API.Data.Node              as Node
import           Empire.API.Data.NodeMeta          (NodeMeta)
import qualified Empire.API.Data.NodeMeta          as NodeMeta
import           Empire.API.Data.Port              (InPort (..), InPort (..), OutPort (..), Port (..), PortId (..))
import qualified Empire.API.Data.Port              as Port
import           Empire.API.Data.PortRef           (AnyPortRef (..), InPortRef (..), toAnyPortRef)
import           Empire.API.Data.TypeRep           (TypeRep)
import           Empire.API.Data.ValueType         (ValueType (..))
import qualified Empire.API.Data.ValueType         as ValueType
import qualified Empire.API.Graph.NodeResultUpdate as NodeResult

import qualified Object.Widget.Graphics            as G
import qualified Utils.Shader                      as Shader
import qualified Graphics.API                      as GR

import           Reactive.Commands.Node.Create (addNode)
import           Reactive.Commands.Node.Ports (displayPorts)
import           Reactive.Commands.Node.Visualization (removeVisualization, visualizeError, visualizeNodeValue)
import           Reactive.Commands.Node.TextResult (nodeValueToText)

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
    maybeWidgetId <- inRegistry $ nodeIdToWidgetId nodeId
    zoom Global.graph $ modify (Graph.addNode node)
    forM_ maybeWidgetId $ \widgetId -> do
        updateNodeMeta nodeId $ node ^. Node.nodeMeta
        inRegistry $ displayPorts widgetId node

        case node ^. Node.nodeType of
            Node.ExpressionNode expression -> do
                inRegistry $ UICmd.update_ widgetId $ Model.expression .~ expression
            _ -> return ()
        -- TODO: obsluzyc to ze moga zniknac polaczenia
    updateConnections


updateNodeValue :: NodeId -> NodeResult.NodeValue -> Command State ()
updateNodeValue id val = inRegistry $ do
    widgetId <- nodeIdToWidgetId id
    withJust widgetId $ \widgetId -> do
        removeVisualization widgetId
        case val of
            NodeResult.Value val -> do
                UICmd.update_ widgetId $ Model.value   .~ (nodeValueToText val)
                UICmd.update_ widgetId $ Model.isError .~ False
                visualizeNodeValue widgetId val
            NodeResult.NoValue -> do
                UICmd.update_ widgetId $ Model.value   .~ ""
                UICmd.update_ widgetId $ Model.isError .~ False
            NodeResult.Error msg -> do
                UICmd.update_ widgetId $ Model.value   .~ "Error!"
                UICmd.update_ widgetId $ Model.isError .~ True
                visualizeError widgetId msg

updateNodeProfilingData :: NodeId -> Integer -> Command State ()
updateNodeProfilingData id execTime = inRegistry $ do
    widgetId <- nodeIdToWidgetId id
    withJust widgetId $ flip UICmd.update_ $ Model.execTime ?~ execTime
