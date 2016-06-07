{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.Create
    ( addNode
    , addDummyNode
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

import           Reactive.Commands.Node.Ports (displayPorts)


addNode :: Node -> Command State ()
addNode node = do
    -- unrenderPending node
    zoom Global.graph $ modify (Graph.addNode node)
    widgetId <- zoom Global.uiRegistry $ registerNode node
    Node.selectNode' Node.performSelect widgetId

addDummyNode :: Node -> Command State ()
addDummyNode dummyNode = do
    mayNode <- preuse $ Global.graph . Graph.nodesMap . ix (dummyNode ^. Node.nodeId)
    case mayNode of
        Just _  -> return ()
        Nothing -> addNode dummyNode


registerNode :: Node -> Command UIRegistry.State WidgetId
registerNode node = do
    let nodeModel = Model.fromNode node
    nodeWidget <- UICmd.register sceneGraphId nodeModel (nodeHandlers node)

    displayPorts nodeWidget node
    focusNode nodeWidget
    return nodeWidget



nodeHandlers :: Node -> HTMap
nodeHandlers node = addHandler (UINode.RemoveNodeHandler removeSelectedNodes)
                  $ addHandler (UINode.RenameNodeHandler $ \_ nodeId name -> BatchCmd.renameNode nodeId name)
                  $ addHandler (UINode.ChangeInputNodeTypeHandler $ \_ nodeId name -> BatchCmd.setInputNodeType nodeId name)
                  $ addHandler (UINode.FocusNodeHandler    $ \id -> inRegistry $ focusNode id)
                  $ addHandler (UINode.ExpandNodeHandler   $ inRegistry $ expandSelectedNodes)
                  $ addHandler (UINode.NodeRequiredHandler $ \nid val -> updateNodeRequired nid val)
                  $ addEnterNodeHandler where
                        addEnterNodeHandler = case node ^. Node.nodeType of
                            Node.FunctionNode _ -> addHandler (UINode.EnterNodeHandler $ enterNode $ Breadcrumb.Function $ Text.unpack $ node ^. Node.name) mempty
                            Node.ModuleNode     -> addHandler (UINode.EnterNodeHandler $ enterNode $ Breadcrumb.Module   $ Text.unpack $ node ^. Node.name) mempty
                            _                   -> mempty

expandSelectedNodes :: Command UIRegistry.State ()
expandSelectedNodes = do
    sn <- selectedNodes
    let allSelected = all (view $ widget . Model.isExpanded) sn
        update      = if allSelected then Model.isExpanded %~ not
                                     else Model.isExpanded .~ True
    forM_ sn $ \wf -> do
        let id = wf ^. objectId
        UICmd.update_ id update
        UICmd.moveBy  id (Vector2 0 0) -- FIXME: trigger moved handler for html widgets

updateNodeRequired :: NodeId -> Bool -> Command State ()
updateNodeRequired id val = do
    Global.graph . Graph.nodesMap . ix id . Node.nodeMeta . NodeMeta.isRequired .= val
    newMeta <- preuse $ Global.graph . Graph.nodesMap . ix id . Node.nodeMeta
    withJust newMeta $ \newMeta -> BatchCmd.updateNodeMeta id newMeta
