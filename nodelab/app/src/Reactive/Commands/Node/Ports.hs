{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.Ports
    ( displayPorts
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

import           Reactive.Commands.Node.PortControls (makePortControl)

nodePorts :: WidgetId -> Command UIRegistry.State [WidgetId]
nodePorts id = do
    children <- UICmd.children id
    let isPort id = (UIRegistry.lookupTypedM id :: UIRegistry.LookupFor PortModel.Port) >>= return . isJust
    filterM isPort children

makePorts :: Node -> [PortModel.Port]
makePorts node = makePort <$> ports where
    nodeId  = node ^. Node.nodeId
    makePort port = PortModel.Port portRef angle (portCount portId) isOnly (colorPort port) False where
        portRef = toAnyPortRef nodeId portId
        angle   = portDefaultAngle (portCount portId) (port ^. Port.portId)
        portId  = port ^. Port.portId
        isOnly  = 0 == portCount (InPortId Self)
    ports = Map.elems $ node ^. Node.ports
    portIds = Map.keys $  node ^. Node.ports
    portCount :: PortId -> Int
    portCount (OutPortId _) = sum $ fmap isOut portIds where
        isOut :: PortId -> Int
        isOut (OutPortId _) = 1
        isOut (InPortId  _) = 0
    portCount (InPortId  _) = sum $ fmap isIn  portIds where
        isIn :: PortId -> Int
        isIn (OutPortId _) = 0
        isIn (InPortId (Arg _)) = 1
        isIn (InPortId Self) = 0




displayPorts :: WidgetId -> Node -> Command UIRegistry.State ()
displayPorts id node = do
    nodeId <- UICmd.get id Model.nodeId
    oldPorts <- nodePorts id
    mapM_ UICmd.removeWidget oldPorts

    groupId <- Node.portControlsGroupId id
    portControls <- UICmd.children groupId
    mapM_ UICmd.removeWidget portControls

    outPortGroupId <- Node.outPortControlsGroupId id
    outPortControls <- UICmd.children outPortGroupId
    mapM_ UICmd.removeWidget outPortControls

    inLabelsGroupId <- Node.inLabelsGroupId id
    inLabels <- UICmd.children inLabelsGroupId
    mapM_ UICmd.removeWidget inLabels

    outLabelsGroupId <- Node.outLabelsGroupId id
    outLabels <- UICmd.children outLabelsGroupId
    mapM_ UICmd.removeWidget outLabels

    let newPorts = makePorts node

    forM_ newPorts $ \p -> UICmd.register id p def
    forM_ (node ^. Node.ports) $ \p -> makePortControl node outPortGroupId groupId (node ^. Node.nodeId) p
    forM_ (node ^. Node.ports) $ \p -> case p ^. Port.portId of
        InPortId  Self -> return ()
        InPortId  _    -> makePortLabel inLabelsGroupId p
        OutPortId _    -> makePortLabel outLabelsGroupId p

vtToText :: Getter ValueType Text
vtToText = to $ \v -> case v of
    ValueType.AnyType     -> "*"
    ValueType.TypeIdent a -> Text.pack $ toString a



makePortLabel :: WidgetId -> Port -> Command UIRegistry.State ()
makePortLabel parent port = do
    let align = case port ^. Port.portId of
            InPortId  _ -> Label.Right
            OutPortId _ -> Label.Left
        label = Label.create (Vector2 360 15) text & Label.alignment .~ align
        text  = (Text.pack $ port ^. Port.name) <> " :: " <> portType
        portType = port ^. Port.valueType . vtToText
    UICmd.register_ parent label def
