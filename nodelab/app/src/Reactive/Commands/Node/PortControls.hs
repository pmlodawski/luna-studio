{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.PortControls
    ( makePortControl
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

isLiteral :: Getter Node Bool
isLiteral = to $ isLiteral' where
    isLiteral' node = (0 == (sum $ fmap isIn' portIds)) where
        portIds = Map.keys $ node ^. Node.ports
        isIn' :: PortId -> Int
        isIn' (OutPortId _) = 0
        isIn' (InPortId  _) = 1


makePortControl :: Node -> WidgetId -> WidgetId -> NodeId -> Port -> Command UIRegistry.State ()
makePortControl node outPortParent groupParent nodeId port = let portRef = toAnyPortRef nodeId $ port ^. Port.portId in
    case port ^. Port.portId of
        InPortId  (Arg ix) -> makeInPortControl groupParent portRef port
        OutPortId All      -> when (node ^. isLiteral) $ makeInPortControl groupParent portRef port -- TODO: change groupParent to outPortParent
        _ -> return ()

makeInPortControl :: WidgetId -> AnyPortRef -> Port -> Command UIRegistry.State ()
makeInPortControl parent portRef port = case port ^. Port.state of
    Port.NotConnected    -> do
        case port ^. Port.valueType . ValueType.toEnum of
            ValueType.Other -> return ()
            otherwise -> do
                let group = Group.create & Group.style . Group.padding .~ Style.Padding 0.0 0.0 0.0 Style.setLabelOffsetX
                groupId <- UICmd.register parent group (Layout.horizontalLayoutHandler 0.0)
                let label  = Label.create Style.setLabelSize (Text.pack $ port ^. Port.name)
                           & Label.position . x .~ Style.setLabelOffsetX
                    button = Button.create Style.setButtonSize "not set"
                    zeroValue = case port ^. Port.valueType . ValueType.toEnum of
                        ValueType.DiscreteNumber   -> DefaultValue.IntValue    def
                        ValueType.ContinuousNumber -> DefaultValue.DoubleValue def
                        ValueType.String           -> DefaultValue.StringValue def
                        ValueType.Bool             -> DefaultValue.BoolValue   False
                        _                          -> undefined
                    handlers = addHandler (Button.ClickedHandler $ \_ -> BatchCmd.setDefaultValue portRef (DefaultValue.Constant $ zeroValue)
                        ) mempty

                UICmd.register_ groupId label def
                UICmd.register_ groupId button handlers
    Port.Connected       -> do
        let widget = Label.create (Style.portControlSize & x -~ Style.setLabelOffsetX) (Text.pack $ (port ^. Port.name) <> " (connected)")
                   & Label.position . x .~ Style.setLabelOffsetX
        void $ UICmd.register parent widget def
    Port.WithDefault def -> void $ case port ^. Port.valueType . ValueType.toEnum of
        ValueType.DiscreteNumber -> do
            let label = port ^. Port.name
                value = fromMaybe 0 $ def ^? DefaultValue._Constant . DefaultValue._IntValue
                widget = DiscreteNumber.create Style.portControlSize (Text.pack $ label) value
                handlers = onValueChanged $ \val _ -> BatchCmd.setDefaultValue portRef (DefaultValue.Constant $ DefaultValue.IntValue val)
            UICmd.register parent widget handlers
        ValueType.ContinuousNumber -> do
            let label = port ^. Port.name
                value = fromMaybe 0.0 $ def ^? DefaultValue._Constant . DefaultValue._DoubleValue
                widget = ContinuousNumber.create Style.portControlSize (Text.pack $ label) value
                handlers = onValueChanged $ \val _ -> BatchCmd.setDefaultValue portRef (DefaultValue.Constant $ DefaultValue.DoubleValue val)
            UICmd.register parent widget handlers
        ValueType.String -> do
            let label = port ^. Port.name
                value = fromMaybe "" $ def ^? DefaultValue._Constant . DefaultValue._StringValue
                widget = LabeledTextBox.create Style.portControlSize (Text.pack $ label) (Text.pack $ value)
                handlers = onValueChanged $ \val _ -> BatchCmd.setDefaultValue portRef (DefaultValue.Constant $ DefaultValue.StringValue $ Text.unpack val)
            UICmd.register parent widget handlers
        ValueType.Bool -> do
            let label = port ^. Port.name
                value = fromMaybe True $ def ^? DefaultValue._Constant . DefaultValue._BoolValue
                widget = Toggle.create Style.portControlSize (Text.pack $ label) value
                handlers = onValueChanged $ \val _ -> BatchCmd.setDefaultValue portRef (DefaultValue.Constant $ DefaultValue.BoolValue val)
            UICmd.register parent widget handlers
        ValueType.Other -> do
            let widget = Label.create (Style.portControlSize & x -~ Style.setLabelOffsetX) (Text.pack $ (port ^. Port.name) )
                       & Label.position . x .~ Style.setLabelOffsetX

            UICmd.register parent widget mempty




