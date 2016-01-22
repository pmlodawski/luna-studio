{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.AddNode
    ( addNode
    , updateNode
    , updateNodeValue
    ) where

import           Utils.PreludePlus
import           Utils.Vector

import qualified Data.Text.Lazy        as Text
import qualified Data.Map.Lazy         as Map
import           Control.Monad.State   hiding (State)
import           GHC.Float             (double2Float)

import           Object.Widget         ()
import           Object.UITypes        (WidgetId)
import qualified Object.Widget.Node    as Model
import qualified Object.Widget.Button  as Button
import qualified Object.Widget.Label   as Label
import qualified Object.Widget.Port    as PortModel
import qualified Object.Widget.Group   as Group
import           Object.Widget.Number.Discrete     (DiscreteNumber(..))
import qualified Object.Widget.Number.Discrete     as DiscreteNumber
import qualified UI.Handlers.Number.Discrete       as DiscreteNumber
import           Object.Widget.Number.Continuous   (ContinuousNumber(..))
import qualified Object.Widget.Number.Continuous   as ContinuousNumber
import qualified UI.Handlers.Number.Continuous     as ContinuousNumber
import           Object.Widget.LabeledTextBox      (LabeledTextBox(..))
import qualified Object.Widget.LabeledTextBox      as LabeledTextBox
import qualified UI.Handlers.LabeledTextBox        as LabeledTextBox
import           Object.Widget.Toggle              (Toggle(..))
import qualified Object.Widget.Toggle              as Toggle
import qualified UI.Handlers.Toggle                as Toggle
import qualified UI.Handlers.Button                as Button

import qualified Reactive.State.Global         as Global
import           Reactive.State.Global         (State, inRegistry)
import qualified Reactive.State.Graph          as Graph
import           Reactive.State.UIRegistry     (sceneGraphId, addHandler)
import qualified Reactive.State.UIRegistry     as UIRegistry
import           Reactive.Commands.EnterNode   (enterNode)
import           Reactive.Commands.Graph       (focusNode, updatePortAngles, portDefaultAngle, nodeIdToWidgetId, updateNodeMeta)
import           Reactive.Commands.RemoveNode  (removeSelectedNodes)
import           Reactive.Commands.Command     (Command, performIO)
import           Reactive.Commands.PendingNode (unrenderPending)
import qualified Reactive.Commands.UIRegistry  as UICmd

import qualified BatchConnector.Commands as BatchCmd
import qualified JS.NodeGraph            as UI

import qualified UI.Widget.Node   as UINode
import qualified UI.Registry      as UIR
import qualified UI.Widget        as UIT
import qualified UI.Scene
import qualified Data.HMap.Lazy   as HMap
import           Data.HMap.Lazy   (HTMap)
import qualified Data.Map.Lazy    as Map
import           Data.Map.Lazy    (Map)
import           UI.Handlers.Generic (triggerValueChanged, ValueChangedHandler(..))
import           UI.Layout                        as Layout

import           Empire.API.Data.Node (Node, NodeId)
import qualified Empire.API.Data.Node as Node
import           Empire.API.Data.Port (Port(..), PortId(..), InPort(..))
import qualified Empire.API.Data.Port as Port
import qualified Empire.API.Data.DefaultValue as DefaultValue
import           Empire.API.Data.ValueType (ValueType(..))
import qualified Empire.API.Data.ValueType as ValueType
import           Empire.API.Data.PortRef (AnyPortRef(..), toAnyPortRef, InPortRef(..))

addNode :: Node -> Command State ()
addNode node = do
    unrenderPending node
    zoom Global.graph $ modify (Graph.addNode node)
    zoom Global.uiRegistry $ registerNode node
    updatePortAngles

colorVT _ = 11
colorPort (Port.InPortId Port.Self) = 12
colorPort _ = 11

registerNode :: Node -> Command UIRegistry.State ()
registerNode node = do
    let nodeModel = Model.node node
    nodeWidget <- UICmd.register sceneGraphId nodeModel (nodeHandlers node)

    displayPorts nodeWidget node
    focusNode nodeWidget


nodePorts :: WidgetId -> Command UIRegistry.State [WidgetId]
nodePorts id = do
    children <- UICmd.children id
    let isPort id = (UIRegistry.lookupTypedM id :: UIRegistry.LookupFor PortModel.Port) >>= return . isJust
    filterM isPort children

makePorts :: Node -> [PortModel.Port]
makePorts node = makePort <$> (Map.elems $ node ^. Node.ports) where
    nodeId  = node ^. Node.nodeId
    makePort port = PortModel.Port portRef angle (colorPort $ port ^. Port.portId ) where
        portRef = toAnyPortRef nodeId (port ^. Port.portId)
        angle   = portDefaultAngle ((length $ node ^. Node.ports) - 1) (port ^. Port.portId)

displayPorts :: WidgetId -> Node -> Command UIRegistry.State ()
displayPorts id node = do
    nodeId <- UICmd.get id Model.nodeId
    oldPorts <- nodePorts id
    mapM_ UICmd.removeWidget oldPorts

    (groupId:_) <- UICmd.children id
    portControls <- UICmd.children groupId
    mapM_ UICmd.removeWidget portControls

    let newPorts = makePorts node

    gr <- nodeExpandedGroup id
    forM_ newPorts $ \p -> UICmd.register id p def
    forM_ (node ^. Node.ports) $ \p -> makePortControl gr (node ^. Node.nodeId) p

nodeHandlers :: Node -> HTMap
nodeHandlers node = addHandler (UINode.RemoveNodeHandler removeSelectedNodes)
                  $ addHandler (UINode.FocusNodeHandler $ \id -> zoom Global.uiRegistry (focusNode id))
                  $ mempty

updateNode :: Node -> Command State ()
updateNode node = do
    let nodeId  = node ^. Node.nodeId
    maybeWidgetId <- inRegistry $ nodeIdToWidgetId nodeId
    zoom Global.graph $ modify (Graph.addNode node)
    forM_ maybeWidgetId $ \widgetId -> do
        inRegistry $ do
            displayPorts widgetId node
            UICmd.update widgetId $ Model.expression .~ (node ^. Node.expression)
        updatePortAngles
        updateNodeMeta nodeId $ node ^. Node.nodeMeta
        -- TODO: obsluzyc to ze moga zniknac polaczenia

nodeExpandedGroup :: WidgetId -> Command UIRegistry.State WidgetId
nodeExpandedGroup id = do
    children <- UICmd.children id
    let isPort id = (UIRegistry.lookupTypedM id :: UIRegistry.LookupFor Group.Group) >>= return . isJust
    groups <- filterM isPort children
    return $ head groups

onValueChanged :: Typeable a => (a -> WidgetId -> Command Global.State ()) -> HTMap
onValueChanged h = addHandler (ValueChangedHandler h) mempty

makePortControl :: WidgetId -> NodeId -> Port -> Command UIRegistry.State ()
makePortControl parent nodeId port = case port ^. Port.portId of
    OutPortId _ -> return ()
    InPortId inPort -> makeInPortControl parent nodeId inPort port

makeInPortControl :: WidgetId -> NodeId -> InPort -> Port -> Command UIRegistry.State ()
makeInPortControl parent nodeId inPort port = case port ^. Port.state of
    Port.NotConnected    -> do
        groupId <- UICmd.register parent Group.create (Layout.horizontalLayoutHandler 10)
        let label  = Label.Label def (Vector2 140 20) (Text.pack $ show inPort)
            button = Button.create (Vector2 50 20) ("Set")
            handlers = addHandler (Button.ClickedHandler $ \_ -> do
                workspace <- use Global.workspace
                performIO $ BatchCmd.setDefaultValue workspace (InPortRef nodeId inPort) (DefaultValue.Constant $ DefaultValue.IntValue def)
                ) mempty

        UICmd.register_ groupId label def
        UICmd.register_ groupId button handlers
    Port.Connected       -> do
        let widget = Label.create (Text.pack $ show inPort <> " (connected)")
        void $ UICmd.register parent widget def
    Port.WithDefault def -> void $ case port ^. Port.valueType . ValueType.toEnum of
        ValueType.DiscreteNumber -> do
            let label = show inPort
                value = fromMaybe 0 $ def ^? DefaultValue._Constant . DefaultValue._IntValue
                widget = DiscreteNumber.create (Vector2 200 20) (Text.pack $ show inPort) value
                handlers = onValueChanged $ \val _ -> do
                    workspace <- use Global.workspace
                    performIO $ BatchCmd.setDefaultValue workspace (InPortRef nodeId inPort) (DefaultValue.Constant $ DefaultValue.IntValue val)
            UICmd.register parent widget handlers
        ValueType.ContinuousNumber -> do
            let label = show inPort
                value = fromMaybe 0.0 $ def ^? DefaultValue._Constant . DefaultValue._DoubleValue
                widget = ContinuousNumber.create (Vector2 200 20) (Text.pack $ show inPort) value
                handlers = onValueChanged $ \val _ -> do
                    workspace <- use Global.workspace
                    performIO $ BatchCmd.setDefaultValue workspace (InPortRef nodeId inPort) (DefaultValue.Constant $ DefaultValue.DoubleValue val)
            UICmd.register parent widget handlers
        ValueType.String -> do
            let label = show inPort
                value = fromMaybe "" $ def ^? DefaultValue._Constant . DefaultValue._StringValue
                widget = LabeledTextBox.create (Vector2 200 20) (Text.pack $ show inPort) (Text.pack $ value)
                handlers = onValueChanged $ \val _ -> do
                    workspace <- use Global.workspace
                    performIO $ BatchCmd.setDefaultValue workspace (InPortRef nodeId inPort) (DefaultValue.Constant $ DefaultValue.StringValue $ Text.unpack val)
            UICmd.register parent widget handlers
        ValueType.Bool -> do
            let label = show inPort
                value = fromMaybe True $ def ^? DefaultValue._Constant . DefaultValue._BoolValue
                widget = Toggle.create (Vector2 200 20) (Text.pack $ show inPort) value
                handlers = onValueChanged $ \val _ -> do
                    workspace <- use Global.workspace
                    performIO $ BatchCmd.setDefaultValue workspace (InPortRef nodeId inPort) (DefaultValue.Constant $ DefaultValue.BoolValue val)
            UICmd.register parent widget handlers
        ValueType.Other -> do
            let widget = Label.create (Text.pack $ show inPort <> " :: " <> (show $ port ^. Port.valueType) )
            UICmd.register parent widget mempty

updateNodeValue :: NodeId -> Int -> Command State ()
updateNodeValue id val = inRegistry $ do
    widgetId <- nodeIdToWidgetId id
    forM_ widgetId $ \widgetId -> UICmd.update_ widgetId $ Model.value .~ (Text.pack $ show val)
