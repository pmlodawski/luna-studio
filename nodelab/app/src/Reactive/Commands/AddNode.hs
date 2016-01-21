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
import qualified Object.Widget.Port    as PortModel
import qualified Object.Widget.Group   as GroupModel
import           Object.Widget.Number.Discrete     (DiscreteNumber(..))
import qualified Object.Widget.Number.Discrete     as DiscreteNumber
import qualified UI.Handlers.Number.Discrete       as DiscreteNumber
import           Object.Widget.Number.Continuous   (ContinuousNumber(..))
import qualified UI.Handlers.Number.Continuous     as ContinuousNumber

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

import           Empire.API.Data.Node (Node, NodeId)
import qualified Empire.API.Data.Node as Node
import           Empire.API.Data.Port (Port(..), PortId(..))
import qualified Empire.API.Data.Port as Port
import qualified Empire.API.Data.DefaultValue as DefaultValue
import           Empire.API.Data.PortRef (AnyPortRef(..), toAnyPortRef, InPortRef(..))
import           Debug.Trace (trace)

addNode :: Node -> Command State ()
addNode node = do
    unrenderPending node
    zoom Global.graph $ modify (Graph.addNode node)
    connectedPorts <- uses (Global.graph . Graph.connectionsMap) Map.keys
    zoom Global.uiRegistry $ registerNode node connectedPorts
    updatePortAngles

colorVT _ = 11
colorPort (Port.InPortId Port.Self) = 12
colorPort _ = 11

registerNode :: Node -> [InPortRef] -> Command UIRegistry.State ()
registerNode node connectedPorts = do
    let nodeModel = Model.node node
    nodeWidget <- UICmd.register sceneGraphId nodeModel (nodeHandlers node)

    displayPorts nodeWidget node connectedPorts
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

displayPorts :: WidgetId -> Node -> [InPortRef] -> Command UIRegistry.State ()
displayPorts id node connectedPorts = do
    nodeId <- UICmd.get id Model.nodeId
    oldPorts <- nodePorts id
    mapM_ UICmd.removeWidget oldPorts

    (groupId:_) <- UICmd.children id
    portControls <- UICmd.children groupId
    mapM_ UICmd.removeWidget portControls

    let newPorts = makePorts node

    gr <- nodeExpandedGroup id
    forM_ newPorts $ \p -> UICmd.register id p def
    forM_ (node ^. Node.ports) $ \p -> makePortControl gr (node ^. Node.nodeId) connectedPorts p

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
        connectedPorts <- uses (Global.graph . Graph.connectionsMap) Map.keys
        inRegistry $ do
            displayPorts widgetId node connectedPorts
            UICmd.update widgetId $ Model.expression .~ (node ^. Node.expression)
        updatePortAngles
        updateNodeMeta nodeId $ node ^. Node.nodeMeta
        -- TODO: obsluzyc to ze moga zniknac porty i trzeba usunac polaczenia

nodeExpandedGroup :: WidgetId -> Command UIRegistry.State WidgetId
nodeExpandedGroup id = do
    children <- UICmd.children id
    let isPort id = (UIRegistry.lookupTypedM id :: UIRegistry.LookupFor GroupModel.Group) >>= return . isJust
    groups <- filterM isPort children
    return $ head groups

onValueChanged :: Typeable a => (a -> WidgetId -> Command Global.State ()) -> HTMap
onValueChanged h = addHandler (ValueChangedHandler h) mempty

makePortControl :: WidgetId -> NodeId -> [InPortRef] -> Port -> Command UIRegistry.State ()
makePortControl parent nodeId connectedPorts port = case port ^. Port.portId of
    OutPortId _ -> return ()
    InPortId inPort -> void $ UICmd.register parent widget handlers where
        label = show inPort
        value = fromMaybe 0 $ port ^? Port.defaultValue . _Just . DefaultValue._Constant . DefaultValue._IntValue
        widget' = DiscreteNumber.create (Vector2 200 20) (Text.pack $ show inPort) value
        widget  = widget' & DiscreteNumber.enabled .~ (not $ elem portRef connectedPorts)
        portRef = (InPortRef nodeId inPort)
        handlers = onValueChanged $ \val _ -> do
            workspace <- use Global.workspace
            performIO $ BatchCmd.setDefaultValue workspace portRef (DefaultValue.Constant $ DefaultValue.IntValue val)


updateNodeValue :: NodeId -> Int -> Command State ()
updateNodeValue id val = inRegistry $ do
    widgetId <- nodeIdToWidgetId id
    forM_ widgetId $ \widgetId -> UICmd.update_ widgetId $ Model.value .~ (Text.pack $ show val)
