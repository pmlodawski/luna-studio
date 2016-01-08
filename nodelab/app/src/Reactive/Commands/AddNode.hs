module Reactive.Commands.AddNode (addNode) where

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
import           Object.Widget.Number.Discrete (DiscreteNumber(..))
import qualified UI.Handlers.Number.Discrete as DiscreteNumber
import           Object.Widget.Number.Continuous  (ContinuousNumber(..))
import qualified UI.Handlers.Number.Continuous as ContinuousNumber

import qualified Reactive.State.Global         as Global
import           Reactive.State.Global         (State, inRegistry)
import qualified Reactive.State.Graph          as Graph
import           Reactive.State.UIRegistry     (sceneGraphId, addHandler)
import qualified Reactive.State.UIRegistry     as UIRegistry
import           Reactive.Commands.EnterNode   (enterNode)
import           Reactive.Commands.Graph       (focusNode, updatePortAngles, portDefaultAngle)
import           Reactive.Commands.RemoveNode  (removeSelectedNodes)
import           Reactive.Commands.Command     (Command, performIO)
import           Reactive.Commands.PendingNode (unrenderPending)
import qualified Reactive.Commands.UIRegistry as UICmd

import qualified BatchConnector.Commands as BatchCmd
import qualified JS.NodeGraph          as UI

import qualified UI.Widget.Node   as UINode
import qualified UI.Registry      as UIR
import qualified UI.Widget        as UIT
import qualified UI.Scene
import qualified Data.HMap.Lazy as HMap
import           Data.HMap.Lazy (HTMap)
import qualified Data.Map.Lazy as Map
import           Data.Map.Lazy (Map)
import           UI.Handlers.Generic (triggerValueChanged, ValueChangedHandler(..))

import           Empire.API.Data.Node (Node)
import qualified Empire.API.Data.Node as Node
import           Empire.API.Data.Port (Port)
import qualified Empire.API.Data.Port as Port
import           Empire.API.Data.PortRef (AnyPortRef(..), toAnyPortRef)
import           Debug.Trace (trace)

addNode :: Node -> Command State ()
addNode node = do
    unrenderPending node
    zoom Global.graph $ modify (Graph.addNode node)
    zoom Global.uiRegistry $ registerNode node
    updatePortAngles

colorVT _ = 11

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
    makePort port = PortModel.Port portRef angle (colorVT $ port ^. Port.valueType) where
        portRef = toAnyPortRef nodeId (port ^. Port.portId)
        angle   = portDefaultAngle ((length $ node ^. Node.ports) - 1) (port ^. Port.portId)


displayPorts :: WidgetId -> Node -> Command UIRegistry.State ()
displayPorts id node = do
    nodeId <- UICmd.get id Model.nodeId
    oldPorts <- nodePorts id
    mapM_ UICmd.removeWidget oldPorts

    let newPorts = makePorts node

    forM_ newPorts $ \p -> UICmd.register id p def

nodeHandlers :: Node -> HTMap
nodeHandlers node = addHandler (UINode.RemoveNodeHandler removeSelectedNodes)
                  $ addHandler (UINode.FocusNodeHandler $ \id -> zoom Global.uiRegistry (focusNode id))
                  $ mempty
