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
import           Reactive.State.Global         (State)
import qualified Reactive.State.Graph          as Graph
import           Reactive.State.UIRegistry     (sceneGraphId, addHandler)
import qualified Reactive.State.UIRegistry     as UIRegistry
import           Reactive.Commands.EnterNode   (enterNode)
import           Reactive.Commands.Graph       (focusNode, updatePortAngles)
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
import           UI.Handlers.Generic (triggerValueChanged, ValueChangedHandler(..))

import           Empire.API.Data.Node (Node)
import qualified Empire.API.Data.Node as Node
import           Empire.API.Data.Port (Port)
import qualified Empire.API.Data.Port as Port
import           Empire.API.Data.Connection (AnyPortRef(..), toAnyPortRef)
import           Debug.Trace (trace)

addNode :: Node -> Command State ()
addNode node = do
    unrenderPending node
    zoom Global.graph $ modify (Graph.addNode node)
    zoom Global.uiRegistry $ registerNode node
    updatePortAngles

colorVT _ = 1

registerNode :: Node -> Command UIRegistry.State ()
registerNode node = do
    let nodeModel = Model.node node
    nodeWidget <- UICmd.register sceneGraphId nodeModel (nodeHandlers node)

    forM_ (Map.toList $ node ^. Node.ports) $ \(portId, port) -> do
        let portRef    = toAnyPortRef (node ^. Node.nodeId) portId
            portWidget = PortModel.Port portRef def (colorVT $ port ^. Port.valueType)
        UICmd.register_ nodeWidget portWidget def

        -- createPortControl nodeWidget port portRef

    registerOutputPorts nodeWidget node
    focusNode nodeWidget

-- createPortControl :: WidgetId -> Port -> InPortRef -> Command UIRegistry.State (Maybe WidgetId)
-- createPortControl parent port portRef = return () -- TODO: expanding node
-- do
--     let pid = port ^. portId
--     case port ^. portValueType of
--         VTFloat -> do
--             let sliderWidget = (ContinuousNumber (Vector2 10 (95 + (fromIntegral $ portIdToNum pid) * 25)) (Vector2 180 20)
--                                        (Text.pack $ "float " <> show pid) 42.42 True Nothing)
--             id <- UICmd.register parent sliderWidget (numberDoubleHandlers portRef)
--             return $ Just id
--         VTNumeric -> do
--             let sliderWidget = (ContinuousNumber (Vector2 10 (95 + (fromIntegral $ portIdToNum pid) * 25)) (Vector2 180 20)
--                                        (Text.pack $ "float " <> show pid) 42.42 True Nothing)
--             id <- UICmd.register parent sliderWidget (numberDoubleHandlers portRef)
--             return $ Just id
--         VTInt -> do
--             let sliderWidget = (DiscreteNumber (Vector2 10 (95 + (fromIntegral $ portIdToNum pid) * 25)) (Vector2 180 20)
--                                           (Text.pack $ "int " <> show pid) 42 True Nothing)
--             id <- UICmd.register parent sliderWidget (numberIntHandlers portRef)
--             return $ Just id
--         otherwise -> do
--             performIO $ putStrLn $ "No widget for this type " <> (show $ port ^. portValueType)
--             return Nothing
--
nodeHandlers :: Node -> HTMap
nodeHandlers node = addHandler (UINode.RemoveNodeHandler removeSelectedNodes)
                  $ addHandler (UINode.FocusNodeHandler $ \id -> zoom Global.uiRegistry (focusNode id))
                  $ mempty
--
-- numberHandleDoubleValueChanged :: PortRef -> Double -> WidgetId -> Command Global.State ()
-- numberHandleDoubleValueChanged portRef value widgetId = do
--     workspace <- use Global.workspace
--     performIO $ BatchCmd.setValue workspace portRef $ show value
--
-- numberHandleIntValueChanged :: PortRef -> Int -> WidgetId -> Command Global.State ()
-- numberHandleIntValueChanged portRef value widgetId = do
--     workspace <- use Global.workspace
--     performIO $ BatchCmd.setValue workspace portRef $ show value
--
-- numberDoubleHandlers :: PortRef -> HTMap
-- numberDoubleHandlers portRef = addHandler (ValueChangedHandler $ numberHandleDoubleValueChanged portRef)
--                              $ mempty
--
-- numberIntHandlers :: PortRef -> HTMap
-- numberIntHandlers portRef = addHandler (ValueChangedHandler $ numberHandleIntValueChanged portRef)
--                           $ mempty

registerSinglePort :: WidgetId -> Node -> Port -> Command UIRegistry.State ()
registerSinglePort nodeWidgetId node port = do
    let portWidget = PortModel.Port (toAnyPortRef (node ^. Node.nodeId) (port ^. Port.portId)) def (colorVT $ port ^. Port.valueType)
    UICmd.register_ nodeWidgetId portWidget def

registerOutputPorts :: WidgetId -> Node -> Command UIRegistry.State ()
registerOutputPorts nodeWidgetId node = mapM_ (registerSinglePort nodeWidgetId node) ports where
    ports = node ^. Node.ports
