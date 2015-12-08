{-# LANGUAGE OverloadedStrings #-}

module Reactive.Commands.AddNode (addNode) where

import           Utils.PreludePlus
import           Utils.Vector

import qualified Data.Text.Lazy        as Text
import           Control.Monad.State   hiding (State)
import           GHC.Float             (double2Float)

import           Object.Object
import           Object.Node
import           Object.Port
import           Object.Widget
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

addNode :: Node -> Command State ()
addNode node = do
    unrenderPending node
    zoom Global.graph $ modify (Graph.addNode node)
    zoom Global.uiRegistry $ registerNode node
    updatePortAngles

registerNode :: Node -> Command UIRegistry.State ()
registerNode node = do
    let nodeModel = Model.node node
    nodeWidget <- UICmd.register sceneGraphId nodeModel (nodeHandlers node)

    forM_ (getPorts InputPort node) $ \port -> do
        let portRef    = PortRef (node ^. nodeId) InputPort (port ^. portId)
            portWidget = PortModel.Port portRef def (colorVT $ port ^. portValueType)
        portW <- UICmd.register nodeWidget portWidget def
        createPortControl nodeWidget port portRef

    registerOutputPorts nodeWidget node
    focusNode nodeWidget

createPortControl :: WidgetId -> Port -> PortRef -> Command UIRegistry.State (Maybe WidgetId)
createPortControl parent port portRef = do
    let pid = port ^. portId
    case port ^. portValueType of
        VTFloat -> do
            let sliderWidget = (ContinuousNumber (Vector2 10 (95 + (fromIntegral $ portIdToNum pid) * 25)) (Vector2 180 20)
                                       (Text.pack $ "float " <> show pid) 42.42 True Nothing)
            id <- UICmd.register parent sliderWidget (numberDoubleHandlers portRef)
            return $ Just id
        VTNumeric -> do
            let sliderWidget = (ContinuousNumber (Vector2 10 (95 + (fromIntegral $ portIdToNum pid) * 25)) (Vector2 180 20)
                                       (Text.pack $ "float " <> show pid) 42.42 True Nothing)
            id <- UICmd.register parent sliderWidget (numberDoubleHandlers portRef)
            return $ Just id
        VTInt -> do
            let sliderWidget = (DiscreteNumber (Vector2 10 (95 + (fromIntegral $ portIdToNum pid) * 25)) (Vector2 180 20)
                                          (Text.pack $ "int " <> show pid) 42 True Nothing)
            id <- UICmd.register parent sliderWidget (numberIntHandlers portRef)
            return $ Just id
        otherwise -> do
            performIO $ putStrLn $ "No widget for  this type " <> (show $ port ^. portValueType)
            return Nothing

nodeHandlers :: Node -> HTMap
nodeHandlers node = addHandler (UINode.RemoveNodeHandler removeSelectedNodes)
                  $ addHandler (UINode.FocusNodeHandler $ \id -> zoom Global.uiRegistry (focusNode id))
                  $ mempty

numberHandleDoubleValueChanged :: PortRef -> Double -> WidgetId -> Command Global.State ()
numberHandleDoubleValueChanged portRef value widgetId = do
    workspace <- use Global.workspace
    performIO $ do
        BatchCmd.setValue workspace portRef $ show value

numberHandleIntValueChanged :: PortRef -> Int -> WidgetId -> Command Global.State ()
numberHandleIntValueChanged portRef value widgetId = do
    workspace <- use Global.workspace
    performIO $ do
        BatchCmd.setValue workspace portRef $ show value

numberDoubleHandlers :: PortRef -> HTMap
numberDoubleHandlers portRef = addHandler (ValueChangedHandler $ numberHandleDoubleValueChanged portRef)
                             $ mempty

numberIntHandlers :: PortRef -> HTMap
numberIntHandlers portRef = addHandler (ValueChangedHandler $ numberHandleIntValueChanged portRef)
                          $ mempty

registerSinglePort :: WidgetId -> Node -> PortType -> Port -> Command UIRegistry.State ()
registerSinglePort nodeWidgetId node portType port = do
    let portWidget = PortModel.Port (PortRef (node ^. nodeId) portType (port ^. portId)) def (colorVT $ port ^. portValueType)
    void $ UICmd.register nodeWidgetId portWidget def

registerOutputPorts :: WidgetId -> Node -> Command UIRegistry.State ()
registerOutputPorts nodeWidgetId node = mapM_ (registerSinglePort nodeWidgetId node OutputPort) ports where
    ports = getPorts OutputPort node
