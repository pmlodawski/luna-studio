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
import           Object.Widget.Slider (Slider(..),IsSlider(..))
import qualified Object.Widget.Slider  as Slider

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

import qualified UI.Widget.Slider as UISlider
import qualified UI.Widget.Node   as UINode
import qualified UI.Registry      as UIR
import qualified UI.Widget        as UIT
import qualified UI.Scene
import qualified Data.HMap.Lazy as HMap
import           Data.HMap.Lazy (HTMap)

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
            let sliderWidget = (Slider (Vector2 10 (95 + (fromIntegral $ portIdToNum pid) * 25)) (Vector2 180 20)
                                       (Text.pack $ "float " <> show pid) 0.0 1.0 0.2 True) :: Slider Double
            id <- UICmd.register parent sliderWidget (sliderDoubleHandlers portRef)
            return $ Just id
        VTNumeric -> do
            let sliderWidget = (Slider (Vector2 10 (95 + (fromIntegral $ portIdToNum pid) * 25)) (Vector2 180 20)
                                       (Text.pack $ "float " <> show pid) 0.0 1.0 0.2 True) :: Slider Double
            id <- UICmd.register parent sliderWidget (sliderDoubleHandlers portRef)
            return $ Just id
        VTInt -> do
            let sliderWidget = (Slider (Vector2 10 (95 + (fromIntegral $ portIdToNum pid) * 25)) (Vector2 180 20)
                                       (Text.pack $ "int " <> show pid) (-42) 42 0.2 True) :: Slider Int
            id <- UICmd.register parent sliderWidget (sliderIntHandlers portRef)
            return $ Just id
        otherwise -> do
            performIO $ putStrLn $ "No widget for  this type " <> (show $ port ^. portValueType)
            return Nothing

nodeHandlers :: Node -> HTMap
nodeHandlers node = addHandler (UINode.RemoveNodeHandler removeSelectedNodes)
                  $ addHandler (UINode.FocusNodeHandler $ \id -> zoom Global.uiRegistry (focusNode id))
                  $ mempty

sliderHandleDoubleValueChanged :: PortRef -> Double -> WidgetId -> Command Global.State ()
sliderHandleDoubleValueChanged portRef value widgetId = do
    workspace <- use Global.workspace
    performIO $ do
        BatchCmd.setValue workspace portRef $ double2Float value

sliderHandleIntValueChanged :: PortRef -> Double -> WidgetId -> Command Global.State ()
sliderHandleIntValueChanged portRef value widgetId = do
    workspace <- use Global.workspace
    performIO $ do
        BatchCmd.setValue workspace portRef $ double2Float $ 100.0 * value

sliderDoubleHandlers :: PortRef -> HTMap
sliderDoubleHandlers portRef = addHandler (UISlider.ValueChangedHandler $ sliderHandleDoubleValueChanged portRef)
                               mempty

sliderIntHandlers :: PortRef -> HTMap
sliderIntHandlers portRef = addHandler (UISlider.ValueChangedHandler $ sliderHandleIntValueChanged portRef)
                            mempty

registerSinglePort :: WidgetId -> Node -> PortType -> Port -> Command UIRegistry.State ()
registerSinglePort nodeWidgetId node portType port = do
    let portWidget = PortModel.Port (PortRef (node ^. nodeId) portType (port ^. portId)) def (colorVT $ port ^. portValueType)
    void $ UICmd.register nodeWidgetId portWidget def

registerOutputPorts :: WidgetId -> Node -> Command UIRegistry.State ()
registerOutputPorts nodeWidgetId node = mapM_ (registerSinglePort nodeWidgetId node OutputPort) ports where
    ports = getPorts OutputPort node
