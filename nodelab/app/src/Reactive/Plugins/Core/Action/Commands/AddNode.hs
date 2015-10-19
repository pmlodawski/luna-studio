{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Commands.AddNode (addNode) where

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
import qualified Object.Widget.Node    as WNode
import qualified Object.Widget.Port    as WPort
import           Object.Widget.Slider (Slider(..),IsSlider(..))
import qualified Object.Widget.Slider  as Slider

import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.State.Global         as Global
import           Reactive.Plugins.Core.Action.State.Global         (State)
import qualified Reactive.Plugins.Core.Action.State.Graph          as Graph
import           Reactive.Plugins.Core.Action.State.UIRegistry     (sceneGraphId)
import qualified Reactive.Plugins.Core.Action.State.UIRegistry     as UIRegistry
import           Reactive.Plugins.Core.Action.Commands.EnterNode   (enterNode)
import           Reactive.Plugins.Core.Action.Commands.RemoveNode  (removeNode)
import           Reactive.Plugins.Core.Action.Commands.Command     (Command, performIO)
import           Reactive.Plugins.Core.Action.Commands.PendingNode (unrenderPending)
import           Reactive.Plugins.Core.Action.Commands.Selection   (handleSelection)

import qualified BatchConnector.Commands as BatchCmd
import qualified JS.NodeGraph          as UI

import qualified ThreeJS.Registry      as JSRegistry
import qualified ThreeJS.Widget.Node   as UINode
import qualified ThreeJS.Widget.Slider as UISlider
import           ThreeJS.Types         (add)

addNode :: Node -> Command State ()
addNode node = do
    unrenderPending node
    zoom Global.graph $ modify (Graph.addNode node)
    zoom Global.uiRegistry $ registerNode node

registerNode :: Node -> Command (UIRegistry.State State) ()
registerNode node = do
    file <- UIRegistry.registerM sceneGraphId
                                 (WNode.Node (node ^. nodeId) [] [])
                                 (nodeHandlers node)

    performIO $ createNodeOnUI node file

    registerPorts (file ^. objectId) InputPort node
    registerPorts (file ^. objectId) OutputPort node

    let rootId      = file ^. objectId
        nodeWidget  = file ^. widget
        inPorts     = getPorts InputPort node
        nat         = [0..] :: [Int]
        portsWithId = zip nat inPorts
        filteredPortsWithId = filter (\(id, port) -> port ^. portValueType == VTFloat || port ^. portValueType == VTNumeric) portsWithId
        slidersDouble    = uncurry makeSliderFromPortDouble <$> filteredPortsWithId
    sliderIds <- sequence $ addSliderToNode rootId (node ^. nodeId) <$> slidersDouble
    UIRegistry.updateM rootId (nodeWidget & WNode.controls .~ sliderIds)

makeSliderFromPortDouble :: Int -> Port -> Slider Double
makeSliderFromPortDouble i port = Slider (Vector2 10 (95 + (fromIntegral i) * 25)) (Vector2 180 20)
                                         (Text.pack $ "param " <> show i) 0.0 1.0 0.2 (PortNum i) True

nodeHandlers :: Node -> UIHandlers State
nodeHandlers node = def & dblClick     .~ [const $ enterNode node]
                        & keyDown      .~ [removeNode node]
                        & mousePressed .~ [\keymods _ _ _ -> handleSelection node keymods]

retriveSliderDouble :: WidgetId -> Command (UIRegistry.State Global.State) (Maybe (WidgetFile Global.State (Slider Double)))
retriveSliderDouble wid = UIRegistry.lookupTypedM wid

handleValueChanged :: NodeId -> WidgetId -> Command Global.State ()
handleValueChanged nodeId wid = do
    sliderFileDoubleMay <- zoom Global.uiRegistry $ retriveSliderDouble wid
    case sliderFileDoubleMay of
        Nothing         -> return ()
        Just sliderFileDouble -> do
            let val     = value $ sliderFileDouble ^. widget
                portId  = sliderFileDouble ^. widget . Slider.sliderPortId
                portRef = PortRef nodeId InputPort portId
            workspace <- use Global.workspace
            performIO $ do
                BatchCmd.setValue workspace portRef $ double2Float val

sliderHandlers :: NodeId -> UIHandlers State
sliderHandlers nodeId = def & dragEnd  .~ [handleValueChanged nodeId]
                            & dblClick .~ [\_ -> handleValueChanged nodeId]

addSliderToNode :: IsSlider a => WidgetId -> NodeId -> Slider a -> Command (UIRegistry.State Global.State) WidgetId
addSliderToNode widgetId nodeId slider = do
    sliderWidget <- UIRegistry.registerM widgetId slider (sliderHandlers nodeId)
    performIO $ addWidgetToNode widgetId sliderWidget
    return $ sliderWidget ^. objectId

addWidgetToNode :: IsSlider a => WidgetId -> WidgetFile b (Slider a) -> IO ()
addWidgetToNode nodeId newWidget = do
    node   <- JSRegistry.lookup nodeId :: IO UINode.Node
    widget <- JSRegistry.build (newWidget ^. objectId) (newWidget ^. widget)
    JSRegistry.register (newWidget ^. objectId) widget
    node `add` widget

createNodeOnUI :: Node -> WidgetFile s WNode.Node -> IO ()
createNodeOnUI node file = do
    let pos   = node ^. nodePos
        ident = node ^. nodeId
        expr  = node ^. expression
    UI.createNodeAt ident pos expr (file ^. objectId)

registerSinglePort :: WidgetId -> Node -> PortType -> Port -> Command (UIRegistry.State b) ()
registerSinglePort nodeWidgetId node portType port = do
    let portWidget = (WPort.Port $ PortRef (node ^. nodeId) portType (port ^. portId))
    widgetFile <- UIRegistry.registerM nodeWidgetId portWidget def
    performIO $ addPort portType
                        (node ^. nodeId)
                        (widgetFile ^. objectId)
                        (port ^. portId)
                        (colorVT $ port ^. portValueType)
                        (port ^. angle)

registerPorts :: WidgetId -> PortType -> Node -> Command (UIRegistry.State b) ()
registerPorts nodeWidgetId portType node = mapM_ (registerSinglePort nodeWidgetId node portType) ports where
    ports = getPorts portType node


addPort :: PortType -> NodeId -> WidgetId -> PortId -> ColorNum -> Double -> IO ()
addPort  InputPort = UI.addInputPort
addPort OutputPort = UI.addOutputPort
