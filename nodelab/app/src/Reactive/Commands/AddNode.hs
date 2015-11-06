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
import           Reactive.State.UIRegistry     (sceneGraphId)
import qualified Reactive.State.UIRegistry     as UIRegistry
import           Reactive.Commands.EnterNode   (enterNode)
import           Reactive.Commands.RemoveNode  (removeSelectedNodes)
import           Reactive.Commands.Command     (Command, performIO)
import           Reactive.Commands.PendingNode (unrenderPending)
-- import           Reactive.Commands.Selection   (handleSelection)
import qualified Reactive.Commands.UIRegistry as UICmd

import qualified BatchConnector.Commands as BatchCmd
import qualified JS.NodeGraph          as UI

import qualified UI.Widget.Slider as UISlider
import qualified UI.Widget.Node   as UINode
import qualified UI.Registry      as UIR
import qualified UI.Widget        as UIT
import qualified UI.Scene

addNode :: Node -> Command State ()
addNode node = do
    unrenderPending node
    zoom Global.graph $ modify (Graph.addNode node)
    zoom Global.uiRegistry $ registerNode node

registerNode :: Node -> Command (UIRegistry.State State) ()
registerNode node = do
    let nodeWidget = Model.Node (node ^. nodeId) [] [] (node ^. nodePos) (node ^. expression) "()" False False False
    id <- UICmd.register sceneGraphId
                           nodeWidget
                           (nodeHandlers node)

    registerPorts id InputPort node
    registerPorts id OutputPort node

    let rootId      = id
        inPorts     = getPorts InputPort node
        nat         = [0..] :: [Int]
        portsWithId = zip nat inPorts
        filteredPortsWithId = filter (\(id, port) -> port ^. portValueType == VTFloat || port ^. portValueType == VTNumeric) portsWithId
        slidersDouble    = uncurry makeSliderFromPortDouble <$> filteredPortsWithId
    sliderIds <- sequence $ addSliderToNode rootId (node ^. nodeId) <$> slidersDouble
    UIRegistry.updateM rootId (nodeWidget & Model.controls .~ sliderIds)

makeSliderFromPortDouble :: Int -> Port -> Slider Double
makeSliderFromPortDouble i port = Slider (Vector2 10 (95 + (fromIntegral i) * 25)) (Vector2 180 20)
                                         (Text.pack $ "param " <> show i) 0.0 1.0 0.2 (PortNum i) True

nodeHandlers :: Node -> UIHandlers State
nodeHandlers node = def

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
sliderHandlers nodeId = def
 -- & dragEnd  .~ [handleValueChanged nodeId]
 -- & dblClick .~ [\_ -> handleValueChanged nodeId]

addSliderToNode :: IsSlider a => WidgetId -> NodeId -> Slider a -> Command (UIRegistry.State Global.State) WidgetId
addSliderToNode widgetId nodeId slider = do
    sliderWidget <- UIRegistry.registerM widgetId slider (sliderHandlers nodeId)
    performIO $ addWidgetToNode widgetId sliderWidget
    return $ sliderWidget ^. objectId

addWidgetToNode :: IsSlider a => WidgetId -> WidgetFile b (Slider a) -> IO ()
addWidgetToNode nodeId newWidget = do
    let sliderId = newWidget ^. objectId
    slider <- UISlider.createSlider sliderId (newWidget ^. widget)
    node   <- UIR.lookup nodeId :: IO UINode.Node
    UIR.register sliderId slider
    UIT.add      slider   node

registerSinglePort :: WidgetId -> Node -> PortType -> Port -> Command (UIRegistry.State b) ()
registerSinglePort nodeWidgetId node portType port = do
    let portWidget = PortModel.Port (PortRef (node ^. nodeId) portType (port ^. portId)) def (colorVT $ port ^. portValueType)
    void $ UICmd.register nodeWidgetId portWidget def

registerPorts :: WidgetId -> PortType -> Node -> Command (UIRegistry.State b) ()
registerPorts nodeWidgetId portType node = mapM_ (registerSinglePort nodeWidgetId node portType) ports where
    ports = getPorts portType node
