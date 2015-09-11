{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Executors.AddNode (addNode) where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.Object
import           Object.Node
import           Object.Port
import           Object.Widget
import           Object.UITypes                                (WidgetId)
import qualified Object.Widget.Node                            as WNode
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.State.Global     as Global
import           Reactive.Plugins.Core.Action.State.Global     (State)
import qualified Reactive.Plugins.Core.Action.State.Graph      as Graph
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry
import           Reactive.Plugins.Core.Action.State.UIRegistry (UIState)
import qualified ThreeJS.Registry                              as JSRegistry
import qualified Control.Monad.State                           as MState
import           Object.Widget.Slider                          (Slider(..))
import           Object.Widget.Scene                           (sceneGraphId)
import qualified JS.NodeGraph                                  as UI
import qualified ThreeJS.Widget.Node                           as UINode
import qualified ThreeJS.Widget.Slider                         as UISlider
import           ThreeJS.Types                                 (add)

addNode :: Node -> Global.State -> (State, IO ())
addNode node oldState = (newState, sequence_ $ reverse $ catMaybes actions) where
    newState                = oldState & Global.iteration  +~ 1
                                       & Global.graph      .~ newGraph
                                       & Global.uiRegistry .~ newRegistry
    newGraph                = Graph.addNode node (oldState ^. Global.graph)
    (newRegistry, actions)  = registerNode  node (oldState ^. Global.uiRegistry)

registerNode :: Node -> UIRegistry.State a -> (UIRegistry.State a, [Maybe (IO ())])
registerNode node oldRegistry = flip MState.execState (oldRegistry, []) $ do
    file <- UIRegistry.registerM sceneGraphId (WNode.Node (node ^. nodeId) []) def
    UIRegistry.uiAction $ createNodeOnUI node file
    let rootId     = file ^. objectId
        nodeWidget = file ^. widget
        sliders    = [ Slider (Vector2 10   75) (Vector2 180 25) "Cutoff"    100.0 25000.0 0.4
                     , Slider (Vector2 10  105) (Vector2 180 25) "Resonance" 0.0   1.0     0.2
                     , Slider (Vector2 10  135) (Vector2 180 25) "Amount"    0.0   1.0     0.4
                     , Slider (Vector2 10  165) (Vector2 180 25) "Gain"      0.0   1.0     0.4
                     ]
    sliderIds <- sequence $ addSliderToNode rootId <$> sliders
    UIRegistry.updateM rootId (nodeWidget & WNode.controls .~ sliderIds)
    return file

addSliderToNode :: WidgetId -> Slider Double -> UIState WidgetId b
addSliderToNode nodeId slider = do
    sliderWidget <- UIRegistry.registerM nodeId slider def
    UIRegistry.uiAction $ addWidgetToNode nodeId sliderWidget
    return $ sliderWidget ^. objectId

addWidgetToNode :: WidgetId -> WidgetFile a (Slider Double) -> IO ()
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
    addPorts  InputPort node
    addPorts OutputPort node

addPorts :: PortType -> Node -> IO ()
addPorts portType node = mapM_ (addPortWith (node ^. nodeId) $ addPort portType) $ getPorts portType node

addPortWith :: NodeId -> (NodeId -> PortId -> Double -> IO ()) -> Port -> IO ()
addPortWith nodeId addPortFun port = addPortFun nodeId (port ^. portId) (port ^. angle)

addPort :: PortType -> NodeId -> PortId -> Double -> IO ()
addPort  InputPort = UI.addInputPort
addPort OutputPort = UI.addOutputPort
