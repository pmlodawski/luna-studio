{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Commands.AddNode (addNode) where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.Object
import           Object.Node
import           Object.Port
import           Object.Widget
import           Object.UITypes       (WidgetId)
import qualified Object.Widget.Node   as WNode
import qualified Object.Widget.Port   as WPort
import           Object.Widget.Slider (Slider(..))

import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.State.Global         as Global
import           Reactive.Plugins.Core.Action.State.Global         (State)
import qualified Reactive.Plugins.Core.Action.State.Graph          as Graph
import           Reactive.Plugins.Core.Action.State.UIRegistry     (sceneGraphId)
import qualified Reactive.Plugins.Core.Action.State.UIRegistry     as UIRegistry
import           Reactive.Plugins.Core.Action.State.UIRegistry     (UIState)
import           Reactive.Plugins.Core.Action.Commands.EnterNode   (enterNode)
import           Reactive.Plugins.Core.Action.Commands.RemoveNode  (removeNode)
import           Reactive.Plugins.Core.Action.Commands.Command     (Command, performIO)

import qualified Control.Monad.State   as MState
import qualified JS.NodeGraph          as UI
import qualified ThreeJS.Registry      as JSRegistry
import qualified ThreeJS.Widget.Node   as UINode
import qualified ThreeJS.Widget.Slider as UISlider
import           ThreeJS.Types         (add)

addNode :: Node -> Command State ()
addNode node = do
    graph    <- use Global.graph
    registry <- use Global.uiRegistry
    let newGraph              = Graph.addNode node graph
        (newRegistry, action) = registerNode node registry
    performIO action
    Global.graph      .= newGraph
    Global.uiRegistry .= newRegistry

registerNode :: Node -> UIRegistry.State State -> (UIRegistry.State State, IO ())
registerNode node oldRegistry = flip MState.execState (oldRegistry, return ()) $ do
    file <- UIRegistry.registerM sceneGraphId
                                 (WNode.Node (node ^. nodeId) [] [])
                                 (nodeHandlers node)

    UIRegistry.uiAction $ createNodeOnUI node file

    registerPorts (file ^. objectId) InputPort node
    registerPorts (file ^. objectId) OutputPort node

    let rootId     = file ^. objectId
        nodeWidget = file ^. widget
        sliders    = [ Slider (Vector2 10   75) (Vector2 180 20) "Cutoff"    100.0 25000.0 0.4
                     , Slider (Vector2 10  100) (Vector2 180 20) "Resonance" 0.0   1.0     0.2
                     , Slider (Vector2 10  125) (Vector2 180 20) "Amount"    0.0   1.0     0.4
                     , Slider (Vector2 10  150) (Vector2 180 20) "Gain"      0.0   1.0     0.4
                     ]
    sliderIds <- sequence $ addSliderToNode rootId <$> sliders
    UIRegistry.updateM rootId (nodeWidget & WNode.controls .~ sliderIds)
    return file

nodeHandlers :: Node -> UIHandlers State
nodeHandlers node = def & dblClick   .~ [const $ enterNode node]
                        & keyDown    .~ [removeNode node]

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


registerPorts :: WidgetId -> PortType -> Node -> UIRegistry.UIState () b
registerPorts nodeWidgetId portType node = do
    let registerPort p = do file <- UIRegistry.registerM nodeWidgetId (WPort.Port $ PortRef (node ^. nodeId) portType (p ^. portId)) def
                            return (file, p)
    let inPorts = getPorts portType node
    ins  <- mapM registerPort inPorts

    forM_ ins $ \(w, p) -> UIRegistry.uiAction $ (addPort portType) (node ^. nodeId) (w ^. objectId) (p ^. portId) (p ^. angle)

    return ()


addPort :: PortType -> NodeId -> WidgetId -> PortId -> Double -> IO ()
addPort  InputPort = UI.addInputPort
addPort OutputPort = UI.addOutputPort
