{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Sandbox where

import           Utils.PreludePlus
import           Utils.Vector

import           JS.Config

import           Object.Widget
import           Object.Node
import           Object.UITypes

import           Event.Mouse    hiding      (Event, WithObjects)
import qualified Event.Mouse    as Mouse
import qualified Event.Window   as Window
import           Event.Event
import           Event.WithObjects

import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.State.Global       as Global
import qualified Reactive.Plugins.Core.Action.State.Sandbox      as Sandbox
import           Reactive.Plugins.Core.Action.State.UIRegistry   (WidgetMap)
import qualified Reactive.Plugins.Core.Action.State.UIRegistry   as UIRegistry

import qualified Object.Widget.Button                            as Button
import qualified Object.Widget.Slider                            as Slider
import qualified Object.Widget.Toggle                            as Toggle
import qualified Object.Widget.Number                            as Number
import qualified Object.Widget.Chart                             as Chart

import           Object.Widget.Button                            (Button(..))
import           Object.Widget.Slider                            (Slider(..))
import           Object.Widget.Toggle                            (Toggle(..))
import           Object.Widget.Number                            (Number(..))
import           Object.Widget.Chart                             (Chart(..))

import           ThreeJS.Types
import qualified ThreeJS.Registry                                as JSRegistry
import qualified ThreeJS.Widget.Button                           as UIButton
import qualified ThreeJS.Widget.Slider                           as UISlider
import qualified ThreeJS.Widget.Toggle                           as UIToggle
import qualified ThreeJS.Widget.Number                           as UINumber
import qualified ThreeJS.Scene                                   as Scene
import qualified Dimple.Render                                   as UIChart
import qualified Control.Monad.State                             as MState
import           Object.Widget.Scene (sceneInterfaceId, sceneGraphId)

import qualified BatchConnector.Commands as BatchCmd

data Action = InitApp
            | ApplyUpdates  { _actions  :: [WidgetUIUpdate] }
            | WidgetClicked { _buttonId :: WidgetId }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "SandboxAction"

toAction (Window (Window.Event Window.Resized width height)) _ = Just $ InitApp
toAction (Mouse (Mouse.Event Mouse.Clicked _ LeftButton _ (Just (EventWidget bid _ _)))) state = Just $ WidgetClicked bid
toAction _ _        = Nothing

sandboxScene = Scene.scene

addWidget b = do
    widget <- JSRegistry.build b
    JSRegistry.register b widget
    sandboxScene `add` widget

addChart b = do
    UIChart.displayChart b

sampleHandler pos st = (st, putStrLn "Hello Handler")


instance ActionStateUpdater Action where
    execSt InitApp oldState = ActionUI  newAction newState
            where
            wasInited             = oldState ^. Global.sandbox . Sandbox.button  /= 0
            newState              = if wasInited || not widgetSandboxEnabled then oldState
                                                 else oldState & Global.sandbox    . Sandbox.button  .~ (objectId button)
                                                               & Global.sandbox    . Sandbox.slider  .~ (objectId slider)
                                                               & Global.sandbox    . Sandbox.slider2 .~ (objectId slider2)
                                                               & Global.sandbox    . Sandbox.slider3 .~ (objectId slider3)
                                                               & Global.sandbox    . Sandbox.slider4 .~ (objectId slider4)
                                                               & Global.sandbox    . Sandbox.toggle  .~ (objectId toggle)
                                                               & Global.sandbox    . Sandbox.chart   .~ (objectId chart)
                                                               & Global.sandbox    . Sandbox.number  .~ (objectId number)
                                                               & Global.uiRegistry                   .~ newRegistry

            oldRegistry     = oldState ^. Global.uiRegistry
            registerWidgets :: UIRegistry.UIState (Button.Button, Slider.Slider Int, Slider.Slider Double, Slider.Slider Double, Slider.Slider Double, Toggle.Toggle, Chart.Chart, Number.Number Int) Global.State
            registerWidgets = do
                button  <- UIRegistry.registerM sceneGraphId (Button 0 "Run!" Button.Normal (Vector2 100 100) (Vector2 100 50)) def
                UIRegistry.uiAction $ addWidget button

                slider  <- UIRegistry.registerM sceneGraphId (Slider 0 (Vector2 100 200) (Vector2 200  25) "Cutoff"    100      20000        0.1 :: Slider Int) (def & UIRegistry.click .~ [sampleHandler])
                UIRegistry.uiAction $ addWidget slider

                slider2 <- UIRegistry.registerM sceneGraphId (Slider 0 (Vector2 100 230) (Vector2 200  25) "Resonance"   0.0      100.0      0.3 :: Slider Double) def
                UIRegistry.uiAction $ addWidget slider2

                slider3 <- UIRegistry.registerM sceneGraphId (Slider 0 (Vector2 100 260) (Vector2 200  25) "Noise"       0.0        1.0      0.1 :: Slider Double) def
                UIRegistry.uiAction $ addWidget slider3

                slider4 <- UIRegistry.registerM sceneGraphId (Slider 0 (Vector2 100 290) (Vector2 200  25) "Gamma"       0.0000001  0.000001 0.9 :: Slider Double) def
                UIRegistry.uiAction $ addWidget slider4

                toggle  <- UIRegistry.registerM sceneGraphId (Toggle 0 (Vector2 100 320) (Vector2 200  25) "Inverse" False) def
                UIRegistry.uiAction $ addWidget toggle

                chart   <- UIRegistry.registerM sceneGraphId (Chart  0 (Vector2 100 380) (Vector2 300 200) Chart.Bar "Brand" Chart.Category "Unit Sales" Chart.Linear) def
                UIRegistry.uiAction $ addChart chart

                number  <- UIRegistry.registerM sceneGraphId (Number 0 (Vector2 100 170) (Vector2 200  25) "Count" 4096) def
                UIRegistry.uiAction $ addWidget number

                return (button, slider, slider2, slider3, slider4, toggle, chart, number)

            ((button, slider, slider2, slider3, slider4, toggle, chart, number), (newRegistry, actions)) = MState.runState registerWidgets (oldRegistry, [])

            newAction             = if wasInited || not widgetSandboxEnabled then ApplyUpdates [] else ApplyUpdates actions
    execSt (WidgetClicked bid) oldState = ActionUI  newAction newState where
        oldRegistry           = oldState ^. Global.uiRegistry
        widget                = UIRegistry.lookup bid oldRegistry
        newRegistry           = oldRegistry -- tu mozna np. zrobis UIRegistry.update, etc.
        newState              = oldState & Global.uiRegistry .~ newRegistry
        wasHelloButtonClicked = bid == oldState ^. Global.sandbox . Sandbox.button
        newAction             = if wasHelloButtonClicked then ApplyUpdates [Just $ putStrLn "HelloWorld", Just BatchCmd.runMain]
                                                            -- tu tablica Maybe (IO ()), np aktualizacja stanu widgeta
                                                         else ApplyUpdates []

instance ActionUIUpdater Action where
    updateUI (WithState (ApplyUpdates actions) state) = sequence_ $ catMaybes actions
